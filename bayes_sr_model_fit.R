#------------------------------------------------------------------------------#
#   Load data  
#------------------------------------------------------------------------------#

age = read.delim("data/AgeCompTab.txt")
esc = read.csv("data/EscTab.csv")
harv = read.csv("data/HarTab.csv")

#------------------------------------------------------------------------------#
#   Set beta prior for stock (uniform (uninformative) prior can be commented
#	out in model description (line 58)) 
#------------------------------------------------------------------------------#

SMAX <- mean(esc[,2]) # mean escapement over time series
#SMAX <- 21300 # lake habitat (photosynthetic rate) based estimate of lake capacity

bpmu = 1/SMAX # turn SMAX into beta prior
bptau = 1/(0.3^2)  # set CV on prior
 
#------------------------------------------------------------------------------#
#   Format data
#------------------------------------------------------------------------------#
Y = nrow(age)             # number of calendar years observed
a.min = 3                 # minimum age class in data set
a.max = 6                 # maximum age class in data set
A = a.max - a.min + 1     # number of age classes
nRyrs = Y + A - 1         # number of recruitment years (see model code for details)
years = age[,"year"]

# escapement: assume a 20% observation CV if directly observed, 40% otherwise

S.cv = ifelse(esc[,3] == 1, 0.5, 1)
#S.cv = ifelse(esc[,3] == 1, 1, 2)
S.obs = esc[,2]

# harvest: assume a 15% observation CV for years when catch was high and dominated by commercial fishery, 30% in years since

C.cv = ifelse(harv[,3] == 1, 0.15, 0.30)
#C.cv = ifelse(harv[,3] == 1, 0.3, 0.6)
C.obs = harv[,2] # choose column that matches assumption about catch (#2 is baseline, 4 is double mixed stock catch pre collapse)

# age composition
ESS = age$ESS
#ESS = age$ESS/2
X = age[,substr(names(age), 1, 1) == "X"]
X = t(apply(X, 1, function(x) x/sum(x)))
X = round(apply(X, 2, function(x) x * ESS))
colnames(X) = NULL
n = rowSums(X)  # n is slightly different than ESS because of rounding errors
x=X

#------------------------------------------------------------------------------#
#  Bayes model
#------------------------------------------------------------------------------#
modelFilename = "dep_mod.txt"
  cat("
model {
  # priors for SR portion
  lnalpha ~ dunif(0,3) 
  beta ~ dunif(0,10)
  #beta ~ dnorm(bpmu,bptau)
  tau.R ~ dgamma(0.01,0.01)  # white noise process error      
  phi ~ dunif(-0.99, 0.99)   # autocorrelation coefficient                                              
  log.resid.0 ~ dnorm(0, tau.red)  # starting residual for AR1 process
  
  # Ricker SR with AR1 process on log recruitment residuals for years with brood year spawners
  for (y in (A+a.min):nRyrs) {
    log.R[y] ~ dnorm(log.R.mean.2[y], tau.R)  # true state R is lognormally distributed around the prediction given by SR with AR1
    R[y] <- exp(log.R[y])
    log.R.mean.1[y] <- lnalpha + log(S[y-a.max]) - beta * S[y-a.max]
    log.resid.a[y] <- log.R[y] - log.R.mean.1[y]
  }             
  
  log.R.mean.2[A+a.min] <- log.R.mean.1[A+a.min] + phi * log.resid.0
  
  for (y in (A+a.min+1):nRyrs) {
    log.R.mean.2[y] <- log.R.mean.1[y] + phi * log.resid.a[y-1]
  }
  
  #derived quantities
  tau.red <- tau.R * (1 - phi * phi)
  sigma.red <- 1 / sqrt(tau.red)
  sigma.R <- 1 / sqrt(tau.R)
  alpha <- exp(lnalpha)
  log.resid <- log.resid.a[(A+a.min):nRyrs]
  
  # First `a.max` years of recruits, for which there is no spawner link
  mean.log.R0 ~ dnorm(0, 1E-4) 
  mean.R0 <- exp(mean.log.R0)
  tau.R0 ~ dgamma(0.1,0.1)
  sigma.R0 <- 1/sqrt(tau.R0)
  for (y in 1:a.max) {
    log.R[y] ~ dnorm(mean.log.R0, tau.R0)   
    R[y] <- exp(log.R[y])
  }
  
  # biological reference points: derived quantities
  lnalpha.c <- lnalpha + (sigma.R * sigma.R)/2/(1-phi * phi)
  S.max <- 1/beta
  S.eq <- lnalpha.c * S.max
  S.msy <- S.eq * (0.5 - 0.07 * lnalpha.c)
  U.msy <- lnalpha.c * (0.5 - 0.07 * lnalpha.c)
  
  # Maturity schedule: here we use a common maturation schedule to draw the brood year specific schedules;
  prob[1] ~ dbeta(1,1)
  prob[2] ~ dbeta(1,1)
  prob[3] ~ dbeta(1,1)
  pi[1]<- prob[1]
  pi[2] <- prob[2] * (1 - pi[1])
  pi[3] <- prob[3] * (1 - pi[1] - pi[2])
  pi[4] <- 1 - pi[1] - pi[2] - pi[3]
  
  D.scale ~ dunif(.045,1)
  D.sum <- 1 / (D.scale * D.scale)
  for (a in 1:A) {
    gamma[a] <- D.sum * pi[a]
    for (y in 1:(Y+A-1)) {                                                    
      g[y,a] ~ dgamma(gamma[a],1.0)
      p[y,a] <- g[y,a]/sum(g[y,])
    }
  }
  
  # Calculate the numbers at age matrix as brood year recruits at age (proportion that matured that year)
  for (t in 1:Y) {
    for(a in 1:A){
      N.ta[t,a] <- R[t+A-a] * p[t+A-a,a]
    }
  }
  
  ## OBSERVATION SUBMODEL ##
  # multinomial scale sampling
  for (t in 1:Y) {
    for (a in 1:A) {
      q[t,a] <- N.ta[t,a]/N[t]
    }
    x[t,1:A] ~ dmulti(q[t,1:A], n[t])
  }
  
  for (t in 1:Y) {
    # get observation tau's from assumed CV's
    log.sigma.C[t] <- sqrt(log((C.cv[t]^2) + 1))
    log.tau.C[t] <- 1/log.sigma.C[t]^2
    log.sigma.S[t] <- sqrt(log((S.cv[t]^2) + 1))
    log.tau.S[t] <- 1/log.sigma.S[t]^2
    
    # catch model
    U[t] ~ dunif(0.01, 0.99)
    N[t] <- sum(N.ta[t,1:A])
    S[t] <- N[t] * (1 - U[t])
    
    C[t] <- N[t] * U[t]
    log.C[t] <- log(C[t])
    C.obs[t] ~ dlnorm(log.C[t], log.tau.C[t])
    
    # escapement model
    log.S[t] <- log(S[t])
    S.obs[t] ~ dlnorm(log.S[t], log.tau.S[t])
  }

}

", fill=TRUE, file=modelFilename)

#file.show(modelFilename)

#------------------------------------------------------------------------------#
#  Jags inputs
#------------------------------------------------------------------------------#
	jags.data = list('Y','a.min','a.max','A','nRyrs','S.cv','S.obs','C.cv','C.obs',
	               'x','n','bpmu','bptau')
	
	jags.parms = c("R", "N", "S", "U", "alpha", "beta", "lnalpha", "phi", "C", "log.resid",
	               "log.resid.0","sigma.R", "lnalpha.c", "mean.log.R0", "pi", "q", 
	               "mean.R0", "sigma.R0","S.msy", "S.max", "S.eq", "U.msy", "gamma", 
	               "D.sum", "p","log.S")
#------------------------------------------------------------------------------#
#   Fit Model
#------------------------------------------------------------------------------#
ptm = proc.time()

	jagsfit.p <- jags.parallel(data=jags.data,  parameters.to.save=jags.parms,n.thin=15,
	              n.iter=300000, model.file=modelFilename,n.burnin = 50000,n.chains=6)

endtime = proc.time()-ptm
endtime[3]/60

post = as.mcmc(jagsfit.p)
mypost = as.matrix(post, chain=F)

saveRDS(post,"outputs/Atnarko_posteriors.baseline.May142019.mcmc")

#------------------------------------------------------------------------------#
# Model diagnostics and parameter summary
#------------------------------------------------------------------------------#

#potential scale reduction factor and trace plots

gelman.diag(post, multivariate = F) 
plot.mcmc(post)

# summarize posteriors for leading parameters and print
R = post.summ(post, "R[")
S = post.summ(post, "S[")
N = post.summ(post, "N[")
U = post.summ(post, "U[")
resid = post.summ(post, "log.resid[")
PP = post.summ(post, "pi[")

alpha = post.summ(post, "alpha")
beta = post.summ(post, "beta")
phi = post.summ(post, "phi")
sigma = post.summ(post, "sigma.R")
S.msy = post.summ(post, "S.msy")
S.eq = post.summ(post, "S.eq")
S.max = post.summ(post, "S.max")
U.msy = post.summ(post, "U.msy")

round(rbind(alpha, beta, sigma, phi, S.msy, S.eq, S.max, U.msy), 2)

quantile(mypost[,'beta'],c(0.025,0.5,0.975))

# re-index spawners, recruits, etc. 

S <- S[,c(1,12,23,34,39:43,2:11,13:22,24:33,35:38)]
R <- R[,c(1,12,23,34,42,43,44,45,46,2:11,13:22,24:33,35:41)]
U <- U[,c(1,12,23,34,39:43,2:11,13:22,24:33,35:38)]
N <- N[,c(1,12,23,34,39:43,2:11,13:22,24:33,35:38)]
resid <- resid[,c(1,12,23,34,36:39,2:11,13:22,24:33,35)]



  