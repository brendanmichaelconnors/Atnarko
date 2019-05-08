## Functions for analysis

#------------------------------------------------------------------------------#
# Posterior summary function
#------------------------------------------------------------------------------#
post.summ = function(post.samp, var) {
  post.samp = as.matrix(post.samp)
  
  # if parameter is indexed
  if(substr(var, nchar(var), nchar(var)) == "[") {
    post = post.samp[,substr(colnames(post.samp), 1, nchar(var)) == var]
    summ = apply(post, 2, function(x) c(mean = mean(x), sd = sd(x), quantile(x, c(0.5, 0.025, 0.975, 0.1, 0.9, 0.2, 0.8, 0.3, 0.7))))
    return(summ)
  }
  
  # if parameter is not indexed
  if(substr(var, nchar(var), nchar(var)) != "[") {
    post = post.samp[,substr(colnames(post.samp), 1, nchar(var)) == var]
    summ = c(mean = mean(post), sd = sd(post), quantile(post, c(0.5, 0.025, 0.975)))
    return(summ)
  }
}

#------------------------------------------------------------------------------#
# Multi-stock simulation function
#------------------------------------------------------------------------------#
# ny <- the number of years
# Ro <- the sub-stock recruiment at time zero
# phi <- the expected correlation through time
# mat <- stock-specific maturation schedules
# alpha <- sub-stock productivity (not in log space)
# beta <- sub-stock density depedence 
# sigma.R <- recruitment variation
# U <- finite annual exploitation rate
# pm.yr <- year of simulation that pms start to be calculated over
# Rec <- estimated recruitments from last years of empirical data 
# Spw <- estimated spawers from last years of empirical data
# lst.resid <- estimated recruitment deviation from last year of empirical data

process = function(ny,Ro,phi,mat,U,alpha,beta,sigma.R,Rec,Spw,lst.resid){
	ns = length(Ro) #number of sub-stocks
	m.alpha <- alpha
	m.beta <- beta
	epi = rnorm(ny, sd= sigma.R)

	#Build time series of Spawners (S), abundance of returning spawners pre-harvest
	# (N), and the component of the residual that is correlated throught time (v)
	R = t(matrix(0,ns,ny))
	S = R * (1-0)
	v = R; v[,]=0
	#R[1:7,]=t(replicate(7,Ro,simplify=T))*exp(epi[1:7,])
	R[1:3,]=Rec
	N = array(0,dim=c(ny,4,ns))
	Ntot = R; Ntot[,]=0
	H = Ntot; S = Ntot
	S[4:7,] = Spw
	predR = Ntot
	
	# populate first few years with realized states
	R[4,] = exp(log(alpha[]*S[4,]*exp(-beta[]*S[4,])) + phi* lst.resid) * exp(epi[4])
	v[4,] = log(R[4,])-log(alpha[]*S[4,]*exp(-beta[]*S[4,]))
	
	for(i in 5:7){
		R[i,] = exp(log(alpha[]*S[i,]*exp(-beta[]*S[i,])) + phi* v[i-1,]) * exp(epi[i])
		v[i,] = log(R[i,])-log(alpha[]*S[i,]*exp(-beta[]*S[i,]))		
	}

	N[4:7,1,]=R[4:7-(3),] * mat[1]
	N[5:7,2,]=R[5:7-(4),] * mat[2]
	N[6:7,3,]=R[6:7-(5),] * mat[3]
	N[7,4,]=R[7-(6),] * mat[4]
	
	# Loop through years of simulation	  
	for(i in (7+1):ny){ 
		N[i,1,1] = R[i-(4),1] * mat[1]
		N[i,2,1] = R[i-(5),1] * mat[2]
		N[i,3,1] = R[i-(6),1] * mat[3]
		N[i,4,1] = R[i-(7),1] * mat[4]
		       
		Ntot[i,1] = sum(N[i,,1])

		# apply harvest 
		H[i,1] =  U*Ntot[i,1]
		S_exp = Ntot[i,1]-H[i,1] ; S_exp[S_exp<0] = 0
		S[i,1] = S_exp
			    
		# predict recruitment
		R[i,1] = alpha[]*S[i,1]*exp(-beta[]*S[i,1]+phi*v[i-1,1]+epi[i])
		predR[i,] = alpha[]*S[i,1]*exp(-beta[]*S[i,1])
		v[i,1] = log(R[i,1])-log(predR[i,1])
		v[v[,1]=='NaN'] <- 0
	    }
	  
	#Output
	S[S[,]=='NaN'] <- 0
	Ntot[Ntot[,]=='NaN'] <- 0
	p_rg <-ifelse(median(S[(ny-10):ny,])>15000,1,0)
	p_lrp <-ifelse(median(S[(ny-10):ny,])>4000,1,0)
	
	list(S=S[,],N=Ntot[,],survival=as.numeric(v),P=c(p_rg,p_lrp))
	}

#------------------------------------------------------------------------------#
# Function to sample from posteriors for forward simulations
#------------------------------------------------------------------------------#	

process.iteration = function(samp) {
  # 1.) extract names
  nms = names(samp)
  
  # 2.) extract elements according to the names and put them into the appropriate data structure
  
  # parameters
  alpha = unname(samp[substr(nms, 1, 5) == "alpha"])
  beta = unname(samp[substr(nms, 1, 5) == "beta"])
  last_resid = unname(samp[substr(nms, 1, 13) == "log.resid[40]"])
  phi = unname(samp["phi"])
  sigma_R = unname(samp["sigma.R"])
  mat.sch = c(as.numeric(samp["pi[1]"]), as.numeric(samp["pi[2]"]), as.numeric(samp["pi[3]"]), as.numeric(samp["pi[4]"]))
  
  # states
  S = c(as.numeric(samp["S[40]"]), as.numeric(samp["S[41]"]), as.numeric(samp["S[42]"]), as.numeric(samp["S[43]"]))
  R = c(as.numeric(samp["R[44]"]), as.numeric(samp["R[45]"]), as.numeric(samp["R[46]"]))
  
  # 3.) create output list
  output = list(
    alpha = as.numeric(alpha),
    beta = as.numeric(beta),
    phi = as.numeric(phi),
    last_resid = as.numeric(last_resid),
    sigma_R = as.numeric(sigma_R),
    mat.sch = mat.sch,
    S = S,
    R = R
    )

  # 4.) return output
  return(output)

}
