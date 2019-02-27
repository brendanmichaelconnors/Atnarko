#------------------------------------------------------------------------------#
# Run closed loop simulations across a range of harves rates
#------------------------------------------------------------------------------#

#load posterior samples
posteriors = read.csv("outputs/Atnarko_posteriors.June292018.csv")

# set number of simulations and create array to store results 
num.sims = 10000
ny = 27 #(7 years plus length of forward sim)

harvest_rate <- seq(0,0.4,length.out=21)
outcomes.1.median <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.lower10 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.upper90 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.lower20 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.upper80 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.lower30 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.upper70 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.lower40 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.1.upper60 <- array(NA,dim=c(ny,3, length(harvest_rate)))
outcomes.2 <- array(NA,dim=c(ny,3, num.sims))
outcomes.3 <- array(NA,dim=c(1,2, num.sims))
outcomes.4 <- array(NA,dim=c(1,2, length(harvest_rate)))

# set conditions for simulation function
alpha <- process.iteration(posteriors[1,])$alpha
beta <- process.iteration(posteriors[1,])$beta
Ro <- log(alpha)/beta; sum(Ro)

ptm <- proc.time()

# run simulations!
for (w in 1:length(harvest_rate)){
		
	for (l in 1: num.sims){
		draw <- sample(10000,1)
		alpha <- process.iteration(posteriors[draw,])$alpha
		beta <- process.iteration(posteriors[draw,])$beta
		sigma.R <- process.iteration(posteriors[draw,])$sigma_R
		mat <- process.iteration(posteriors[draw,])$mat.sch
		Rec <- process.iteration(posteriors[draw,])$R
		Spw <- process.iteration(posteriors[draw,])$S
		lst.resid <- process.iteration(posteriors[draw,])$last_resid
		phi <- process.iteration(posteriors[draw,])$phi
		
		out <- process(ny,Ro,phi,mat,harvest_rate[w],alpha,beta,sigma.R,Rec,Spw,lst.resid)
		outcomes.2[,1,l] <- out$S
		outcomes.2[,2,l] <- out$N
		outcomes.2[,3,l] <- out$survival
		outcomes.3[,1,l] <- out$P[1]
		outcomes.3[,2,l] <- out$P[2]
	}	
	
outcomes.1.median[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.5),na.rm=T)
outcomes.1.lower10[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.1),na.rm=T)
outcomes.1.upper90[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.9),na.rm=T)
outcomes.1.lower20[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.2),na.rm=T)
outcomes.1.upper80[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.8),na.rm=T)
outcomes.1.lower30[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.3),na.rm=T)
outcomes.1.upper70[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.7),na.rm=T)
outcomes.1.lower40[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.4),na.rm=T)
outcomes.1.upper60[,,w] <- apply(outcomes.2,c(1,2),quantile,probs=c(0.6),na.rm=T)
outcomes.4[,,w] <- apply(outcomes.3,c(1,2),mean,na.rm=T)

}

(proc.time() - ptm)/60
