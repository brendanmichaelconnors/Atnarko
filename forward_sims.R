#------------------------------------------------------------------------------#
# Run closed loop simulations across a range of harves rates
#------------------------------------------------------------------------------#

#load posterior samples
sr_model.mcmc <- readRDS("outputs/Atnarko_posteriors.baseline.April302019.mcmc")
posteriors = as.matrix(sr_model.mcmc, chain=F)

# set number of simulations and create array to store results 
num.sims = 1000
ny = 27 #(7 years plus length of forward sim)
harvest_rate <- seq(0,0.4,length.out=21)
temporal.outcomes <- array(NA,dim=c(ny,3,num.sims,length(harvest_rate)))
static.outcomes <- array(NA,dim=c(1,2, num.sims,length(harvest_rate)))

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
		temporal.outcomes[,1,l,w] <- out$S
		temporal.outcomes[,2,l,w] <- out$N
		temporal.outcomes[,3,l,w] <- out$survival
		static.outcomes[,1,l,w] <- out$P[1]
		static.outcomes[,2,l,w] <- out$P[2]
	}	
}
saveRDS(temporal.outcomes,"outputs/forward_sims_baseline.temp_out.Apr302019")  
saveRDS(static.outcomes,"outputs/forward_sims_baseline.static_out.Apr302019")  
	
(proc.time() - ptm)/60
