library('R2jags')
rm(list=ls())
bikes <- read.csv('bikes.csv')
t <- bikes$start_min_day # Time in minutes of trip start
itr <- bikes$interarrivals # Interarrivals (time since last trip start)
weekdays <- bikes$weekday # Day of the week (Monday, Tuesday, ...) 
hours <- bikes$start_hour_day
n_obs <- length(itr) # Total number of observations
observed <- list('t','itr','n_obs')
unobserved <- c('lambda_prior','lambda_post')
write('
model{ 
	# Rates
	lambda_post~dunif(0,3)
	lambda_prior~dunif(0,3)
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda_post) 
	}
}
','simple_exponential.bug')
bayes <- jags(data=observed,
						  model.file='simple_exponential.bug',
							parameters.to.save=unobserved,
							n.iter=500,n.chains=3,n.thin=1,n.burnin=0)
unlink('simple_exponential.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_simple_exponential.RData')
