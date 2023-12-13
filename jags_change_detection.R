library('R2jags')
rm(list=ls())
bikes <- read.csv('bikes.csv')
t <- bikes$start_min_day # Time in minutes of trip start
itr <- bikes$interarrivals # Interarrivals (time since last trip start)
weekdays <- bikes$weekday # Day of the week (Monday, Tuesday, ...) 
n_obs <- length(itr) # Total number of observations
observed <- list('t','itr','n_obs')
unobserved <- c('lambda','tau1','tau2','z')
write('
model{ 
	# Change point
	tau1 ~ dunif(0,1440)
	tau2 ~ dunif(tau1,1440) # Forces tau2 > tau1

	# Rates
	lambda[1]~dunif(0,3)
	lambda[2]~dunif(0,3)
	lambda[3]~dunif(0,3)
	
	for(i in 1:n_obs){
		z1[i] <- step(t[i]-tau1) # Is t > tau1?
		z2[i] <- step(t[i]-tau2) # Is t > tau2?
		z[i] <- z1[i]+z2[i]+1
	}
		
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda[z[i]]) 
	}
}
','exponential_change_detection.bug')
my_inits <- list(
	list(tau1=5*60,tau2=22*60),
	list(tau1=6*60,tau2=23*60),
	list(tau1=5.5*60,tau2=22.5*60))
bayes <- jags(data=observed,
						  model.file='exponential_change_detection.bug',
							parameters.to.save=unobserved,
							inits=my_inits,
							n.iter=1000,n.chains=3,n.thin=10,n.burnin=00)
unlink('exponential_change_detection.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_change_detection.RData')
