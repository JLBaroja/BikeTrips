library('R2jags')
rm(list=ls())
bikes <- read.csv('bikes.csv')
t <- bikes$start_min_day # Time in minutes of trip start
itr <- bikes$interarrivals # Interarrivals (time since last trip start)
weekdays <- bikes$weekday # Day of the week (Monday, Tuesday, ...) 
hours <- bikes$start_hour_day
n_obs <- length(itr) # Total number of observations
unobserved <- c('lambda_post','A1','A2','f1','f2','phi1','phi2','C')

# Exponential distribution of IRT by hour
observed <- list('itr','n_obs','hours','pi')
write('
model{ 

	# Sinusoidal model
  A1 <- -1
  A2 ~ dunif(-.5,0)
	f1 <- 1/24
	f2 ~ dunif(1/24,4/24)
	phi1 ~ dunif(0,1)
	phi2 ~ dunif(0,1)
	#C <- 1.5
	C ~ dunif(-(A1+A2),1.5)

	# Rates
	for(j in 1:24){
		lambda_post[j] <- A1*sin(2*pi*f1*j+phi1)+A2*sin(2*pi*f2*j+phi2)+C
	}
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda_post[hours[i]]) 
	}
}
','hour_sinusoidal.bug')
bayes <- jags(data=observed,
						  model.file='hour_sinusoidal.bug',
							parameters.to.save=unobserved,
							n.iter=2000,n.chains=3,n.thin=5,n.burnin=0)
unlink('hour_sinusoidal.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_hour_sinusoidal.RData')
