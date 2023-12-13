library('R2jags')
rm(list=ls())
bikes <- read.csv('bikes.csv')
t <- bikes$start_min_day # Time in minutes of trip start
itr <- bikes$interarrivals # Interarrivals (time since last trip start)
weekdays <- bikes$weekday # Day of the week (Monday, Tuesday, ...) 
hours <- bikes$start_hour_day
n_obs <- length(itr) # Total number of observations
observed <- list('itr','n_obs','hours','pi')
unobserved <- c('lambda_post','A1','A2','f1','f2','phi1','phi2','C')
# Exponential distribution of IRT by hour
write('
model{ 

	# Sinusoidal model
  A1 ~ dunif(0,2)
  A2 ~ dunif(0,2)
	f1 ~ dunif(1/36,1/12)
	f2 ~ dunif(1/36,1/12)
	phi1 ~ dunif(0,12)
	phi2 ~ dunif(0,12)
	C ~ dunif((A1+A2),3)

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

# Exponential distribution of IRT by hour and weekday
weekday_number <- rep(NA,length(weekdays))
day_labs <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
for(dl in 1:length(day_labs)){
	weekday_number[weekdays==day_labs[dl]] <- dl
}
observed <- list('itr','n_obs','hours','weekday_number','pi')
write('
model{ 
	
	for(d in 1:7){
	# Sinusoidal model
  A1 [d]~ dunif(0,2)
  A2 [d]~ dunif(0,2)
	f1 [d]~ dunif(1/36,1/12)
	f2 [d]~ dunif(1/36,1/12)
	phi1 [d]~ dunif(0,12)
	phi2 [d]~ dunif(0,12)
	C [d]~ dunif((A1[d]+A2[d]),3)
		for(j in 1:24){
			# Rates
			lambda_post[j,d] <- A1[d]*sin(2*pi*f1[d]*j+phi1[d])+A2[d]*sin(2*pi*f2[d]*j+phi2[d])+C[d]
		}
	}
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda_post[hours[i],weekday_number[i]]) 
	}
}
','hour_day_sinusoidal.bug')
bayes <- jags(data=observed,
						  model.file='hour_day_sinusoidal.bug',
							parameters.to.save=unobserved,
							n.iter=2000,n.chains=3,n.thin=5,n.burnin=0)
unlink('hour_day_sinusoidal.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_hour_day_sinusoidal.RData')
