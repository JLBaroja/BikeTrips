library('R2jags')
rm(list=ls())
bikes <- read.csv('bikes.csv')
t <- bikes$start_min_day # Time in minutes of trip start
itr <- bikes$interarrivals # Interarrivals (time since last trip start)
weekdays <- bikes$weekday # Day of the week (Monday, Tuesday, ...) 
hours <- bikes$start_hour_day
n_obs <- length(itr) # Total number of observations
observed <- list('itr','n_obs','hours')
unobserved <- c('lambda_post')
# Exponential distribution of IRT by hour
write('
model{ 
	# Rates
	for(j in 1:24){
		lambda_post[j]~dunif(0,3)
	}
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda_post[hours[i]]) 
	}
}
','hour_exponential.bug')
bayes <- jags(data=observed,
						  model.file='hour_exponential.bug',
							parameters.to.save=unobserved,
							n.iter=10,n.chains=3,n.thin=1,n.burnin=0)
unlink('hour_exponential.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_hour_exponential.RData')

# Exponential distribution of IRT by hour and weekday
weekday_number <- rep(NA,length(weekdays))
day_labs <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
for(dl in 1:length(day_labs)){
	weekday_number[weekdays==day_labs[dl]] <- dl
}
observed <- list('itr','n_obs','hours','weekday_number')
write('
model{ 
	# Rates
	for(d in 1:7){
		for(j in 1:24){
			lambda_post[j,d]~dunif(0,3)
		}
	}
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda_post[hours[i],weekday_number[i]]) 
	}
}
','hour_day_exponential.bug')
bayes <- jags(data=observed,
						  model.file='hour_day_exponential.bug',
							parameters.to.save=unobserved,
							n.iter=100,n.chains=3,n.thin=1,n.burnin=0)
unlink('hour_day_exponential.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_hour_day_exponential.RData')
