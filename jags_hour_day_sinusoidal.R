library('R2jags')
rm(list=ls())
bikes <- read.csv('bikes.csv')
t <- bikes$start_min_day # Time in minutes of trip start
itr <- bikes$interarrivals # Interarrivals (time since last trip start)
weekdays <- bikes$weekday # Day of the week (Monday, Tuesday, ...) 
hours <- bikes$start_hour_day
n_obs <- length(itr) # Total number of observations
unobserved <- c('lambda_post','A1','A2','f1','f2','phi1','phi2','C')

# Exponential distribution of IRT by hour and weekday
weekday_number <- rep(NA,length(weekdays))
day_labs <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
for(dl in 1:length(day_labs)){
	weekday_number[weekdays==day_labs[dl]] <- dl
}
observed <- list('itr','n_obs','hours','weekday_number','pi')
write('
model{ 
	
	# Sinusoidal model
  A1 <- -1
  f1 <- 1/24
	f2 ~ dunif(1/24,4/24)
	
	for(d in 1:7){
  	A2 [d]~ dunif(-0.1,0)
		phi1[d] ~ dunif(0,1)
		phi2 [d]~ dunif(1,3)
		C[d] ~ dunif(-(A1+A2[d]),1.5)
		for(j in 1:24){
			# Rates
			lambda_post[j,d] <- A1*sin(2*pi*f1*j+phi1[d])+A2[d]*sin(2*pi*f2*j+phi2[d])+C[d]
		}
	}
	# Observed nodes
	for(i in 1:n_obs){
		itr[i]~dexp(lambda_post[hours[i],weekday_number[i]]) 
	}
}
','hour_day_sinusoidal.bug')
ins <- list(
	list(C=runif(7,1.1,1.2)),
	list(C=runif(7,1.1,1.2)),
	list(C=runif(7,1.1,1.2)),
	list(C=runif(7,1.1,1.2))
)
bayes <- jags(data=observed,
						  model.file='hour_day_sinusoidal.bug',
							inits=ins,
							parameters.to.save=unobserved,
							n.iter=8000,n.chains=4,n.thin=20,n.burnin=2000)
unlink('hour_day_sinusoidal.bug')
nds <- bayes$BUGSoutput$sims.list
print(summary(bayes$BUGSoutput$summary))
save(nds,file='post_hour_day_sinusoidal.RData')
