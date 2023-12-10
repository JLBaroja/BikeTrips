rm(list=ls())
bikes <- read.csv('bikes.csv')

try(dev.off())
x11(width=10,height=10)
par(mar=rep(2,4))
layout(1:2,heights=c(2,1))
# Top plot
plot(NULL,xlim=c(0,1440),ylim=c(0,100),axes=F)
hours <- 0:23
hr_avg <- array(dim=c(length(unique(bikes$day_quarter)),length(hours)))
for(dy in unique(bikes$day_quarter)){
#for(dy in 51){ # Day 51 in q3 was August 20, the day Tropical Storm Hillary hit LA!
	dy_bikes <- subset(bikes,day_quarter==dy)
	for(hr in hours){
		hr_avg[dy,hr+1] <- mean(dy_bikes$interarrivals[dy_bikes$start_hour_day==hr],na.rm=T)
	}
	lines(dy_bikes$start_min_day,dy_bikes$interarrivals,col='#00000022')
}
for(dy in unique(bikes$day_quarter)){
	lines(hours*60+0.5*60,hr_avg[dy,],lwd=2,type='o',pch=21,bg='#eeeeeea0',col='#3000ad82')
}
hr_min <- seq(0,1440,60)
axis(1,at=hr_min,labels=hr_min/60)
points(hours*60+0.5*60,
	apply(hr_avg,MARGIN=2,FUN=mean,na.rm=T),
	type='o',pch=21,bg='#ffffffa0',col='#dd0012',lwd=2,cex=2)
# Bottom plot
plot(NULL,xlim=c(0,24),ylim=c(0,3),axes=F)
for(dy in unique(bikes$day_quarter)){
#for(dy in 51){
	lines(hours+0.5,1/hr_avg[dy,],lwd=2,type='o',pch=21,bg='#eeeeeea0',col='#3000ad82')
}
axis(1,at=0:24)
points(hours+0.5,
	1/apply(hr_avg,MARGIN=2,FUN=mean,na.rm=T),
	type='o',pch=21,bg='#ffffffc3',col='#dd0082',cex=2,lwd=2)
