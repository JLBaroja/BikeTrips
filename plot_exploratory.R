#rm(list=ls())
bikes <- read.csv('bikes.csv')

if(T){
try(dev.off())
x11(width=10,height=10)
par(mar=rep(2,4))
layout(1:2,heights=c(2,1))
# Top plot
plot(NULL,xlim=c(0,1440),ylim=c(0,100),axes=F)
hours <- 1:24
hr_avg <- array(dim=c(length(unique(bikes$day_quarter)),length(hours)))
weekday <- rep(NA,length(unique(bikes$day_quarter)))
for(dy in unique(bikes$day_quarter)){
#for(dy in 51){ # Day 51 in q3 was August 20, the day Tropical Storm Hillary hit LA!
	dy_bikes <- subset(bikes,day_quarter==dy)
  weekday[dy] <- unique(dy_bikes$weekday)
	for(hr in hours){
		hr_avg[dy,hr] <- mean(dy_bikes$interarrivals[dy_bikes$start_hour_day==hr],na.rm=T)
	}
	#lines(dy_bikes$start_min_day,dy_bikes$interarrivals,col='#00000022')
	points(dy_bikes$start_min_day,dy_bikes$interarrivals,col='#000000a2',pch=16,cex=.6)
}
for(dy in unique(bikes$day_quarter)){
	lines(hours*60-0.5*60,hr_avg[dy,],lwd=2,type='o',pch=21,bg='#eeeeeea0',col='#3000ad82')
}
hr_min <- seq(0,1440,60)
axis(1,at=hr_min,labels=hr_min/60)
axis(2)
points(hours*60-0.5*60,
	apply(hr_avg,MARGIN=2,FUN=mean,na.rm=T),
	type='o',pch=21,bg='#ffffffa0',col='#dd0012',lwd=2,cex=2)
# Bottom plot
plot(NULL,xlim=c(0,24),ylim=c(0,3),axes=F)
day_names <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
colors <- c("#66a61e", "#e6ab02", "#1b9e77", "#d95f02", "#7570b3","#fd2c3b","#77d6cf")
for(dy in unique(bikes$day_quarter)){
	dy_bikes <- subset(bikes,day_quarter==dy)
#for(dy in 51){
	#lines(hours-0.5,1/hr_avg[dy,],lwd=2,type='o',pch=21,bg='#eeeeeea0',col='#3000ad82')
	lines(hours-0.5,1/hr_avg[dy,],lwd=2,type='o',pch=21,bg='#eeeeeea0',
		col=colors[which(day_names==unique(dy_bikes$weekday))])
}
axis(1,at=0:24)
axis(2)
points(hours-0.5,
	1/apply(hr_avg,MARGIN=2,FUN=mean,na.rm=T),
	type='o',pch=21,bg='#ffffffc3',col='#dd0082',cex=2,lwd=2)
}

if(F){
day_names <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
colors <- c("#66a61e", "#e6ab02", "#1b9e77", "#d95f02", "#7570b3","#fd2c3b","#77d6cf")
#try(dev.off())
x11(width=15,height=15)
layout(matrix(1:9,ncol=3,byrow=F))
for(wk in unique(day_names)){
	plot(NULL,xlim=c(0,24),ylim=c(0,3),axes=F,main=wk)
	for(dy in unique(bikes$day_quarter)){
		lines(hours-0.5,1/hr_avg[dy,],lwd=2,col='#00000016')
	}
	for(dy in which(weekday==wk)){
		dy_bikes <- subset(bikes,day_quarter==dy)
		lines(hours-0.5,1/hr_avg[dy,],lwd=2,type='o',pch=21,bg='#eeeeeea0',
			col=colors[which(day_names==unique(dy_bikes$weekday))])
	}
axis(1,at=0:24)
}
}


#try(dev.off())
#x11(width=20,height=10)
#layout(matrix(1:24,ncol=6,byrow=T))
#for(hr in hours){
#	hr_trips <- subset(bikes,start_hour_day==hr)
#	hist(hr_trips$interarrivals,breaks=200,xlim=c(0,50),
#		main=hr)
#}
