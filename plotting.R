rm(list=ls())
bikes <- read.csv('bikes.csv')
plot(NULL,xlim=c(-1440,1440),ylim=c(0,100),axes=F)
for(dy in unique(bikes$day_quarter)){
#for(dy in sample(1:max(bikes$day_quarter),size=2)){
	dy_bikes <- subset(bikes,day_quarter==dy)
	lines(dy_bikes$start_min_day-12*60,dy_bikes$interarrivals,col='#00000022')
}
hr_min <- seq(0,1440,60)
axis(1,at=hr_min,labels=hr_min/60)
