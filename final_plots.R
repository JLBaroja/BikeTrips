
bikes <- read.csv('bikes.csv')

base_data <- function(color_data='#000000'){
	plot(NULL,xlim=c(0,1440),ylim=c(0,100))
	points(bikes$start_min_day,bikes$interarrivals,
		pch=16,col=color_data,cex=.5)
}

hist_hour <- function(){
	base_data('#000000')
	#segments(x0=(0:24)*60,x1=(0:24)*60,
#		y0=rep(0,25),y1=rep(150,25))
	hrs <- 1:24
	for(hr in hrs){
		itr <- bikes$interarrivals[bikes$start_hour_day==hr]
		itr <- itr[itr<=100]
		#hist(itr,breaks=seq(0,150,length.out=150),plot=F)->ht
		hist(itr,breaks=seq(-.5,100.5,1),plot=F)->ht
		right <- ht$density*(0.45*60)/max(ht$density)+(hr-.5)*60
		left <- -ht$density*(0.45*60)/max(ht$density)+(hr-.5)*60
		#lines(right,ht$mids,lwd=2,col='#dddddd')
		#lines(left,ht$mids,lwd=2,col='#dddddd')
		polygon(x=c(right,left[length(left):1]),
			y=c(ht$mids,ht$mids[length(ht$mids):1]),col='#8833edaa',border=F)
	}
}

try(dev.off())
x11(width=10,height=6)
hist_hour()
