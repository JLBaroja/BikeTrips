
bikes <- read.csv('bikes.csv')

base_data <- function(color_data='#000000'){
	plot(NULL,xlim=c(0,1440),ylim=c(0,100),axes=F,ann=F)
	axis(1,at=(0:24)*60,labels=0:24)
	axis(2,las=1,hadj=0.5,labels=NA,pos=-30)
	axis(2,las=1,hadj=0.5,pos=-1*60,lwd=0,tck=0)
	mtext('Hours',1,line=1.25)
	mtext('Time between trips (mins.)',2,line=.75)
	mtext('Observed times between trips during the day',3,font=2,line=0)
	points(bikes$start_min_day,bikes$interarrivals,
		pch=16,col=color_data,cex=.45)
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
		#ht$mids[ht$density<0.0000] <- NA
		#ht$density[ht$density<0.0000] <- NA
		right <- ht$density*(0.45*60)/max(ht$density)+(hr-.5)*60
		left <- -ht$density*(0.45*60)/max(ht$density)+(hr-.5)*60
		#lines(right,ht$mids,lwd=2,col='#dddddd')
		#lines(left,ht$mids,lwd=2,col='#dddddd')
		polygon(x=c(right,left[length(left):1]),
			y=c(ht$mids,ht$mids[length(ht$mids):1]),
				#col='#8833edaa',border=F)
				lwd=.75,
				col='#23dbe188',border='#ddddddcc')
	}
}

#try(dev.off())
#x11(width=10,height=6)
pdf('plot_data.pdf',width=7.5,height=3)
par(mar=c(2.5,2,1.5,0),cex.axis=0.65,tck=-0.02,mgp=c(3,0.1,0))
#plot(NULL,xlim=c(0,24),ylim=c(0,3),axes=F,ann=F)
hist_hour()
dev.off()
