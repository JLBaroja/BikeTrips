
bikes <- read.csv('bikes.csv')

load('post_hour_exponential.RData')
#load('post_hour_sinusoidal.RData')
nds_hr <- nds
load('post_hour_day_exponential.RData')
#load('post_hour_day_sinusoidal.RData')
#load('post_hour_day_sinusoidal_3c.RData')
nds_hr_dy <- nds
rm('nds')

days <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
colors <- c("#2b6abe", "#e6ab02", "#1b9e77", "#d95f02", "#7125bd","#2a9739","#8dd983")
transparency <- rep(c('44','ee'),c(5,2))

#try(dev.off())
#x11(width=8,height=4)

pdf('posterior_rates.pdf',width=7.5,height=3)
par(mar=c(2.5,2.75,1.5,0),cex.axis=0.65,tck=-0.02,mgp=c(3,0.1,0))
plot(NULL,xlim=c(0,24),ylim=c(0,3),axes=F,ann=F)
#plot(NULL,xlim=c(12,19),ylim=c(1,2.5),axes=F,ann=F)
axis(1,at=0:24)
axis(2,las=1,hadj=1.4)
mtext('Hours',1,line=1.25)
mtext('Posterior Rate',2,line=01.5)
mtext('Inferred rates of interarrival times',3,font=2,line=0)

#points(bikes$start_min_day*24/1440,(bikes$interarrivals)*6/max(bikes$interarrivals))

n_hours <- dim(nds_hr_dy$lambda_post)[2]
n_days <- dim(nds_hr_dy$lambda_post)[3]
qt_global <- array(dim=c(2,n_hours))
for(dy  in 1:n_days){
	quant_array <- array(dim=c(2,n_hours))
	for(hr in 1:n_hours){
		quant_array[,hr] <- quantile(nds_hr_dy$lambda_post[,hr,dy],probs=c(0.025,0.975))
		if(dy==n_days){
			qt_global[,hr] <- quantile(nds_hr$lambda_post[,hr],
				probs=c(0.025,0.975))
		}
	}
	polygon(y=c(quant_array[1,],quant_array[2,24:1]),
					x=c(1:24,24:1)-.5,
					col=paste(colors[dy],transparency[dy],sep=''),
					border=F)
}
	polygon(y=c(qt_global[1,],qt_global[2,24:1]),
					x=c(1:24,24:1)-.5,
					lty='11',
					col='#ffffff00',
					border='#000000')

legend(0,3,xjust=0,yjust=1,legend=days,
	pt.bg=paste(colors,rep(c('77','ee'),c(5,2)),sep=''),
	pt.cex=1.5,col='#ffffff00',pch=22,cex=0.75)
dev.off()
