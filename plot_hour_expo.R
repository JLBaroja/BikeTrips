
#load('post_hour_exponential.RData')
load('post_hour_sinusoidal.RData')
nds_hr <- nds
#load('post_hour_day_exponential.RData')
load('post_hour_day_sinusoidal.RData')
nds_hr_dy <- nds
rm('nds')

days <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

#try(dev.off())
#x11(width=8,height=4)

pdf('posterior_rates.pdf',width=7.5,height=3)
par(mar=rep(2,4))
plot(NULL,xlim=c(0,25),ylim=c(0,3))

n_hours <- dim(nds_hr_dy$lambda_post)[2]
n_days <- dim(nds_hr_dy$lambda_post)[3]
colors <- c("#66a61e", "#e6ab02", "#1b9e77", "#d95f02", "#7570b3","#fd2c3b","#77d6cf")
transparency <- rep(c('22','ee'),c(5,2))
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
					x=c(1:24,24:1),
					col=paste(colors[dy],transparency[dy],sep=''),
					border=F)
}
	polygon(y=c(qt_global[1,],qt_global[2,24:1]),
					x=c(1:24,24:1),
					col='#ffffff00',
					border='#000000')

legend(0,3,xjust=0,yjust=1,legend=days,pt.bg=colors,pch=22)
dev.off()
