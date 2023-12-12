load('post_hour_day_exponential.RData')
days <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')

try(dev.off())
x11(width=8,height=4)
plot(NULL,xlim=c(0,25),ylim=c(0,3))
n_hours <- dim(nds$lambda_post)[2]
n_days <- dim(nds$lambda_post)[3]
colors <- c("#66a61e", "#e6ab02", "#1b9e77", "#d95f02", "#7570b3","#fd2c3b","#77d6cf")
transparency <- rep(c('22','ee'),c(5,2))
for(dy  in 1:n_days){
	quant_array <- array(dim=c(2,n_hours))
	for(hr in 1:n_hours){
		quant_array[,hr] <- quantile(nds$lambda_post[,hr,dy],probs=c(0.025,0.975))
		#lines(rep(hr+rnorm(1,0,0),2),quantile(nds$lambda_post[,hr,dy],probs=c(0.025,0.975)),
		#	col=colors[dy],lwd=3)
#	hist(nds$lambda_post[,hr],breaks=seq(0,3,length))
	}
	polygon(y=c(quant_array[1,],quant_array[2,24:1]),
					x=c(1:24,24:1),
					col=paste(colors[dy],transparency[dy],sep=''),
					border=F)
}
legend(0,3,xjust=0,yjust=1,legend=days,pt.bg=colors,pch=22)
