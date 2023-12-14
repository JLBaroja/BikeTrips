rm(list=ls())
load('post_hour_day_sinusoidal.RData')
source('aux_days_colors.R')

plot_marginal <- function(node_name = 'C',
													axis_label = NULL,
													xlimz=NULL,
													label=NULL){
  if(is.null(xlimz)){
		xlimz = c(min(nds[[node_name]]),max(nds[[node_name]]))
		xlimz = c(xlimz[1]-(xlimz[2]-xlimz[1])*.1,
							xlimz[2]+(xlimz[2]-xlimz[1])*.1)
	}
	brks = seq(xlimz[1],xlimz[2],length.out=50)
	#densities <- array(dim=c(7,length(brks)-1))
	densities <- array(dim=c(7,512))
	mids <- array(dim=c(7,512))
	for(dy in 1:7){
		ht <- hist(nds[[node_name]][,dy],breaks=brks,plot=F)
		#densities[dy,] <- ht$density
		ht <- density(nds[[node_name]][,dy])
		densities[dy,] <- ht$y
		mids[dy,] <- ht$x
	}
	#mids <- ht$mids
	#transparency <- rep(c('44','ee'),c(5,2))
	ylimz=c(0,max(densities,na.rm=T)*1.5)
	plot(NULL,xlim=xlimz, 
		ylim=ylimz,
		ann=F,axes=F)
	mtext(axis_label,1,line=1.75,cex=0.85)
	text(xlimz[1],ylimz[2],label,cex=2,font=2,adj=c(0,1))
	box()
	axis(1)
	for(dy in 1:7){
		dens <- densities[dy,]
		#polygon(x=c(xlimz[1],mids,xlimz[2]),
		polygon(x=c(xlimz[1],mids[dy,],xlimz[2]),
						y=c(0,dens,0),
						col=paste(day_cols[dy],'44',sep=''),
						border=F)
		dens[dens==0] <- NA
		lines(mids[dy,],dens,col=day_cols[dy],lwd=2)
	}
}

pdf('sinusoidal_marginals.pdf',width=6,height=4)
par(mar=c(2,1,2,1),oma=c(2,0,0,0),
		cex.axis=.8,tck=-0.03,mgp=c(3,0.25,0))
layout(matrix(1:6,ncol=3,byrow=F))
plot_marginal('phi1',label='A',
	axis_label=expression(paste('Offset'[1],' (',delta[1],')')),xlimz=c(0.4,0.65))
plot_marginal('phi2',label='B',
	axis_label=expression(paste('Offset'[2],' (',delta[2],')')),xlimz=c(1,3))
plot_marginal('A2',label='C',
	axis_label=expression(paste('Amplitude'[2],' (',alpha[2],')')),xlimz=c(-0.06,-0.01))
plot_marginal('C',label='D',
	axis_label=expression(paste('Average Rate',' (',beta,')')),xlimz=c(1.01,1.06))
par(mar=c(2,2,2,1.65))
plot(NULL,xlim=c(-0.06,-0.01),ylim=c(0.4,0.65),axes=F)
text(-0.06,0.65,'E',cex=2,font=2,adj=c(0,1))
box()
axis(1)
axis(2,padj=-0.5)
mtext(expression(alpha[2]),1,cex=0.85,line=1.25)
mtext(expression(delta[1]),2,cex=0.85,line=1.25)
indx <- sample(dim(nds$phi1)[1],size=300)
for(dy in 1:7){
	points(nds$A2[indx,dy],nds$phi1[indx,dy],
		bg=paste(day_cols[dy],'44',sep=''),
		col=paste(day_cols[dy],'44',sep=''),
		pch=21,cex=0.5)
}
par(mar=rep(1,4))
plot(NULL,xlim=c(-1,1),ylim=c(-1,1),axes=F,ann=F)
legend(0,0,xjust=0.5,yjust=0.5,legend=days,
	pt.bg=paste(day_cols,'77',sep=''),pt.lwd=1.5,
	pt.cex=2,col=day_cols,pch=21,cex=1.25,
	box.lty='blank')
dev.off()
