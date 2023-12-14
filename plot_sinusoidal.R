
sinusoidal <- function(t,A1=1,A2=.5,f1=1/24,f2=2/24,phi1=1,phi2=0,C=1.5){
	f <- A1*sin(2*pi*f1*t+phi1)+A2*sin(2*pi*f2*t+phi2)+C
	return(f)
}

try(dev.off())
x11(width=10,height=5)
plot(NULL,xlim=c(0,24),ylim=c(0,3))
t <- seq(0,24,.001)
values1 <- seq(2,4*pi,length.out=10)
values2 <- seq(0,0,length.out=6)
day_names <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
colors <- c("#66a61e", "#e6ab02", "#1b9e77", "#d95f02", "#7570b3","#fd2c3b","#77d6cf")
for(v2 in 1:length(values2)){
for(v1 in 1:length(values1)){
	lines(t,sinusoidal(t,
		phi2=values2[v2],
		phi1=values1[v1]),
		col=rgb(v2/length(values2),0,0),
		lwd=v1*.5)
}}

