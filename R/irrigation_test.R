library(plotBy)
library(doBy)
library(magicaxis)
library(lme4)
library(plotrix)
source("//ad.uws.edu.au/dfshare/HomesHWK$/30035219/My Documents/Work/R/generic_functions.R")
source("C:/Repos/wtc3_flux_share/R/functions.R")

#- read in the irrigation testing data
irr1 <- read.csv("W://WORKING_DATA/GHS39/GREAT/Share/Data/irrigation testing.csv")
irr1$group <- factor(substr(irr1$dripper,start=1,stop=1))
irr <- subset(irr1,group %in% c(1,6) & runtime<5.5)
irr$group <- factor(irr$group)
levels(irr$group) <- c("near","far")
irr$combo <- as.factor(paste(irr$side,irr$line,irr$group,sep="-"))


lmirr1 <- lmList(volume~runtime|combo,data=irr)
lmirr1coef <- coef(lmirr1)

windows();par(mar=c(6,6,4,1))
irr.l <- split(irr,irr$combo)
with(irr,plot(runtime,volume,col="white",xlab="Time (minutes)",ylab="Volume (mL)",cex.lab=2,ylim=c(0,350),xlim=c(1,5)))
for(i in 1:length(irr.l)){
  xmin <- min(irr.l[[i]]$runtime)
  xmax <- max(irr.l[[i]]$runtime)
  
  ablineclip(lmirr1coef[i,1],lmirr1coef[i,2],x1=xmin,x2=xmax,col=irr.l[[i]]$combo[1],lty=1,lwd=2)
}
with(irr,points(jitter(runtime,factor=0.2),volume,col=combo,pch=16))
legend(x=1,y=420,levels(irr$combo),col=palette()[1:6],lty=1,ncol=3,xpd=NA)
