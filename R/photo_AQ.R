library(plotBy)
library(doBy)
library(magicaxis)
library(rgl)
source("R/generic_functions.R")
source("R/GREAT_functions.R")

#- get the AQ data
aq <- getAQ()

#- plot each light level's temperature response
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
aq.l <- split(aq,aq$LightFac)
for(i in 1:length(aq.l)){
  toplot <- aq.l[[i]]
  plotBy(Photo~Tleaf|prov,data=subset(toplot,Water_trt=="wet"),las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo+1)),legend=F,pch=16,
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)

}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")


#- plot 3d object
plot3d(x=aq$Tleaf,y=aq$PARi,z=aq$Photo,size=7,col=palette()[as.factor(aq$prov)])
