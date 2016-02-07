library(plotBy)
library(doBy)
library(magicaxis)
library(rgl)
source("R/generic_functions.R")
source("R/GREAT_functions.R")

#- get the AQ data
aq <- getAQ()


#-------------------------------------------------------
#- plot the well-watered data

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
#-------------------------------------------------------




#-------------------------------------------------------
#- plot TREATMENT MEANS
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+prov,data=aq,FUN=c(mean,standard.error))

#- plot each light level's temperature response
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
aq.m.l <- split(aq.m,aq.m$LightFac)
for(i in 1:length(aq.m.l)){
  toplot <- aq.m.l[[i]]
  plotBy(Photo.mean~Tleaf.mean|prov,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo.mean+1)),legend=F,pch=16,
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Photo.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  plotBy(Photo.mean~Tleaf.mean|prov,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo.mean+1)),legend=F,pch=16,
         axes=F,add=T)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")
#-------------------------------------------------------






#-------------------------------------------------------
#- plot just the drought data's temperature response
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
aq.l <- split(aq,aq$LightFac)
for(i in 1:length(aq.l)){
  toplot <- subset(aq.l[[i]],prov=="B")
  plotBy(Photo~Tleaf|Water_trt,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo+1)),legend=F,pch=16,
         axes=F,col=c("blue","red"))
  legend("topright",paste("PAR = ",round(mean(toplot$PARi),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("Wet","Dry"),col=c("blue","red"),pch=16,ncol=2,bg="white")
#-------------------------------------------------------

