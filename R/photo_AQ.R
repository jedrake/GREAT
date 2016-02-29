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
  toplot <- subset(aq.l[[i]],campaign==1)
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
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+prov,data=subset(aq,campaign==1),FUN=c(mean,standard.error))

plot3d(x=aq.m$Tleaf.mean,y=aq.m$PARi.mean,z=aq.m$Photo.mean,col=palette()[aq.m$prov],size=15)
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
         axes=F,add=T,cex=2)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white",cex=1.5)
#-------------------------------------------------------






#-------------------------------------------------------
#- plot just the drought data's temperature response

#- first, merge in the soil moisture data measured associated with campaign 2.
vwc <- getVWC_AQ()

vwc.aq <- merge(subset(aq,campaign==2 & LightFac==4),vwc,by=c("pot","Water_trt"))

#- plot 3-d relationship between Photosynthesis, conductance, and soil moisture
plot3d(x=vwc.aq$vwc,y=vwc.aq$Cond,z=vwc.aq$Ci,size=20,col=palette()[vwc.aq$Water_trt])

#- Photo
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
aq.l <- split(aq,aq$LightFac)
for(i in 1:length(aq.l)){
  toplot <- subset(aq.l[[i]],prov=="B" & campaign==2)
  plotBy(Photo~Tleaf|Water_trt,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo+1)),legend=F,pch=16,
         axes=F,col=c("blue","red"))
  legend("topright",paste("PAR = ",round(mean(toplot$PARi),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("Wet","Dry"),col=c("blue","red"),pch=16,ncol=2,bg="white")


#- Cond
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
aq.l <- split(aq,aq$LightFac)
for(i in 1:length(aq.l)){
  toplot <- subset(aq.l[[i]],prov=="B"& campaign==2)
  plotBy(Cond~Tleaf|Water_trt,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Cond+0.1)),legend=F,pch=16,
         axes=F,col=c("blue","red"))
  legend("topright",paste("PAR = ",round(mean(toplot$PARi),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(g[s]~(mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("Wet","Dry"),col=c("blue","red"),pch=16,ncol=2,bg="white")
#-------------------------------------------------------






#-------------------------------------------------------
#- plot TREATMENT MEANS
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+Water_trt,data=subset(aq,campaign==2),FUN=c(mean,standard.error))

#- plot each light level's temperature response
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
aq.m.l <- split(aq.m,aq.m$LightFac)
for(i in 1:length(aq.m.l)){
  toplot <- aq.m.l[[i]]
  plotBy(Photo.mean~Tleaf.mean|Water_trt,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo.mean+1)),legend=F,pch=16,
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Photo.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  plotBy(Photo.mean~Tleaf.mean|Water_trt,data=toplot,las=1,xlim=c(15,45),ylim=c(0,max(toplot$Photo.mean+1)),legend=F,pch=16,
         axes=F,add=T,cex=2)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("Wet","Dry"),col=palette()[1:2],pch=16,ncol=2,bg="white")
#-------------------------------------------------------







#-------------------------------------------------------
#- fit AvT to estimate Topts.
tofit <- subset(aq,campaign==1 & LightFac==4 & Water_trt=="wet")
tofit.l <- split(tofit,tofit$prov)
AvTfits <- data.frame(do.call(rbind,lapply(tofit.l,FUN=fitAvT)))

windows(30,60);par(mfrow=c(3,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
#- plot Asat at Topt
barplot2(height=AvTfits$Aref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,30),las=1,
         ylab=expression(A[ref]~(mu*mol~m^-2~s^-1)),
         ci.l=AvTfits$Aref-AvTfits$Aref.se,ci.u=AvTfits$Aref+AvTfits$Aref.se)
#- plot Topt
barplot2(height=AvTfits$Topt,names.arg=c("A","B","C"),plot.ci=T,las=1,ylim=c(0,30),
         ylab=expression(T[opt]~(degree*C)),
         ci.l=AvTfits$Topt-AvTfits$Topt.se,ci.u=AvTfits$Topt+AvTfits$Topt.se)
#- plot Theta
barplot2(height=AvTfits$theta,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,20),las=1,
         ylab=expression(Omega~(degree*C)),
         ci.l=AvTfits$theta-AvTfits$theta.se,ci.u=AvTfits$theta+AvTfits$theta.se)
title(xlab="Provenance",outer=T,cex.lab=2,adj=0.6)
#-------------------------------------------------------
