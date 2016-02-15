library(plotBy)
library(doBy)
library(magicaxis)
library(rgl)
source("R/generic_functions.R")
source("R/GREAT_functions.R")

#- get the RvT data
rvt <- getRvT()


#-------------------------------------------------------
#- Plot RAW data

#- plot each light level's temperature response
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
plotBy(Rarea~CTleaf|pot,data=rvt,las=1,type="o",xlim=c(10,35),ylim=c(0,2),legend=F,pch=16,
       axes=F,xlab="",ylab="",col=palette()[rvt$prov])
magaxis(side=1:4,labels=c(0,1,0,1),las=1)
title(main="Raw data",line=-1)

title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(R[dark]~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
#-------------------------------------------------------






#-------------------------------------------------------
#- plot TREATMENT MEANS

rvt.m <- summaryBy(Rarea+Tleaf+CTleaf~TleafFac+prov,data=rvt,FUN=c(mean,standard.error),na.rm=T)

#- plot each light level's temperature response
plotBy(Rarea.mean~CTleaf.mean|prov,data=rvt.m,las=1,xlim=c(10,35),ylim=c(0,2),legend=F,pch=16,
       axes=F)
magaxis(side=1:4,labels=c(0,1,0,1),las=1)
adderrorbars(x=rvt.m$CTleaf.mean,y=rvt.m$Rarea.mean,SE=rvt.m$Rarea.standard.error,direction="updown")
adderrorbars(x=rvt.m$CTleaf.mean,y=rvt.m$Rarea.mean,SE=rvt.m$CTleaf.standard.error,direction="leftright")
plotBy(Rarea.mean~CTleaf.mean|prov,data=rvt.m,las=1,xlim=c(10,35),ylim=c(0,1.5),legend=F,pch=16,
       axes=F,add=T,cex=1.5)
title(main="Provenance means",line=-1)

magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(R[dark]~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")
#-------------------------------------------------------





