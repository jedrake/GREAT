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







#-------------------------------------------------------
#-------------------------------------------------------
#- fit RvT to estimate Q10 and Rref, for the DIRECT 
#   short term fits only!
tofit <- rvt
tofit.l <- split(tofit,tofit$prov)

#- fit all the curves
RvTfits.list.st <- lapply(tofit.l,FUN=fitRvT)


#- pull out the parameter means and SE's for plotting
RvTfits <- data.frame(do.call(rbind,
                              list(RvTfits.list.st[[1]][[1]],RvTfits.list.st[[2]][[1]],RvTfits.list.st[[3]][[1]])))

windows(30,50);par(mfrow=c(2,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
#- plot Rref (at 22.5 degC)
barplot2(height=RvTfits$Rref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,1),las=1,
         ylab=expression(R[ref]~(mu*mol~m^-2~s^-1)),
         ci.l=RvTfits$Rref-RvTfits$Rref.se,ci.u=RvTfits$Rref+RvTfits$Rref.se)

#- plot Q10
barplot2(height=RvTfits$Q10,names.arg=c("A","B","C"),plot.ci=T,las=1,ylim=c(0,2.5),
         ylab=expression(Q[10]),
         ci.l=RvTfits$Q10-RvTfits$Q10.se,ci.u=RvTfits$Q10+RvTfits$Q10.se)
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/RleafvT_area_fits.pdf")


#- pull out the predictions and confidence intervals for plotting
toplot <- data.frame(do.call(rbind,
                             list(RvTfits.list.st[[1]][[2]],RvTfits.list.st[[2]][[2]],RvTfits.list.st[[3]][[2]])))
toplot$prov <- c(rep("A",51),rep("B",51),rep("C",51))

windows(30,30);par(mar=c(5,7,1,1))
COL=palette()[1:3]

plotBy(Sim.Mean~Tleaf|prov,data=toplot,legend=F,type="l",las=1,ylim=c(0,1.75),lwd=3,cex.lab=2,
       ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),
       xlab=expression(T[leaf]~(degree*C)))
as <- subset(toplot,prov=="A")
bs <- subset(toplot,prov=="B")
cs <- subset(toplot,prov=="C")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5, rev(as$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5, rev(bs$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5, rev(cs$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)
legend("topleft",c("A","B","C"),fill=COL,cex=2,title="Provenance")

#- add TREATMENT MEANS
rvt.m <- summaryBy(Rarea+Tleaf~TleafFac+prov,data=rvt,FUN=c(mean,standard.error))
plotmeans <- rvt.m
plotBy(Rarea.mean~Tleaf.mean|prov,data=plotmeans,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Rarea.mean,
                                 SE=plotmeans$Rarea.standard.error,direction="updown")))
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/RleafvT_predictions_area.pdf")

#-------------------------------------------------------
#-------------------------------------------------------




