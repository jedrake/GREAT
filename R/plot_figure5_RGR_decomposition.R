#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Code to make the RGR decomposition figure (SLA, LMF, and NAR)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")


#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.




#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data, and plot temperature response curves
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+Tair~room+prov+location,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)


#-----------------------------------------------------------------------------------------
#- plot temperature response curves for NAR, LMF, and SLA

windows(30,60);par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]

#- SLA
plotBy(SLA.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(250,500),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$SLA.mean,SE=dat2$SLA.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
legend("bottomright",levels(dat2$location),title="Provenance",col=COL[1:3],pch=16,bg="white",cex=1.4)

#- LMF
plotBy(LMF.mean~Tair.mean|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$LMF.mean,SE=dat2$LMF.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)

#- NAR
plotBy(NAR.mean~Tair.mean|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,10),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$NAR.mean,SE=dat2$NAR.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)




title(xlab=expression(T[air]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(SLA~(cm^2~g^-1)),outer=T,cex.lab=2,adj=0.95)
title(ylab=expression(LMF~(g~g^-1)),outer=T,cex.lab=2,adj=0.5)
title(ylab=expression(NAR~(g~m^-2~d^-1)),outer=T,cex.lab=2,adj=0.1)
dev.copy2pdf(file="output/NAR_LMF_SLA.pdf")
