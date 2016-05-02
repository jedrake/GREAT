#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Code to make the RGR decomposition figure (SLA, LMF, and NAR)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

source("R/gamplotfunctions.R")

#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.




#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data, and plot temperature response curves
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+Tair~room+prov+location,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Fit T-response curves for SLA, LMF, and NAR


#-----------------------------------------------------------------------------------------
#- T-response of SLA

dat.l <- split(dat,dat$location)

#- fit all the curves
SLAfits.l <- lapply(dat.l,FUN=fitJuneT,namex="Tair",namey="SLA",lengthPredict=51,start=list(Rref=450,Topt=30,theta=20))

SLA.pred <- data.frame(do.call(rbind,
                                list(SLAfits.l[[1]][[2]],SLAfits.l[[2]][[2]],SLAfits.l[[3]][[2]])))
SLA.pred$prov <- c(rep("A",nrow(SLA.pred)/3),rep("B",nrow(SLA.pred)/3),rep("C",nrow(SLA.pred)/3))
SLA.pred$location <- c(rep("Cold-edge",nrow(SLA.pred)/3),rep("Warm-edge",nrow(SLA.pred)/3),rep("Central",nrow(SLA.pred)/3))
SLA.pred$location <- factor(SLA.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- T-response of LMF. This list gets called below in the plotting of LMF
LMFfits.l <- list()
for(i in 1:length(dat.l)){
  LMFfits.l[[i]] <- lm(LMF ~Tair,data=dat.l[[i]])
  
}

NARfits.l <- list()
for(i in 1:length(dat.l)){
  NARfits.l[[i]] <- lm(NAR ~Tair,data=dat.l[[i]])
  
}
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#- plot temperature response curves for NAR, LMF, and SLA

windows(30,60);par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]


#-----------------------------------------------------------------------------------------
#- plot SLA
plotBy(Sim.Mean~Tleaf|location,data=SLA.pred,legend=F,type="l",las=1,ylim=c(250,500),lwd=3,cex.lab=2,xlim=c(17,37),axes=F,
       ylab="",
       xlab="")
as.m <- subset(SLA.pred,prov=="A")
bs.m <- subset(SLA.pred,prov=="B")
cs.m <- subset(SLA.pred,prov=="C")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
#legend("topleft",levels(SLA.pred$location),fill=COL,cex=1.7,title="Provenance")

#- SLA
plotBy(SLA.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(250,500),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$SLA.mean,SE=dat2$SLA.standard.error,direction="updown"))


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
legend("bottomright",levels(dat2$location),title="Provenance",col=COL[1:3],pch=16,bg="white",cex=1.4)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- LMF
plotBy(LMF.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),type="n",legend=F,axes=F)
predline(LMFfits.l[[1]],col=alpha(COL[1],0.5))
predline(LMFfits.l[[2]],col=alpha(COL[2],0.5))
predline(LMFfits.l[[3]],col=alpha(COL[3],0.5))
plotBy(LMF.mean~Tair.mean|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$LMF.mean,SE=dat2$LMF.standard.error,direction="updown"))


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#- NAR
plotBy(NAR.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,10),type="n",legend=F,axes=F)
predline(NARfits.l[[1]],col=alpha(COL[1],0.5))
predline(NARfits.l[[2]],col=alpha(COL[2],0.5))
predline(NARfits.l[[3]],col=alpha(COL[3],0.5))
plotBy(NAR.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,10),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$NAR.mean,SE=dat2$NAR.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
#-----------------------------------------------------------------------------------------



title(xlab=expression(T[air]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(SLA~(cm^2~g^-1)),outer=T,cex.lab=2,adj=0.95)
title(ylab=expression(LMF~(g~g^-1)),outer=T,cex.lab=2,adj=0.5)
title(ylab=expression(NAR~(g~m^-2~d^-1)),outer=T,cex.lab=2,adj=0.1)
dev.copy2pdf(file="output/NAR_LMF_SLA.pdf")
