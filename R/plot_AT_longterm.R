#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- This plots temperature response curves forlight-saturated photosynthetic rates
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------




#--- Asat
#- get the AQ data (i.e., "long-term" photosynthesis dataset)
aq <- getAQ()

Asatdata <- subset(aq,campaign==1 & LightFac==4 & W_treatment=="w")
Asatdata.l <- split(Asatdata,Asatdata$location)

#- fit all the curves
AvTfits.list <- lapply(Asatdata.l,FUN=fitAvT)



#---- Final mass
#- pull out the predictions and confidence intervals for plotting
A.pred <- data.frame(do.call(rbind,
                                list(AvTfits.list[[1]][[2]],AvTfits.list[[2]][[2]],AvTfits.list[[3]][[2]])))
A.pred$location <- c(rep("Cold-edge",nrow(A.pred)/3),rep("Central",nrow(A.pred)/3),rep("Warm-edge",nrow(A.pred)/3))
A.pred$location <- factor(A.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  

windows();par(mar=c(6,7,1,1))

#- plot Asat
plotBy(Sim.Mean~Tleaf|location,data=A.pred,legend=F,type="l",las=1,xlim=c(18,42),ylim=c(0,30),lwd=3,cex.lab=2,axes=F,
       ylab="",col=COL,
       xlab="")
as.m <- subset(A.pred,location=="Cold-edge")
bs.m <- subset(A.pred,location=="Central")
cs.m <- subset(A.pred,location=="Warm-edge")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
legend("bottomleft",levels(A.pred$location),fill=COL,cex=1.2,title="Provenance",bty="n")

#- add TREATMENT MEANS for mass
dat3 <- summaryBy(Photo+Tair+Tleaf~Room+location,FUN=c(mean,standard.error),data=Asatdata,na.rm=T)

adderrorbars(x=dat3$Tleaf.mean,y=dat3$Photo.mean,SE=dat3$Photo.standard.error,direction="updown")
adderrorbars(x=dat3$Tleaf.mean,y=dat3$Photo.mean,SE=dat3$Tleaf.standard.error,direction="leftright")

plotBy(Photo.mean~Tleaf.mean|location,data=dat3,add=T,pch=16,cex=2,legend=F,col=COL)


#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.4)
title(xlab=expression(Measurement~T[leaf]~(degree*C)),cex.lab=2,line=4)
title(ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),cex.lab=2,line=2)

