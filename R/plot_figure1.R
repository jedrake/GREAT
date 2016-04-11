#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- This plots temperature response curves for final mass, leaf respiration, and photosynthesis
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")






#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- prepare the three datasets, fit temperature response curves in preparation for plotting

#--- Biomass
#- prepare biomass data, fit and plot the first panel
dat <- getHarvest()
size <- getSize()

#- pull out just the unique pot and room numbers from teh size dataframe
size2 <- unique(size[,c("pot","room","Water_trt")])

#- merge pot ids and harvest. Note the pre-treatment plants get excluded here
dat2 <- merge(size2,dat,by.x=c("pot"),by.y="Pot")
key <- data.frame(room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
massdata <- merge(subset(dat2,Water_trt == "wet"),key,by="room")

#- fit final mass
MASSvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=5,Topt=30,theta=5),namey="totdm",namex="Tair",lengthPredict=20)







#--- Asat
#- get the AQ data (i.e., "long-term" photosynthesis dataset)
aq <- getAQ()

Asatdata <- subset(aq,campaign==1 & LightFac==4 & Water_trt=="wet")
Asatdata.l <- split(Asatdata,Asatdata$prov)

#- fit all the curves
AvTfits.list <- lapply(Asatdata.l,FUN=fitAvT)



#----- Respiration
#- get the RvT data
rvt <- getRvT()

rvt.l <- split(rvt,rvt$prov)

#- fit all the curves
RvTfits.list.st <- lapply(rvt.l,FUN=fitRvT)


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- set up the plot
windows(60,30);par(mar=c(6,7,1,1),mfrow=c(1,3),cex.lab=2,cex.axis=1.2,oma=c(2,0,0,0))
COL=palette()[1:3]
xlims <- c(12,40)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




#-------- Make plots

#---- Final mass
#- pull out the predictions and confidence intervals for plotting
mass.pred <- data.frame(do.call(rbind,
                             list(MASSvTfits.l[[1]][[2]],MASSvTfits.l[[2]][[2]],MASSvTfits.l[[3]][[2]])))
mass.pred$prov <- c(rep("A",nrow(mass.pred)/3),rep("B",nrow(mass.pred)/3),rep("C",nrow(mass.pred)/3))


#- plot mass
plotBy(Sim.Mean~Tleaf|prov,data=mass.pred,legend=F,type="l",las=1,ylim=c(0,12),lwd=3,cex.lab=2,xlim=xlims,axes=F,
       ylab=expression(Final~mass~(g)),
       xlab="")
as.m <- subset(mass.pred,prov=="A")
bs.m <- subset(mass.pred,prov=="B")
cs.m <- subset(mass.pred,prov=="C")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
legend("topleft",c("A","B","C"),fill=COL,cex=2,title="Provenance")

#- add TREATMENT MEANS for mass
dat3 <- summaryBy(totdm+Tair~room+prov,FUN=c(mean,standard.error),data=subset(tofit,Water_trt=="wet"),na.rm=T)

plotBy(totdm.mean~Tair.mean|prov,data=dat3,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=dat3$Tair.mean,y=dat3$totdm.mean,
                                 SE=dat3$totdm.standard.error,direction="updown")))


#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.4)
title(xlab=expression(Mean~air~temperature~(degree*C)),cex.lab=2,line=4)
legend("topright",letters[1],bty="n",cex=2)





#---- Asat
#- pull out the predictions and confidence intervals for plotting
Asat.pred <- data.frame(do.call(rbind,
                             list(AvTfits.list[[1]][[2]],AvTfits.list[[2]][[2]],AvTfits.list[[3]][[2]])))
Asat.pred$prov <- c(rep("A",nrow(Asat.pred)/3),rep("B",nrow(Asat.pred)/3),rep("C",nrow(Asat.pred)/3))


plotBy(Sim.Mean~Tleaf|prov,data=Asat.pred,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=2,xlim=xlims,
       ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),axes=F,
       xlab="")
as.asat <- subset(Asat.pred,prov=="A")
bs.asat <- subset(Asat.pred,prov=="B")
cs.asat <- subset(Asat.pred,prov=="C")

polygon(x = c(as.asat$Tleaf, rev(as.asat$Tleaf)), y = c(as.asat$Sim.97.5, rev(as.asat$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.asat$Tleaf, rev(bs.asat$Tleaf)), y = c(bs.asat$Sim.97.5, rev(bs.asat$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.asat$Tleaf, rev(cs.asat$Tleaf)), y = c(cs.asat$Sim.97.5, rev(cs.asat$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)

#- add TREATMENT MEANS
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+Water_trt+prov,data=subset(aq,campaign==1),FUN=c(mean,standard.error))
plotmeans <- subset(aq.m,LightFac==4 & Water_trt=="wet")
plotBy(Photo.mean~Tleaf.mean|prov,data=plotmeans,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Photo.mean,
                                 SE=plotmeans$Photo.standard.error,direction="updown")))

#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.4)
title(xlab=expression(Leaf~temperature~(degree*C)),cex.lab=2,line=4)
legend("topright",letters[2],bty="n",cex=2)





#----- Respiration
#- pull out the predictions and confidence intervals for plotting
r.pred <- data.frame(do.call(rbind,
                             list(RvTfits.list.st[[1]][[2]],RvTfits.list.st[[2]][[2]],RvTfits.list.st[[3]][[2]])))
r.pred$prov <- c(rep("A",nrow(r.pred)/3),rep("B",nrow(r.pred)/3),rep("C",nrow(r.pred)/3))

plotBy(Sim.Mean~Tleaf|prov,data=r.pred,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=2,xlim=xlims,axes=F,
       ylab=expression(R[dark]~(nmol~g^-1~s^-1)),
       xlab="")
as.r <- subset(r.pred,prov=="A")
bs.r <- subset(r.pred,prov=="B")
cs.r <- subset(r.pred,prov=="C")

polygon(x = c(as.r$Tleaf, rev(as.r$Tleaf)), y = c(as.r$Sim.97.5, rev(as.r$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.r$Tleaf, rev(bs.r$Tleaf)), y = c(bs.r$Sim.97.5, rev(bs.r$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.r$Tleaf, rev(cs.r$Tleaf)), y = c(cs.r$Sim.97.5, rev(cs.r$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)

#- add TREATMENT MEANS
rvt.m <- summaryBy(Rarea+Rmass+Tleaf~TleafFac+prov,data=rvt,FUN=c(mean,standard.error))
plotmeans <- rvt.m
plotBy(Rmass.mean~Tleaf.mean|prov,data=plotmeans,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Rmass.mean,
                                 SE=plotmeans$Rmass.standard.error,direction="updown")))

#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.4)
title(xlab=expression(Leaf~temperature~(degree*C)),cex.lab=2,line=4)
legend("topright",letters[3],bty="n",cex=2)


#- export the graph
dev.copy2pdf(file="output/Figure1_growth_Asat_Rdark.pdf")
