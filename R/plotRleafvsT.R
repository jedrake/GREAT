#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- process and plot the leaf respiraiton short-term temperature response curves
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#----- Respiration
#- get the RvT data
rvt <- getRvT()

rvt.l <- split(rvt,rvt$location)

#- fit all the curves
RvTfits.list.st <- lapply(rvt.l,FUN=fitRvT,namex="Tleaf",namey="Rmass")
RvTfits.list.st.area <- lapply(rvt.l,FUN=fitRvT,namex="Tleaf",namey="Rarea")
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- make a plot


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- set up the plot
windows(60,40);par(mar=c(6,8,1,1),mfrow=c(1,2),cex.lab=1.5,cex.axis=1.2,oma=c(2,0,0,0))
palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]
xlims <- c(12,32)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#-------- Make plots

#---- Rmass
#- pull out the predictions and confidence intervals for plotting
R.pred <- data.frame(do.call(rbind,
                                list(RvTfits.list.st[[1]][[2]],RvTfits.list.st[[2]][[2]],RvTfits.list.st[[3]][[2]])))
R.pred$location <- c(rep("Cold-edge",nrow(R.pred)/3),rep("Central",nrow(R.pred)/3),rep("Warm-edge",nrow(R.pred)/3))
R.pred$location <- factor(R.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  

#- plot mass
plotBy(Sim.Mean~Tleaf|location,data=R.pred,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=2,xlim=xlims,axes=F,
       ylab=expression(R[mass]~(nmol~CO[2]~g^-1~s^-1)),col=COL,
       xlab="")
as.m <- subset(R.pred,location=="Cold-edge")
bs.m <- subset(R.pred,location=="Central")
cs.m <- subset(R.pred,location=="Warm-edge")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
#legend("topleft",levels(R.pred$location),fill=COL,cex=1.2,title="Provenance",bty="n")
legend("topleft",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1.2,title="Provenance",bty="n")

#- add TREATMENT MEANS for mass
dat3 <- summaryBy(Rmass+Rarea+Tleaf~Room_meas+location,FUN=c(mean,standard.error),data=rvt,na.rm=T)

plotBy(Rmass.mean~Tleaf.mean|location,data=dat3,add=T,pch=16,cex=2,legend=F,col=COL,
       panel.first=(adderrorbars(x=dat3$Tleaf.mean,y=dat3$Rmass.mean,
                                 SE=dat3$Rmass.standard.error,direction="updown")))

#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.4)
title(xlab=expression(Measurement~T[leaf]~(degree*C)),cex.lab=2,line=4)
legend("topright",letters[1],bty="n",cex=1.5)




#---- Rarea
#- pull out the predictions and confidence intervals for plotting
RA.pred <- data.frame(do.call(rbind,
                             list(RvTfits.list.st.area[[1]][[2]],RvTfits.list.st.area[[2]][[2]],RvTfits.list.st.area[[3]][[2]])))
RA.pred$location <- c(rep("Cold-edge",nrow(RA.pred)/3),rep("Central",nrow(RA.pred)/3),rep("Warm-edge",nrow(RA.pred)/3))
RA.pred$location <- factor(RA.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  

#- plot mass
plotBy(Sim.Mean~Tleaf|location,data=RA.pred,legend=F,type="l",las=1,ylim=c(0,1.5),lwd=3,cex.lab=2,xlim=xlims,axes=F,
       ylab=expression(R[area]~(mu*mol~CO[2]~m^-2~s^-1)),col=COL,
       xlab="")
as.m <- subset(RA.pred,location=="Cold-edge")
bs.m <- subset(RA.pred,location=="Central")
cs.m <- subset(RA.pred,location=="Warm-edge")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
#legend("topleft",levels(R.pred$location),fill=COL,cex=1.2,title="Provenance",bty="n")


plotBy(Rarea.mean~Tleaf.mean|location,data=dat3,add=T,pch=16,cex=2,legend=F,col=COL,
       panel.first=(adderrorbars(x=dat3$Tleaf.mean,y=dat3$Rarea.mean,
                                 SE=dat3$Rarea.standard.error,direction="updown")))


#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.4)
title(xlab=expression(Measurement~T[leaf]~(degree*C)),cex.lab=2,line=4)
legend("topright",letters[2],bty="n",cex=1.5)


dev.copy2pdf(file="output/FigureS5-RvsT.pdf")
#-----------------------------------------------------------------------------------------