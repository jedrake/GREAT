#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Growth analysis script focusing on the 11-day growth INVERVAL. 
#       returnRGR() does most of the heavy data-manipulation work
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.

dat$LAR <- dat$LAR/1000 # convert back to m2 g-1
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- fit RGR, LAR, and NAR v T to estimate Topts
tofit <- subset(dat,W_treatment=="w") # not needed, as this is done in returnRGR() now.
tofit.l <- split(tofit,tofit$location)

#- fit AGR and RGR T response curves
RGRvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.12,Topt=25,theta=12),namey="RGR",namex="Tair",lengthPredict=20)
LARvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.020,Topt=30,theta=5),namey="LAR",namex="Tair",lengthPredict=20)


#- fit NAR response curves
NARfits.l <- list()
for(i in 1:length(tofit.l)){
  NARfits.l[[i]] <- lm(NAR ~Tair,data=tofit.l[[i]])
  
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF~Room,FUN=c(mean,standard.error),data=subset(dat,W_treatment=="w"),na.rm=T)
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- Make a 3-panel plot showing RGR, LAR, and NAR relative to growth temperature.
pdf(file="output/Figure5_RGR_LAR_NAR_interval.pdf",width=3.5,height=8)

#windows(30,60)
par(mfrow=c(3,1),mar=c(3,4,1,1),oma=c(5,4,1,1))
palette(rev(brewer.pal(6,"Spectral")))
ptsize <- 1.5
COL <- palette()[c(1,2,6)]
xlims <- c(17,37)
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF~Room+Tair+location,FUN=c(mean,standard.error),data=subset(dat,W_treatment=="w"),na.rm=T)


#------------
#- plot the First panel (RGR vs. Temperature)
RGRplot <- data.frame(do.call(rbind,
                              list(RGRvTfits.l[[1]][[2]],RGRvTfits.l[[2]][[2]],RGRvTfits.l[[3]][[2]])))
RGRplot$prov <- c(rep("A",nrow(RGRplot)/3),rep("B",nrow(RGRplot)/3),rep("C",nrow(RGRplot)/3))
RGRplot$location <- c(rep("Cold-edge",nrow(RGRplot)/3),rep("Warm-edge",nrow(RGRplot)/3),rep("Central",nrow(RGRplot)/3))
RGRplot$location <- factor(RGRplot$location,levels=c("Cold-edge","Central","Warm-edge"))  

plotBy(Sim.Mean~Tleaf|location,data=RGRplot,legend=F,type="l",las=1,xlim=xlims,ylim=c(0,0.15),lwd=3,cex.lab=2,col=COL,
       ylab="",axes=F,
       xlab="")
as.rgr <- subset(RGRplot,prov=="A")
bs.rgr <- subset(RGRplot,prov=="B")
cs.rgr <- subset(RGRplot,prov=="C")

polygon(x = c(as.rgr$Tleaf, rev(as.rgr$Tleaf)), y = c(as.rgr$Sim.97.5., rev(as.rgr$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.rgr$Tleaf, rev(bs.rgr$Tleaf)), y = c(bs.rgr$Sim.97.5., rev(bs.rgr$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.rgr$Tleaf, rev(cs.rgr$Tleaf)), y = c(cs.rgr$Sim.97.5., rev(cs.rgr$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)


#- add TREATMENT MEANS
plotBy(RGR.mean~Tair|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,legendwhere="bottomleft",pch=16,
       axes=F,xlab="",ylab="",cex=ptsize,col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$RGR.mean,SE=dat2$RGR.standard.error,direction="updown"))
palette(COL) 
points(RGR.mean~Tair,data=dat2,add=T,pch=21,cex=2,legend=F,col="black",bg=location)


magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2,minorn=2,ratio=0.25)
axis(side=1,at=c(20,25,30,35),labels=T,tick=F,cex.axis=2)
legend("topright",letters[1],bty="n",cex=1.8)
legend("bottomleft",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1.2,title="Provenance",bty="n")

#legend("bottomleft",levels(dat2$location),fill=COL,cex=1.2,title="",bty="n")


#------------
#- plot the Second panel (LAR vs. Temperature)

LARplot <- data.frame(do.call(rbind,
                              list(LARvTfits.l[[1]][[2]],LARvTfits.l[[2]][[2]],LARvTfits.l[[3]][[2]])))
#LARplot$prov <- c(rep("A",nrow(LARplot)/3),rep("B",nrow(LARplot)/3),rep("C",nrow(LARplot)/3))
LARplot$location <- c(rep("Cold-edge",nrow(LARplot)/3),rep("Central",nrow(LARplot)/3),rep("Warm-edge",nrow(LARplot)/3))
LARplot$location <- factor(LARplot$location,levels=c("Cold-edge","Central","Warm-edge"))  

plotBy(Sim.Mean~Tleaf|location,data=LARplot,legend=F,type="l",las=1,xlim=xlims,ylim=c(0,0.03),lwd=3,cex.lab=2,col=COL,
       ylab="",axes=F,
       xlab="")
cold.rgr <- subset(LARplot,location=="Cold-edge")
central.rgr <- subset(LARplot,location=="Central")
warm.rgr <- subset(LARplot,location=="Warm-edge")

polygon(x = c(cold.rgr$Tleaf, rev(cold.rgr$Tleaf)), y = c(cold.rgr$Sim.97.5., rev(cold.rgr$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(central.rgr$Tleaf, rev(central.rgr$Tleaf)), y = c(central.rgr$Sim.97.5., rev(central.rgr$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(warm.rgr$Tleaf, rev(warm.rgr$Tleaf)), y = c(warm.rgr$Sim.97.5., rev(warm.rgr$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)


#- add TREATMENT MEANS
plotBy(LAR.mean~Tair|location,data=dat2,las=1,xlim=c(17,37),legend=F,pch=16,
       axes=F,xlab="",ylab="",cex=ptsize,col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$LAR.mean,SE=dat2$LAR.standard.error,direction="updown"))
points(LAR.mean~Tair,data=dat2,add=T,pch=21,cex=2,legend=F,col="black",bg=location)
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2,ratio=0.25,majorn=3)
axis(side=1,at=c(20,25,30,35),labels=T,tick=F,cex.axis=2)
legend("topright",letters[2],bty="n",cex=1.8)





#-----------------------------------------------------------------------------------------
#- NAR
plotBy(NAR.mean~Tair|location,data=dat2,las=1,xlim=xlims,ylim=c(0,8),type="n",legend=F,axes=F,ylab="")
predline(NARfits.l[[1]],col=alpha(COL[1],0.5))
predline(NARfits.l[[2]],col=alpha(COL[2],0.5))
predline(NARfits.l[[3]],col=alpha(COL[3],0.5))
plotBy(NAR.mean~Tair|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,10),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$NAR.mean,SE=dat2$NAR.standard.error,direction="updown"))
points(NAR.mean~Tair,data=dat2,add=T,pch=21,cex=2,legend=F,col="black",bg=location)

magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2,ratio=0.25)
axis(side=1,at=c(20,25,30,35),labels=T,tick=F,cex.axis=2)

legend("topright",letters[3],bty="n",cex=1.8)
#-----------------------------------------------------------------------------------------



#- add axis labels
title(ylab=expression(RGR~(g~g^-1~d^-1)),outer=T,adj=0.95,cex.lab=2,line=0.5)
title(ylab=expression(LAR~(m^2~g^-1)),outer=T,adj=0.5,cex.lab=2,line=0.5)
title(ylab=expression(NAR~(g~m^-2~d^-1)),outer=T,adj=0.1,cex.lab=2,line=0.5)
title(xlab=expression(Growth~T[air]~(degree*C)),outer=T,adj=0.6,cex.lab=2,line=2)

#------------

dev.off()
#dev.copy2pdf(file="output/Figure5_RGR_LAR_NAR_interval.pdf")
