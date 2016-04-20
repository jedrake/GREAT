#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Growth analysis script focusing on the 11-day growth INVERVAL. 
#       returnRGR() does most of the heavy data-manipulation work
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- fit AGR and RGR v T to estimate Topts
tofit <- subset(dat,Water_trt=="wet")
tofit.l <- split(tofit,tofit$location)

#- fit AGR and RGR T response curves
AGRvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.5,Topt=30,theta=5),namey="AGR",namex="Tair",lengthPredict=20)
RGRvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.15,Topt=25,theta=20),namey="RGR",namex="Tair",lengthPredict=20)
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+canopy~room,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- Make a 6-panel plot showing AGR and RGR relative to growth temperature, total crown leaf area,
#    and total plant mass.

windows(60,50);par(mfrow=c(2,3),mar=c(1,4,1,1),oma=c(9,7,1,1))
palette(rev(brewer.pal(6,"Spectral")))
ptsize <- 1.5
COL <- palette()[c(1,2,6)]
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+canopy+logmass+totmass~room+Tair+location,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)


#--- plot AGR vs. T (panel 1)
#- pull out the predictions and confidence intervals for plotting
AGRplot <- data.frame(do.call(rbind,
                             list(AGRvTfits.l[[1]][[2]],AGRvTfits.l[[2]][[2]],AGRvTfits.l[[3]][[2]])))
AGRplot$prov <- c(rep("A",nrow(AGRplot)/3),rep("B",nrow(AGRplot)/3),rep("C",nrow(AGRplot)/3))
AGRplot$location <- c(rep("Cold-edge",nrow(AGRplot)/3),rep("Warm-edge",nrow(AGRplot)/3),rep("Central",nrow(AGRplot)/3))
AGRplot$location <- factor(AGRplot$location,levels=c("Cold-edge","Central","Warm-edge"))  

plotBy(Sim.Mean~Tleaf|prov,data=AGRplot,legend=F,type="l",las=1,ylim=c(0,0.5),lwd=3,cex.lab=2,col=COL,
       ylab="",axes=F,
       xlab="")
as.agr <- subset(AGRplot,prov=="A")
bs.agr <- subset(AGRplot,prov=="B")
cs.agr <- subset(AGRplot,prov=="C")

polygon(x = c(as.agr$Tleaf, rev(as.agr$Tleaf)), y = c(as.agr$Sim.97.5., rev(as.agr$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.agr$Tleaf, rev(bs.agr$Tleaf)), y = c(bs.agr$Sim.97.5., rev(bs.agr$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.agr$Tleaf, rev(cs.agr$Tleaf)), y = c(cs.agr$Sim.97.5., rev(cs.agr$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)


#- add TREATMENT MEANS
plotBy(AGR.mean~Tair|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,
       axes=F,xlab="",ylab="",cex=ptsize,col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$AGR.mean,SE=dat2$AGR.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2)
legend("bottomright",levels(dat2$location),fill=COL,cex=1.5,title="Provenance",bty="n")
legend("topleft",letters[1],bty="n",cex=1.8)
#--



#------------
#--- plot the second panel and third panels (AGR vs. leaf area and total mass)
dat3 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+canopy+logmass+totmass~Tair,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)

#- AGR vs. canopy leaf area
plotBy(AGR~canopy|Tair,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,ylim=c(0,0.5),
       xlab="",ylab="",cex.lab=2,legend=F,axes=F)
adderrorbars(x=dat3$canopy.mean,y=dat3$AGR.mean,SE=dat3$AGR.standard.error,direction="updown")
adderrorbars(x=dat3$canopy.mean,y=dat3$AGR.mean,SE=dat3$canopy.standard.error,direction="leftright")
plotBy(AGR.mean~canopy.mean|Tair,data=dat3,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3,legend=F)
arrows(x0=dat3$canopy.mean[1:5],x1=dat3$canopy.mean[2:6],y0=dat3$AGR.mean[1:5],y1=dat3$AGR.mean[2:6])
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2)
legend("bottomright",levels(as.factor(dat2$Tair)),fill=rev(brewer.pal(6,"Spectral")),cex=1.5,title="Temperature",bty="n")
legend("topleft",letters[3],bty="n",cex=1.8)


#- AGR vs. mass
plotBy(AGR~totmass|Tair,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,legend=F,
       xlim=c(0,9),
       xlab="",ylab="",axes=F)
adderrorbars(x=dat3$totmass.mean,y=dat3$AGR.mean,SE=dat3$AGR.standard.error,direction="updown")
adderrorbars(x=dat3$totmass.mean,y=dat3$AGR.mean,SE=dat3$totmass.standard.error,direction="leftright")
plotBy(AGR.mean~totmass.mean|Tair,data=dat3,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3,legend=F)
arrows(x0=dat3$totmass.mean[1:5],x1=dat3$totmass.mean[2:6],y0=dat3$AGR.mean[1:5],y1=dat3$AGR.mean[2:6])
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2)
legend("topleft",letters[5],bty="n",cex=1.8)

#------------




#------------
#- plot the fourth panel (RGR vs. Temperature)
RGRplot <- data.frame(do.call(rbind,
                              list(RGRvTfits.l[[1]][[2]],RGRvTfits.l[[2]][[2]],RGRvTfits.l[[3]][[2]])))
RGRplot$prov <- c(rep("A",nrow(RGRplot)/3),rep("B",nrow(RGRplot)/3),rep("C",nrow(RGRplot)/3))
RGRplot$location <- c(rep("Cold-edge",nrow(RGRplot)/3),rep("Warm-edge",nrow(RGRplot)/3),rep("Central",nrow(RGRplot)/3))
RGRplot$location <- factor(RGRplot$location,levels=c("Cold-edge","Central","Warm-edge"))  

plotBy(Sim.Mean~Tleaf|prov,data=RGRplot,legend=F,type="l",las=1,ylim=c(0,0.17),lwd=3,cex.lab=2,col=COL,
       ylab="",axes=F,
       xlab="")
as.rgr <- subset(RGRplot,prov=="A")
bs.rgr <- subset(RGRplot,prov=="B")
cs.rgr <- subset(RGRplot,prov=="C")

polygon(x = c(as.rgr$Tleaf, rev(as.rgr$Tleaf)), y = c(as.rgr$Sim.97.5., rev(as.rgr$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.rgr$Tleaf, rev(bs.rgr$Tleaf)), y = c(bs.rgr$Sim.97.5., rev(bs.rgr$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.rgr$Tleaf, rev(cs.rgr$Tleaf)), y = c(cs.rgr$Sim.97.5., rev(cs.rgr$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)


#- add TREATMENT MEANS
plotBy(RGR.mean~Tair|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,
       axes=F,xlab="",ylab="",cex=ptsize,col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$RGR.mean,SE=dat2$RGR.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2)
axis(side=1,at=c(20,25,30,35),labels=T,tick=F,cex.axis=2)
legend("topleft",letters[2],bty="n",cex=1.8)



#------------
#--- plot the fifth panel and sixth panels (RGR vs. leaf area and total mass)

#- RGR vs. canopy leaf area
plotBy(RGR~canopy|Tair,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,
       xlab="",ylab="",cex.lab=2,axes=F,ylim=c(0,0.17),legend=F,
       legendwhere="bottomright")
adderrorbars(x=dat3$canopy.mean,y=dat3$RGR.mean,SE=dat3$RGR.standard.error,direction="updown")
adderrorbars(x=dat3$canopy.mean,y=dat3$RGR.mean,SE=dat3$canopy.standard.error,direction="leftright")
plotBy(RGR.mean~canopy.mean|Tair,data=dat3,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3,legend=F)
arrows(x0=dat3$canopy.mean[1:5],x1=dat3$canopy.mean[2:6],y0=dat3$RGR.mean[1:5],y1=dat3$RGR.mean[2:6])
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2)
axis(side=1,at=c(0,500,1000,1500),labels=T,tick=F,cex.axis=2,las=2)
legend("topleft",letters[4],bty="n",cex=1.8)

#- RGR vs. mass
plotBy(RGR~totmass|Tair,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,legend=F,ylim=c(0,0.17),
       xlim=c(0,9),
       xlab="",ylab="",cex.lab=2,axes=F)
adderrorbars(x=dat3$totmass.mean,y=dat3$RGR.mean,SE=dat3$RGR.standard.error,direction="updown")
adderrorbars(x=dat3$totmass.mean,y=dat3$RGR.mean,SE=dat3$totmass.standard.error,direction="leftright")
plotBy(RGR.mean~totmass.mean|Tair,data=dat3,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3,legend=F)
arrows(x0=dat3$totmass.mean[1:5],x1=dat3$totmass.mean[2:6],y0=dat3$RGR.mean[1:5],y1=dat3$RGR.mean[2:6])
magaxis(side=1:4,labels=c(0,1,0,0),las=1,cex.axis=2)
axis(side=1,at=c(0,2,4,6,8),labels=T,tick=F,cex.axis=2)
legend("topleft",letters[6],bty="n",cex=1.8)



#- add axis labels
title(ylab=expression(AGR~(g~d^-1)),outer=T,adj=0.85,cex.lab=3,line=2)
title(ylab=expression(RGR~(g~g^-1~d^-1)),outer=T,adj=0.15,cex.lab=3,line=2)
title(xlab=expression(T[growth]~(degree*C)),outer=T,adj=0.1,cex.lab=3,line=7)
title(xlab=expression(Leaf~area~(cm^2)),outer=T,adj=0.55,cex.lab=3,line=7)
title(xlab=expression(Mass~(g)),outer=T,adj=0.95,cex.lab=3,line=7)

#------------


dev.copy2pdf(file="output/Figure2_AGR_RGR_interval.pdf")
