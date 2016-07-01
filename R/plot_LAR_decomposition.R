#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Code to make the LAR decomposition figure, th (SLA, LMF, average leaf size, and leaf number)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.




#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data, and plot temperature response curves
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+Tair~Room+Prov+location,FUN=c(mean,standard.error),data=subset(dat,W_treatment=="w"),na.rm=T)
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Fit T-response curves for SLA, LMF, and NAR


#-----------------------------------------------------------------------------------------
#- T-response of SLA
dat.wet <- subset(dat,W_treatment=="w")
dat.l <- split(dat.wet,dat.wet$location)

#- fit all the curves
SLAfits.l <- lapply(dat.l,FUN=fitJuneT,namex="Tair",namey="SLA",lengthPredict=21,start=list(Rref=450,Topt=30,theta=20))

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

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- process the leaf size data, fit T-response curves

#- process the total plant leaf area data
la.raw <- getLA()

#- work out the air temperature and new provenance keys
key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                 levels=c("Cold-edge","Central","Warm-edge")))
la1 <- merge(la.raw,key,by="Room")
la <- merge(la1,key2,by="Prov")

#- calculate total plant leaf area. This method uses a different average leaf size for each plant
la$canopy <- with(la,Leafno*Leafarea)


#- calculate the average size of "large" leaves, along with the total number of leaves
leaf_size <- summaryBy(Leafarea+Leafno+Tair~Room+location,data=subset(la,Leafsize=="large" & Date==as.Date("2016-01-28")),
                       FUN=c(mean,standard.error),keep.names=F,na.rm=T)

leaf_no <- summaryBy(Leafno~Room+location+Tair+Date+Code,data=subset(la,W_treatment=="w" & Date==as.Date("2016-01-28")),
                     FUN=c(sum),keep.names=T,na.rm=T)
leaf_no2 <- summaryBy(Leafno+Tair~Room+location,data=subset(leaf_no,Date==as.Date("2016-01-28")),
                      FUN=c(mean,standard.error),keep.names=F,na.rm=T)


#- fit T-response of leaf size
la.wet <- subset(la,W_treatment=="w" & Leafsize=="large" & Date==as.Date("2016-01-28"))
la.l <- split(la.wet,la.wet$location)

#- fit all the curves
LSfits.l <- lapply(la.l,FUN=fitJuneT,namex="Tair",namey="Leafarea",lengthPredict=21,start=list(Rref=30,Topt=25,theta=10))

LS.pred <- data.frame(do.call(rbind,
                               list(LSfits.l[[1]][[2]],LSfits.l[[2]][[2]],LSfits.l[[3]][[2]])))
LS.pred$location <- c(rep("Cold-edge",nrow(LS.pred)/3),rep("Central",nrow(LS.pred)/3),rep("Warm-edge",nrow(LS.pred)/3))
LS.pred$location <- factor(LS.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  
#----------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- T-response of leaf number. This list gets called below in the plotting 
leaf_no.l <- split(leaf_no,leaf_no$location)
LNfits.l <- list()
for(i in 1:length(leaf_no.l)){
  LNfits.l[[i]] <- lm(Leafno ~Tair,data=leaf_no.l[[i]])
  
}

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
















#-----------------------------------------------------------------------------------------
#- plot temperature response curves for SLA, LMF, leafsize, and leaf number

windows(30,30);par(mfrow=c(2,2),mar=c(2,6,1,1),oma=c(3,3,1,2))
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
title(ylab=expression(SLA~(cm^2~g^-1)),outer=F,cex.lab=2)
legend("topleft",letters[1],bty="n",cex=1.2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- LMF
plotBy(LMF.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),type="n",legend=F,axes=F,ylab="",xlab="")
predline(LMFfits.l[[1]],col=alpha(COL[1],0.5))
predline(LMFfits.l[[2]],col=alpha(COL[2],0.5))
predline(LMFfits.l[[3]],col=alpha(COL[3],0.5))
plotBy(LMF.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$LMF.mean,SE=dat2$LMF.standard.error,direction="updown"))


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
title(ylab=expression(LMF~(g~g^-1)),outer=F,cex.lab=2)
legend("topleft",letters[2],bty="n",cex=1.2)

#-----------------------------------------------------------------------------------------








#-----------------------------------------------------------------------------------------
#- plot leafsize
plotBy(Sim.Mean~Tleaf|location,data=LS.pred,legend=F,type="l",las=1,ylim=c(0,50),lwd=3,cex.lab=2,xlim=c(17,37),axes=F,
       ylab="",col=COL,
       xlab="")
as.m <- subset(LS.pred,location=="Cold-edge")
bs.m <- subset(LS.pred,location=="Central")
cs.m <- subset(LS.pred,location=="Warm-edge")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
#legend("topleft",levels(SLA.pred$location),fill=COL,cex=1.7,title="Provenance")

#- Leaf size
plotBy(Leafarea.mean~Tair.mean|location,data=leaf_size,las=1,legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=leaf_size$Tair.mean,y=leaf_size$Leafarea.mean,SE=leaf_size$Leafarea.standard.error,direction="updown"))


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
title(ylab=expression(Leaf~size~(cm^2)),outer=F,cex.lab=2)
legend("topleft",letters[3],bty="n",cex=1.2)

#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#- leaf number
plotBy(Leafno.mean~Tair.mean|location,data=leaf_no2,las=1,xlim=c(17,37),ylim=c(0,20),type="n",legend=F,axes=F,ylab="",xlab="")
predline(LNfits.l[[1]],col=alpha(COL[1],0.5))
predline(LNfits.l[[2]],col=alpha(COL[2],0.5))
predline(LNfits.l[[3]],col=alpha(COL[3],0.5))
plotBy(Leafno.mean~Tair.mean|location,data=leaf_no2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=leaf_no2$Tair.mean,y=leaf_no2$Leafno.mean,SE=leaf_no2$Leafno.standard.error,direction="updown"))


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
title(ylab=expression(Leaf~number~("#")),outer=F,cex.lab=2)
legend("topleft",letters[4],bty="n",cex=1.2)

#-----------------------------------------------------------------------------------------


title(xlab=expression(Growth~T[air]~(degree*C)),outer=T,cex.lab=2,adj=0.6,line=1)

dev.copy2pdf(file="output/Figure4_leaves.pdf")
