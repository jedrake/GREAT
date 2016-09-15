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
#- Fit T-response curves for SLA


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

#-----
#- new bit to calculate the INCREMENT in leaf number
leaf_increment <- summaryBy(Leafno~Room+location+Tair+Date+Code,data=subset(la,W_treatment=="w"),
                            FUN=c(sum),keep.names=T,na.rm=T)
leaf_increment.l <- split(leaf_increment,leaf_increment$Code)

#- loop over each code, calculate the difference in leaf number, add to new dataframe
newleaves <- data.frame(Room=rep(NA,length(leaf_increment.l)),location=rep(NA,length(leaf_increment.l)),
                        Tair=rep(NA,length(leaf_increment.l)),Code=rep(NA,length(leaf_increment.l)),
                        leaf_inc=rep(NA,length(leaf_increment.l)))
for (i in 1:length(leaf_increment.l)){
  dat <- leaf_increment.l[[i]]
  dat <- dat[with(dat,order(Date)),]
  
  newleaves$Room[i] <- as.character(dat$Room[1])
  newleaves$location[i] <- as.character(dat$location[1])
  newleaves$Tair[i] <- dat$Tair[1]
  newleaves$Code[i] <- as.character(dat$Code[1])
  newleaves$leaf_inc[i] <- dat$Leafno[2] - dat$Leafno[1]
}
newleaves2 <- subset(newleaves,leaf_inc>=0) # exclude the trees that were harvested (and thus have no leaves on the second date)

newleaves.m <- summaryBy(leaf_inc+Tair~Room+location,data=newleaves2,
                      FUN=c(mean,standard.error),keep.names=F,na.rm=T)
#-----




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





#- fit T-response of leaf increment
newleaves2.l <- split(newleaves2,newleaves2$location)

#- fit all the curves
NLfits.l <- lapply(newleaves2.l,FUN=fitJuneT,namex="Tair",namey="leaf_inc",lengthPredict=21,start=list(Rref=20,Topt=25,theta=10))

NL.pred <- data.frame(do.call(rbind,
                              list(NLfits.l[[1]][[2]],NLfits.l[[2]][[2]],NLfits.l[[3]][[2]])))
NL.pred$location <- c(rep("Cold-edge",nrow(NL.pred)/3),rep("Central",nrow(NL.pred)/3),rep("Warm-edge",nrow(NL.pred)/3))
NL.pred$location <- factor(NL.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  
#----------------------------------------------------------------------------------------------------















#-----------------------------------------------------------------------------------------
#- plot temperature response curves for SLA, LMF, leafsize, and leaf number

windows(48,30);par(mfrow=c(2,2),mar=c(2,6,1,1),oma=c(3,3,1,2))
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
plotBy(SLA.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(250,500),legend=F,pch=16,cex=0.5,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$SLA.mean,SE=dat2$SLA.standard.error,direction="updown"))
palette(COL) 
points(SLA.mean~Tair.mean,data=dat2,add=T,pch=21,cex=2,legend=F,col="black",bg=location)

magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
legend("bottomright",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1.2,title="Provenance",bty="n")
#legend("bottomright",levels(dat2$location),title="Provenance",col=COL[1:3],pch=16,bg="white",cex=1.4)
title(ylab=expression(SLA~(cm^2~g^-1)),outer=F,cex.lab=2)
legend("topleft",letters[1],bty="n",cex=1.2)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- LMF
plotBy(LMF.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.6),type="n",legend=F,axes=F,ylab="",xlab="")
predline(LMFfits.l[[1]],col=alpha(COL[1],0.5))
predline(LMFfits.l[[2]],col=alpha(COL[2],0.5))
predline(LMFfits.l[[3]],col=alpha(COL[3],0.5))
plotBy(LMF.mean~Tair.mean|location,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.6),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=dat2$Tair.mean,y=dat2$LMF.mean,SE=dat2$LMF.standard.error,direction="updown"))

palette(COL) 
points(LMF.mean~Tair.mean,data=dat2,add=T,pch=21,cex=2,legend=F,col="black",bg=location)


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

palette(COL) 
points(Leafarea.mean~Tair.mean,data=leaf_size,add=T,pch=21,cex=2,legend=F,col="black",bg=location)


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
title(ylab=expression(Leaf~size~(cm^2)),outer=F,cex.lab=2)
legend("topleft",letters[3],bty="n",cex=1.2)

#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- plot leaf number INCREMENT
plotBy(Sim.Mean~Tleaf|location,data=NL.pred,legend=F,type="l",las=1,ylim=c(0,20),lwd=3,cex.lab=2,xlim=c(17,37),axes=F,
       ylab="",col=COL,
       xlab="")
as.m <- subset(NL.pred,location=="Cold-edge")
bs.m <- subset(NL.pred,location=="Central")
cs.m <- subset(NL.pred,location=="Warm-edge")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
#legend("topleft",levels(SLA.pred$location),fill=COL,cex=1.7,title="Provenance")

#- Leaf size
plotBy(leaf_inc.mean~Tair.mean|location,data=newleaves.m,las=1,legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",col=COL,add=T,
       panel.first=adderrorbars(x=newleaves.m$Tair.mean,y=newleaves.m$leaf_inc.mean,SE=newleaves.m$leaf_inc.standard.error,direction="updown"))

palette(COL) 
points(leaf_inc.mean~Tair.mean,data=newleaves.m,add=T,pch=21,cex=2,legend=F,col="black",bg=as.factor(location))

magaxis(side=1:4,labels=c(0,1,0,0),las=1)
magaxis(side=1,labels=c(1),las=1)
title(ylab=expression(Leaf~growth~("#"~"11-days"^-1)),outer=F,cex.lab=2)
legend("topleft",letters[4],bty="n",cex=1.2)

#-----------------------------------------------------------------------------------------



# 
# #-----------------------------------------------------------------------------------------
# #- leaf number
# plotBy(Leafno.mean~Tair.mean|location,data=leaf_no2,las=1,xlim=c(17,37),ylim=c(0,20),type="n",legend=F,axes=F,ylab="",xlab="")
# predline(LNfits.l[[1]],col=alpha(COL[1],0.5))
# predline(LNfits.l[[2]],col=alpha(COL[2],0.5))
# predline(LNfits.l[[3]],col=alpha(COL[3],0.5))
# plotBy(Leafno.mean~Tair.mean|location,data=leaf_no2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,cex=2,
#        axes=F,xlab="",ylab="",col=COL,add=T,
#        panel.first=adderrorbars(x=leaf_no2$Tair.mean,y=leaf_no2$Leafno.mean,SE=leaf_no2$Leafno.standard.error,direction="updown"))
# 
# 
# magaxis(side=1:4,labels=c(0,1,0,0),las=1)
# magaxis(side=1,labels=c(1),las=1)
# title(ylab=expression(Leaf~number~("#")),outer=F,cex.lab=2)
# legend("topleft",letters[4],bty="n",cex=1.2)
# 
# #-----------------------------------------------------------------------------------------


title(xlab=expression(Growth~T[air]~(degree*C)),outer=T,cex.lab=2,adj=0.25,line=1)
title(xlab=expression(Growth~T[air]~(degree*C)),outer=T,cex.lab=2,adj=0.85,line=1)

dev.copy2pdf(file="output/Figure4_leaves.pdf")


















#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Are leaf size and SLA correlated?

#- process the total plant leaf area data
la.raw <- getLA()

#- work out the air temperature and new provenance keys
key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
key2 <- data.frame(Prov=as.factor(LETTERS[1:3]),location= factor(c("Cold-edge","Warm-edge","Central"),
                                                                 levels=c("Cold-edge","Central","Warm-edge")))
la1 <- merge(la.raw,key,by="Room")
la <- merge(la1,key2,by="Prov")
la$Date[which(la$Date==as.Date("2016-01-28"))] <- as.Date("2016-01-29") # add one to the leaf area dates to facilitate merging

#- pull out just the large leaves
la.large <- subset(la,Leafsize=="large")

#-- get the SLA data
SLA <- getSLA()[[1]] # these are the punches of the SAME plants measured for leaf size



#- merge SLA and leaf size datasets
dat2 <- merge(SLA,la.large,by=c("Room","Prov","Code","W_treatment","Date","prov_trt"))

plot(SLA~Leafarea,data=dat2,xlab="leaf size (cm2)",ylab="SLA")

#- average across groups
dat2.m <- summaryBy(SLA+Leafarea~Tair,data=subset(dat2,W_treatment=="w"),FUN=mean,keep.names=T,na.rm=T)


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
