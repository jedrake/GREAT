library(plotBy)
library(doBy)
library(magicaxis)
library(rgl)
source("R/generic_functions.R")
source("R/GREAT_functions.R")

#- read in the respiration data
path <- "W://WORKING_DATA/GHS39/GREAT"
Rdat1 <- read.csv(paste(path,"/Share/Data/GasEx/R/GREAT-R-compiled-20160217-20160224-L1.csv",sep=""))

#- average over subreplicate logs
Rdat2 <- summaryBy(dCO2+Photo+Cond+Ci+Trmmol+VpdL+Area~Date+Organ+Pot+Unit,data=Rdat1,FUN=mean,keep.names=T)

#- reformat the "Pot" variable. E.g, change "A-01" to "A-1" to match the size datasets.
crap <- unlist(strsplit(as.character(Rdat2$Pot), "-"))
Rdat2$prov <- as.factor(crap[seq(1,length(crap)-1,2)])
Rdat2$pot <- as.factor(paste(Rdat2$prov,as.numeric(crap[seq(0,length(crap),2)]),sep="-"))
Rdat2$prov <- NULL #- remove the "prov" variable. It gets added later with getSize().
Rdat2$Pot <- NULL

#------------------------------------------------------------------------------------------------
#- merge in the harvest mass data, to calculate mass-specific respiration rates
finalHarvest <- read.csv(paste(path,"/Share/Data/Harvests/GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L1.csv",sep=""))
finalHarvest <- finalHarvest[,c("Code","LA_sub","leafsubdm","stemdm","rootdm","rootdm_measured")]
Rdat3 <- merge(Rdat2,finalHarvest,by.x=c("pot"),by.y=c("Code"))

#- calculate mass-based respiration rates
Rdat3$Rmass <- NA
leaves <- which(Rdat3$Organ=="leaf") #- find the indexes of the leaf measurements
stems <- which(Rdat3$Organ=="stem") #- find the indexes of the stem measurements
roots <- which(Rdat3$Organ=="root") #- find the indexes of the root measurements

#- sort out the samples where the measured root mass was not different than the root total mass
rootstofix <- which(is.na(Rdat3$rootdm_measured)==T & is.na(Rdat3$rootdm)==F)
Rdat3$rootdm_measured[rootstofix] <- Rdat3$rootdm[rootstofix]  

Rdat3$Rmass[leaves] <- Rdat3$Photo[leaves]*-1*Rdat3$LA_sub[leaves]/10000/Rdat3$leafsubdm[leaves]*1000*1000 # nmol CO2 g-1 s-1
Rdat3$Rmass[stems] <- Rdat3$Photo[stems]*-1*Rdat3$Area[stems]/10000/Rdat3$stemdm[stems]*1000*1000 # nmol CO2 g-1 s-1
Rdat3$Rmass[roots] <- Rdat3$Photo[roots]*-1*Rdat3$Area[roots]/10000/Rdat3$rootdm_measured[roots]*1000*1000 # nmol CO2 g-1 s-1

#------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------
#- merge in the size data (just to get things like room numbers and drought treatments, etc)
size <- getSize()
size <- size[,c("pot","room","prov","Water_trt","prov_trt")]
size <- unique(size)

#- merge in room temperature key
key <- data.frame(room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
size2 <- merge(size,key)
Rdat <- merge(Rdat3,size2,by=c("pot"))
Rdat$Date <- as.Date(as.character(Rdat$Date),format="%Y%m%d")
Rdat$Rarea <- -1*Rdat$Photo

#- predict respiration at growth temperature
Rdat$Rmass_insitu <- with(Rdat,Rmass*2^((Tair-25)/10))
Rdat$Rarea_insitu <- with(Rdat,Rarea*2^((Tair-25)/10))

#- average across provenances in each room
Rdat.m <- summaryBy(Rarea+Rmass+Rmass_insitu+Rarea_insitu~room+Tair+prov+Water_trt+Organ,data=Rdat,FUN=c(mean,standard.error),na.rm=T)
#Rdat.m$Rarea.standard.error[which(is.na(Rdat.m$Rarea.standard.error))] <- 0
#------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#- plot treatment average respiration rates
toplot <- subset(Rdat.m,Water_trt=="wet")
toplot$room <- as.numeric(toplot$room)

windows(40,70);par(mfrow=c(4,1),mar=c(2,7,1,0),oma=c(3,1,3,2),cex.lab=2)

#- Rarea for leaves
plotBy(Rarea.mean~Tair|prov,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,2),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rarea.mean,
                                SE=subset(toplot,Organ=="leaf")$Rarea.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[area],
                                   (mu*mol~CO[2]~m^-2~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",legend="Leaf",bty="n",cex=2)
legend(x=22,y=2.7,xpd=NA,legend=c("A","B","C"),pch=16,col=palette()[1:3],ncol=3,bty="n",cex=1.5)
#- Rmass for leaves
plotBy(Rmass.mean~Tair|prov,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rmass.mean,
                                SE=subset(toplot,Organ=="leaf")$Rmass.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",legend="Leaf",bty="n",cex=2)

#- Rmass for stems
plotBy(Rmass.mean~Tair|prov,data=subset(toplot,Organ=="stem"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="stem")$Tair,
                                y=subset(toplot,Organ=="stem")$Rmass.mean,
                                SE=subset(toplot,Organ=="stem")$Rmass.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",legend="Stem",bty="n",cex=2)


#- Rmass for roots
plotBy(Rmass.mean~Tair|prov,data=subset(toplot,Organ=="root"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="root")$Tair,
                                y=subset(toplot,Organ=="root")$Rmass.mean,
                                SE=subset(toplot,Organ=="root")$Rmass.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",legend="root",bty="n",cex=2)
title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.7,line=1)
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/Rcomponents.pdf")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------








#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#- plot treatment average respiration rates, estimated for in-situ conditions

windows(40,70);par(mfrow=c(4,1),mar=c(2,7,1,0),oma=c(3,1,3,2),cex.lab=2)

#- Rarea for leaves
plotBy(Rarea_insitu.mean~Tair|prov,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,2),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rarea_insitu.mean,
                                SE=subset(toplot,Organ=="leaf")$Rarea_insitu.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[area],
                                   (mu*mol~CO[2]~m^-2~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topleft",legend="Leaf",bty="n",cex=2)
legend(x=22,y=2.7,xpd=NA,legend=c("A","B","C"),pch=16,col=palette()[1:3],ncol=3,bty="n",cex=1.5)
#- Rmass for leaves
plotBy(Rmass_insitu.mean~Tair|prov,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rmass_insitu.mean,
                                SE=subset(toplot,Organ=="leaf")$Rmass_insitu.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topleft",legend="Leaf",bty="n",cex=2)

#- Rmass for stems
plotBy(Rmass_insitu.mean~Tair|prov,data=subset(toplot,Organ=="stem"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="stem")$Tair,
                                y=subset(toplot,Organ=="stem")$Rmass_insitu.mean,
                                SE=subset(toplot,Organ=="stem")$Rmass_insitu.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topleft",legend="Stem",bty="n",cex=2)


#- Rmass for roots
plotBy(Rmass_insitu.mean~Tair|prov,data=subset(toplot,Organ=="root"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
       panel.first=adderrorbars(x=subset(toplot,Organ=="root")$Tair,
                                y=subset(toplot,Organ=="root")$Rmass_insitu.mean,
                                SE=subset(toplot,Organ=="root")$Rmass_insitu.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topleft",legend="root",bty="n",cex=2)
title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.7,line=1)
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/Rcomponents_insituRates.pdf")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------