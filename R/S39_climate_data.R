library(HIEv)
library(stringr)
library(dplyr)
library(plantecophys)
library(plotBy)
library(doBy)
library(reshape2)
source("R/generic_functions.R")

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Script to read, process, and plot the climate data from the S39 glasshouse
#-- The data consist of "fast" data recorded every minute (PAR, Tair, and RH),
#--    and "slow" data recorded every 15-minutes (soil VWC)
#-- This script reads and processes them separately.
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- get the "fast" files. This takes a little while, as the files are huge.
fastfiles <- list.files("R://WORKING_DATA/GHS39/GREAT/Share/Data/climate/s39climate20160226/",pattern="fast",full.names=T)

dat <- list()
for(i in 1:length(fastfiles)){
  #- read in the data
  dat[[i]] <- readTOA5(fastfiles[i])
  
  #- extract the room number from the filename
  name <- tolower(fastfiles[i])
  dat[[i]]$bay <- as.factor(substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5))
}
dat.fast.all <- do.call(rbind,dat)

#- subset to rooms 3-8 and after Jan 8th 2016.
dat.fast <- subset(dat.fast.all,DateTime > as.POSIXct("2016-1-8 00:00:00") & bay %in% 3:8)

#- calculate VPD
dat.fast$VPD_Avg <- RHtoVPD(RH=dat.fast$RH_Avg,TdegC=dat.fast$Tair_Avg)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- process "fast" data

#- hourly averages
dat.fast$DateTime_hr <- as.POSIXct(round.POSIXt(dat.fast$DateTime,units="hours"))


#- create hourly averages
dat.fast.hr <- dplyr::summarize(group_by(dat.fast,DateTime_hr,bay),
                           BattV_Min=mean(BattV_Min,na.rm=T),
                           Tair=mean(Tair_Avg,na.rm=T),
                           RH=mean(RH_Avg,na.rm=T),
                           VPD=mean(VPD_Avg,na.rm=T),
                           PAR=mean(PAR_Avg,na.rm=T))
dat.fast.hr <- as.data.frame(dat.fast.hr)
dat.fast.hr$Date <- as.Date(dat.fast.hr$DateTime_hr)



#-----------------------------------------------------------------------------------------
#- make a conversion table between bay and room numbers, which depends on date (rooms were rotated)
lookup <- expand.grid(bay=3:8,
                      Date=seq.Date(from=min(dat.fast.hr$Date),to=max(dat.fast.hr$Date),by=1),
                      room=NA)
lookup$room[which(lookup$bay==3)] <- ifelse(lookup$Date[which(lookup$bay==3)] < as.Date("2016-1-21"),1,2)
lookup$room[which(lookup$bay==4)] <- ifelse(lookup$Date[which(lookup$bay==4)] < as.Date("2016-1-21"),2,5)
lookup$room[which(lookup$bay==5)] <- ifelse(lookup$Date[which(lookup$bay==5)] < as.Date("2016-1-21"),3,6)
lookup$room[which(lookup$bay==6)] <- ifelse(lookup$Date[which(lookup$bay==6)] < as.Date("2016-1-21"),4,1)
lookup$room[which(lookup$bay==7)] <- ifelse(lookup$Date[which(lookup$bay==7)] < as.Date("2016-1-21"),5,4)
lookup$room[which(lookup$bay==8)] <- ifelse(lookup$Date[which(lookup$bay==8)] < as.Date("2016-1-21"),6,3)

dat.fast.hr <- merge(dat.fast.hr,lookup,by=c("Date","bay"))
dat.fast.hr$room <- factor(dat.fast.hr$room)
dat.fast.hr <- dat.fast.hr[!(dat.fast.hr$Date %in% c(as.Date("2016-1-20"),as.Date("2016-1-21"))),] #- remove dates of rotation
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- Averages, only after 13 Jan (humidifier fixed in bay 8)
summaryBy(Tair+RH+VPD~room,data=subset(dat.fast.hr,DateTime_hr>=as.POSIXct("2016-1-15 00:00:00")))
#-----------------------------------------------------------------------------------------


#- plot hourly data
windows(40,70);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(6,7,1,4))

plotBy(Tair~DateTime_hr|room,data=dat.fast.hr,legend=F,type="l",ylim=c(15,45),lwd=2,las=1)
axis(4,labels=T,las=1)
axis.POSIXct(side=1,at=seq.POSIXt(from=min(dat.fast.hr$DateTime_hr),to=max(dat.fast.hr$DateTime_hr),by="day"),
             labels=F)
legend("top",legend=levels(dat.fast.hr$room),col=palette()[1:6],lty=1,ncol=6,bty="n",lwd=2)
plotBy(RH~DateTime_hr|room,data=dat.fast.hr,legend=F,type="l",ylim=c(25,120),lwd=2,las=1)
axis(4,labels=T,las=1)
axis.POSIXct(side=1,at=seq.POSIXt(from=min(dat.fast.hr$DateTime_hr),to=max(dat.fast.hr$DateTime_hr),by="day"),
             labels=F)
plotBy(VPD~DateTime_hr|room,data=dat.fast.hr,legend=F,type="l",lwd=2,ylim=c(0,7),las=1)
axis(4,labels=T,las=1)
axis.POSIXct(side=1,at=seq.POSIXt(from=min(dat.fast.hr$DateTime_hr),to=max(dat.fast.hr$DateTime_hr),by="day"),
             labels=F)
plotBy(PAR~DateTime_hr,data=subset(dat.fast.hr,bay %in% 3:8),col="gray",legend=F,type="l",lwd=2,ylim=c(0,2000),las=1)
axis(4,labels=T,las=1)
axis.POSIXct(side=1,at=seq.POSIXt(from=min(dat.fast.hr$DateTime_hr),to=max(dat.fast.hr$DateTime_hr),by="day"),
             labels=T)
title(ylab=expression(T[air]~(degree*C)),outer=T,adj=0.9,line=3,cex.lab=2)
title(ylab=expression(RH~("%")),outer=T,adj=0.65,line=3,cex.lab=2)
title(ylab=expression(VPD~(kPa)),outer=T,adj=0.35,line=3,cex.lab=2)
title(ylab=expression(PAR),outer=T,adj=0.1,line=3,cex.lab=2)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
















#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- read in the VWC data ("slow")

#- get the vwc files. 
vwc.files <- list.files("R://WORKING_DATA/GHS39/GREAT/Share/Data/climate/s39climate20160226/",pattern="VW",full.names=T)

dat <- list()
for(i in 1:length(vwc.files)){
  #- read in the data
  dat[[i]] <- readTOA5(vwc.files[i])
  
  #- extract the room number from the filename
  name <- tolower(vwc.files[i])
  dat[[i]]$bay <- as.numeric((substr(str_extract(name,pattern="room[0-9]"),start=5,stop=5)))
}
dat.vwc.all <- do.call(rbind,dat)

#- subset to rooms 3-8 and after Jan 14th 2016. Not all probes were install prior to this
dat.vwc1 <- subset(dat.vwc.all,DateTime > as.POSIXct("2016-1-14 00:00:00",tz="GMT") & bay %in% 3:8)[,c(1,3:10,27,29)]
dat.vwc  <- melt(dat.vwc1,id.vars=c("DateTime","Date","bay"))
dat.vwc$id <- substr(dat.vwc$variable,start=8,stop=8)
dat.vwc$variable <- NULL
names(dat.vwc)[4] <- "VWC"
dat.vwc$Water_treatment <- ifelse(dat.vwc$id >=5,"dry","wet")
dat.vwc$bay <- factor(dat.vwc$bay)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- make a conversion table between bay and room numbers, which depends on date (rooms were rotated)
lookup <- expand.grid(bay=3:8,
                      Date=seq.Date(from=min(dat.vwc$Date),to=max(dat.vwc$Date),by=1),
                      room=NA)
lookup$room[which(lookup$bay==3)] <- ifelse(lookup$Date[which(lookup$bay==3)] < as.Date("2016-1-21"),1,2)
lookup$room[which(lookup$bay==4)] <- ifelse(lookup$Date[which(lookup$bay==4)] < as.Date("2016-1-21"),2,5)
lookup$room[which(lookup$bay==5)] <- ifelse(lookup$Date[which(lookup$bay==5)] < as.Date("2016-1-21"),3,6)
lookup$room[which(lookup$bay==6)] <- ifelse(lookup$Date[which(lookup$bay==6)] < as.Date("2016-1-21"),4,1)
lookup$room[which(lookup$bay==7)] <- ifelse(lookup$Date[which(lookup$bay==7)] < as.Date("2016-1-21"),5,4)
lookup$room[which(lookup$bay==8)] <- ifelse(lookup$Date[which(lookup$bay==8)] < as.Date("2016-1-21"),6,3)

dat.vwc <- merge(dat.vwc,lookup,by=c("Date","bay"))
dat.vwc$room <- factor(dat.vwc$room)
dat.vwc <- dat.vwc[!(dat.vwc$Date %in% c(as.Date("2016-1-20"),as.Date("2016-1-21"))),]
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- plot soil moisture over time
dat.vwc.l <- split(dat.vwc,dat.vwc$room)

windows(40,70);par(mfrow=c(6,1),mar=c(0,0,0,0),oma=c(6,7,1,4))
for (i in 1:length(dat.vwc.l)){
  toplot <- dat.vwc.l[[i]]
  
  plotBy(VWC~DateTime|id,data=toplot,type="o",lwd=2,col=c("blue","blue","blue","blue","red","red","red","red"),
         legend=F,ylim=c(0,0.3),las=1)
  axis(side=4,labels=T,las=1)
  legend("bottomright",paste("Room",toplot$room[1]),bty="n",xpd=NA)
  axis.POSIXct(side=1,at=seq.POSIXt(from=min(dat.vwc$DateTime),to=max(dat.vwc$DateTime),by="day"),
               labels=F)
}
title(ylab=expression(VWC~(m^3~m^-3)),xlab="Date",
      outer=T,cex.lab=3)
axis.POSIXct(side=1,at=seq.POSIXt(from=min(dat.vwc$DateTime),to=max(dat.vwc$DateTime),by="day"),
             labels=T)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- process vwcdata to daily averages

#- hourly averages
dat.vwc$Date <- as.Date(dat.vwc$DateTime)


#- create daily averages
dat.vwc.d <- dplyr::summarize(group_by(dat.vwc,Date,room,Water_treatment,id),
                              VWC=mean(VWC,na.rm=T))
dat.vwc.d <- as.data.frame(dat.vwc.d)

dat.vwc.d2 <- summaryBy(VWC~Date+room+Water_treatment,data=dat.vwc.d,FUN=c(mean,standard.error))


#- plot treatment averages for soil moisture over time
dat.vwc.l <- split(dat.vwc.d2,dat.vwc.d2$room)
windows(40,70);par(mfrow=c(6,1),mar=c(0,0,0,0),oma=c(6,7,1,4))
for (i in 1:length(dat.vwc.l)){
  toplot <- dat.vwc.l[[i]]
  
  plotBy(VWC.mean~Date|Water_treatment,data=toplot,type="o",lwd=2,col=c("red","blue"),
         legend=F,ylim=c(0,0.3),las=1,
         panel.first=adderrorbars(x=toplot$Date,y=toplot$VWC.mean,SE=toplot$VWC.standard.error,
                                  direction="updown",col=c("red","blue")))
  axis(side=4,labels=T,las=1)
  legend("bottomright",paste("Room",toplot$room[1]),bty="n",xpd=NA)
  axis.Date(side=1,at=seq.Date(from=min(dat.vwc.d2$Date),to=max(dat.vwc.d2$Date),by="day"),labels=F)
}
title(ylab=expression(VWC~(m^3~m^-3)),xlab="Date",
      outer=T,cex.lab=3)
axis.Date(side=1,at=seq.Date(from=min(dat.vwc.d2$Date),to=max(dat.vwc.d2$Date),by="day"),labels=T)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------



