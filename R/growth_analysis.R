library(plotBy)
library(doBy)
library(magicaxis)
source("R/generic_functions.R")
source("R/GREAT_functions.R")


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Script to read, process, and plot the plant size and leaf area data
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#--- double check possible error for Bd-78. It seems too large on 2016-01-28!

#-----------------------------------------------------------------------------------------
#- get the size data, estimate mass

hddata <- getSize() # specify path to "GREAT" share folder on HIE-Data2. Defaults to W://WORKING_DATA/GHS39/GREAT
hddata$totmass <- returnMassFromAllom(d2hdat=hddata$d2h,plotson=T) #- predict mass from d2h allometry
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#- calculate RGR and AGR based on the estimated total mass
hddata <- hddata[with(hddata,order(pot,Date)),]

hddata.l <- split(hddata,hddata$pot)
for(i in 1:length(hddata.l)){
  crap <- hddata.l[[i]]
  hddata.l[[i]]$RGR <- c(NA,diff(log(hddata.l[[i]]$totmass)))/c(NA,diff(hddata.l[[i]]$Date)) # g g-1 day-1
  hddata.l[[i]]$AGR <- c(NA,diff(hddata.l[[i]]$totmass))/c(NA,diff(hddata.l[[i]]$Date))      # g day-1
  
}
hddata2 <- do.call(rbind,hddata.l)

#- 3d plots
library(rgl)
plot3d(x=hddata2$Date,y=hddata2$room,z=hddata2$rgr,col=palette()[hddata2$prov],size=12)
plot3d(x=hddata2$Date,y=hddata2$room,z=hddata2$agr,col=palette()[hddata2$prov],size=12)


#- pull out the most important agr and rgr estimates (during our growth interval from Jan 28th to Feb 8th)
rgrinterval1 <- subset(hddata2,Date==as.Date("2016-02-08"))
rgrinterval <- rgrinterval1[complete.cases(rgrinterval1),] # remove missing data
#-----------------------------------------------------------------------------------------


# 
# #------------------------------------------------------------------------------------------------------------
# #- average across rooms, plot
# hddata.m <- summaryBy(diam+h+d2h+totmass~room+prov+Date,data=subset(hddata,Water_trt=="wet"),FUN=c(mean,standard.error), na.rm=T)
# hddata.l <- split(hddata.m,hddata.m$room)
# 
# #- plot d2h
# windows(30,60);par(mfrow=c(6,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
# ylims=c(0,50)
# for(i in 1:length(hddata.l)){
#   toplot <- hddata.l[[i]]
#   plotBy(d2h.mean~Date|prov,data=toplot,type="o",ylim=ylims,pch=16,legend=F,yaxt="n",xaxt="n",cex=1.5,
#          panel.first=adderrorbars(x=toplot$Date,y=toplot$d2h.mean,SE=toplot$d2h.standard.error,direction="updown"))
#   legend("top",paste("room",toplot$room[1]),bty="n")
#   if(i<6)axis.Date(side=1,at=seq.Date(from=min(toplot$Date),to=max(toplot$Date),by=1),labels=F)
#   if(i==6)axis.Date(side=1,at=seq.Date(from=min(toplot$Date),to=max(toplot$Date),by=1),labels=T)
#   magaxis(side=c(2,4),labels=c(1,1),las=1)
#   if(i==1)legend("topleft",c("A","B","C"),pch=16,col=c("black","red","green3"))
#   title(ylab=expression(d^2*h~(cm^3)),outer=T,adj=0.5,cex.lab=3)
# }
# 
# #- 3d plots
# library(rgl)
# plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$h.m,col=palette()[hddata.m$prov],size=12)
# plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$diam.m,col=palette()[hddata.m$prov],size=12)
# plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$d2h.m,col=palette()[hddata.m$prov],size=12)
# plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$totmass.m,col=palette()[hddata.m$prov],size=12)
# 
# #------------------------------------------------------------------------------------------------------------
# 
# 
# 
# #------------------------------------------------------------------------------------------------------------
# #- get and plot the growth increments and RGR estimates of d2h, ending on a specified date
# focaldate <- max(hddata$Date)
# hddata2.m <- summaryBy(diam+h+d2h~room+Date,data=subset(hddata,Water_trt=="wet"),FUN=c(mean),keep.names=T, na.rm=T)
# hddata2.m$dh <-c(NA,diff(hddata2.m$h)) 
# hddata2.m$ddiam <-c(NA,diff(hddata2.m$diam)) 
# hddata2.m$dd2h <-c(NA,diff(hddata2.m$d2h))
# hddata2.m$dDate <-c(NA,diff(hddata2.m$Date)) 
# 
# hddata2.m$rgr_d2h <-c(NA,diff(log(hddata2.m$d2h)))/hddata2.m$dDate # no longer assumes a 10-day interval
# 
# #- plot growth increments
# windows(40,60);par(mfrow=c(3,1),mar=c(5,7,1,1),oma=c(0,0,0,0),cex.lab=2)
# plot(dh~as.numeric(room),data=subset(hddata2.m,Date==focaldate),ylim=c(0,30),xlab="room",ylab="dHeight (cm)",pch=16,cex=2)
# plot(ddiam~as.numeric(room),data=subset(hddata2.m,Date==focaldate),ylim=c(0,2),xlab="room",ylab="dDiameter (mm)",pch=16,cex=2)
# plot(dd2h~as.numeric(room),data=subset(hddata2.m,Date==focaldate),ylim=c(0,10),xlab="room",ylab="dD2h (cm3)",pch=16,cex=2)
# 
# #- plot RGR over time. Ontogenetic drift is already apparent.
# rgrdat <- subset(hddata2.m,Date>as.Date("2016-01-10"))
# plot3d(x=rgrdat$Date,y=rgrdat$room,z=rgrdat$rgr_d2h,size=12)
# 
# #------------------------------------------------------------------------------------------------------------
# 
# 
# 
# 
# #------------------------------------------------------------------------------------------------------------
# #- get and plot the growth increments for DRY VS WET in B, ending on a specified date
# #- This code will have to change when new RGR estimates are available, as the diff() function will return more values.
# focaldate <- max(hddata$Date)
# hddata3.m <- summaryBy(diam+h+d2h~room+Water_trt+Date,data=subset(hddata,prov=="B"),FUN=c(mean),keep.names=T, na.rm=T)
# hddata3.m$dh <-c(NA,diff(hddata3.m$h)) 
# hddata3.m$ddiam <-c(NA,diff(hddata3.m$diam)) 
# hddata3.m$dd2h <-c(NA,diff(hddata3.m$d2h)) 
# toplot <- subset(hddata3.m,Date==focaldate)
# 
# windows(40,60);par(mfrow=c(3,1),mar=c(5,7,1,1),oma=c(0,0,0,0),cex.lab=2)
# with(toplot,plot(dh~as.numeric(room),col=Water_trt,ylim=c(0,30),xlab="room",ylab="dHeight (cm)",pch=16,cex=2))
# legend("topleft",pch=16,col=c("black","red"),legend=c("wet","dry"),cex=1.5)
# with(toplot,plot(ddiam~as.numeric(room),col=Water_trt,ylim=c(0,2),xlab="room",ylab="dDiameter (mm)",pch=16,cex=2))
# with(toplot,plot(dd2h~as.numeric(room),col=Water_trt,ylim=c(0,10),xlab="room",ylab="dD2h (cm3)",pch=16,cex=2))
# #------------------------------------------------------------------------------------------------------------









#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#- merge in the measurements of total plant leaf area to the RGR interval

#- process the total plant leaf area data
la <- getLA()

#- calculate total plant leaf area. This method uses a different average leaf size for each plant
la$canopy <- with(la,leaf_no*lf_area)


#----------------------------------------------------------------------------------------------------
#- calculate total plant leaf area using a room and date-specific mean value
leaf_size <- summaryBy(lf_area~room+lf_size+Date,data=la,FUN=mean,keep.names=F,na.rm=T)


#-- average leaf size is temperature dependent, but not provenance dependent
boxplot(lf_area~lf_size+room,data=la)
boxplot(lf_area~prov+room,data=subset(la,lf_size=="large"))

la2.1 <- merge(la,leaf_size,by=c("room","lf_size","Date"))
la2.1$canopy2 <- with(la2.1,leaf_no*lf_area.mean)

la2 <- summaryBy(canopy+canopy2~room+pot+prov+prov_trt+Date+Water_trt,data=la2.1,FUN=sum,keep.names=T)

#- merge in total plant leaf number
leaf_no <-summaryBy(leaf_no~pot,data=la,FUN=sum,keep.names=T)
la2 <- merge(la2,leaf_no,by="pot")

#- average the leaf area data across the two dates, which bracket the RGR growth interval
la.m <- summaryBy(canopy+canopy2+leaf_no~room+pot+prov+prov_trt+Water_trt,data=la2,FUN=mean,keep.names=T)


#- merge total plant leaf area with tree size from the interval measurements
la3 <- merge(la.m,subset(rgrinterval,Date %in% as.Date(c("2016-1-28","2016-02-08"))),by=c("room","prov","prov_trt","pot","Water_trt"))
la3 <- la3[complete.cases(la3),]
la3$logLA <- with(la3,log10(canopy))
la3$logd2h <- with(la3,log10(d2h))

#- plot log-log relation between d2h and leaf area, compare to allometry from GLAHD
windows()
plotBy(logLA~logd2h|Date,data=la3)
abline(a=1.889,b=0.7687) # allometry from GLAHD
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------







#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#---- get the SLA data from punches. SLA increase with temperature, to a point.
sla <- getPunches()

rgrdat <- merge(la3,sla,by=c("room","prov","pot","prov_trt","Water_trt"))
rgrdat$Date.x <- rgrdat$Date.y <- rgrdat$punch_no <- rgrdat$area_cm2 <- rgrdat$mass_mg <- NULL
# 
# sla.m <- summaryBy(SLA+LMA~room+prov+Date,data=subset(sla,Water_trt=="wet"),FUN=c(mean,standard.error),keep.names=T, na.rm=T)
# 
# #- plot provenance means
# windows(40,30);par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
# plotBy(SLA.mean~as.numeric(room)|prov,data=sla.m,type="o",ylim=c(200,600),pch=16,legend=F,yaxt="n",xaxt="n",cex=1.5,
#        ylab="",xlab="",
#        panel.first=adderrorbars(x=as.numeric(sla.m$room),y=sla.m$SLA.mean,SE=sla.m$SLA.standard.error,direction="updown"))
# magaxis(side=c(1,2,3,4),labels=c(1,1,0,0),las=1)
# legend("topleft",c("A","B","C"),pch=16,col=c("black","red","green3"),cex=2)
# title(ylab=expression(SLA~(cm^2~g^-1)),outer=T,adj=0.5,cex.lab=3)
# title(xlab="Room",outer=T,cex.lab=3)
# 
# #- dry vs. wet
# sla.m.dry <- summaryBy(SLA+LMA~room+prov+Date+Water_trt,data=subset(sla,prov=="B"),FUN=c(mean,standard.error),keep.names=T, na.rm=T)
# windows(40,30);par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
# plotBy(SLA.mean~as.numeric(room)|Water_trt,data=sla.m.dry,type="o",ylim=c(200,600),pch=16,legend=F,yaxt="n",xaxt="n",cex=1.5,
#        ylab="",xlab="",
#        panel.first=adderrorbars(x=as.numeric(sla.m.dry$room),y=sla.m.dry$SLA.mean,SE=sla.m.dry$SLA.standard.error,direction="updown"))
# magaxis(side=c(1,2,3,4),labels=c(1,1,0,0),las=1)
# legend("topleft",c("Wet","Dry"),pch=16,col=c("black","red"),cex=2)
# title(ylab=expression(SLA~(cm^2~g^-1)),outer=T,adj=0.5,cex.lab=3)
# title(xlab="Room",outer=T,cex.lab=3)

#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------








#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#- okay, so NOW we have a dataframe with all the bits in it for a formal RGR analysis. 
#    We have growth, canopy leaf area, and SLA. These were measured INDEPENDENTLY.
head(rgrdat)

#- calculate LAR (m2 kg-1)
rgrdat$LAR <- with(rgrdat,canopy/10000/(totmass/1000))

#- calculate net assimilation rate (NAR; g m-2 d-1) from absolute growth rate and total canopy leaf area
rgrdat$NAR <- with(rgrdat,AGR/(canopy/10000))

pairs(subset(rgrdat,Water_trt=="wet")[,c("RGR","AGR","canopy","LAR","NAR","SLA","room")],
      col=palette()[subset(rgrdat,Water_trt=="wet")$prov])
#--------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

