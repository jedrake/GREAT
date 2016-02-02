library(plotBy)
library(doBy)
library(magicaxis)
source("R/generic_functions.R")


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Script to read, process, and plot the plant size and leaf area data
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#- find the most recent file
files <-  list.files("W://WORKING_DATA/GHS39/GREAT/Share/Data/Height&Diam",pattern="HEIGHT&DIAMETER",full.names=T)
files2 <- files[grep(".csv",files)]
dates <- c()
for (i in 1:length(files2)){
  dates[i] <- as.numeric(substr(files2[i],start=100,stop=102)) # gets the month and date as a single number
}

#- read data, plot size over time
hddata <- read.csv(files2[which.max(dates)])
hddata$prov <- as.factor(substr(hddata$pot,start=1,stop=1))
hddata$room <- as.factor(hddata$room)
hddata$prov_trt <- as.factor(paste(hddata$prov,hddata$room,sep="-"))
hddata$diam <- with(hddata,((d1+d2)/2))
hddata$d2h <- with(hddata,(diam/10)^2*h) #cm^3
hddata$Date <- as.Date(hddata$date,format="%d/%m/%Y")

#- assign drought treatments
hddata$Water_trt <- "wet"
hddata$Water_trt[grep("Bd",hddata$pot)] <- "dry"
hddata$Water_trt <- factor(hddata$Water_trt,levels=c("wet","dry"))

hddata.m <- summaryBy(diam+h+d2h~room+prov+Date,data=subset(hddata,Water_trt=="wet"),FUN=c(mean,standard.error))
hddata.l <- split(hddata.m,hddata.m$room)

#- plot d2h
windows(30,60);par(mfrow=c(6,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
ylims=c(0,3)
for(i in 1:length(hddata.l)){
  toplot <- hddata.l[[i]]
  plotBy(d2h.mean~Date|prov,data=toplot,type="o",ylim=ylims,pch=16,legend=F,yaxt="n",xaxt="n",cex=1.5,
         panel.first=adderrorbars(x=toplot$Date,y=toplot$d2h.mean,SE=toplot$d2h.standard.error,direction="updown"))
  legend("top",paste("room",toplot$room[1]),bty="n")
  if(i<6)axis.Date(side=1,at=seq.Date(from=min(toplot$Date),to=max(toplot$Date),by=1),labels=F)
  if(i==6)axis.Date(side=1,at=seq.Date(from=min(toplot$Date),to=max(toplot$Date),by=1),labels=T)
  magaxis(side=c(2,4),labels=c(1,1),las=1)
  if(i==1)legend("topleft",c("A","B","C"),pch=16,col=c("black","red","green3"))
  title(ylab=expression(d^2*h~(cm^3)),outer=T,adj=0.5,cex.lab=3)
}

#- 3d plots
library(rgl)
plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$h.m,col=palette()[hddata.m$prov],size=12)
plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$diam.m,col=palette()[hddata.m$prov],size=12)
plot3d(x=hddata.m$Date,y=hddata.m$room,z=hddata.m$d2h.m,col=palette()[hddata.m$prov],size=12)




#------------------------------------------------------------------------------------------------------------
#- get and plot the growth increments, ending on a specified date
focaldate <- max(hddata$Date)
hddata2.m <- summaryBy(diam+h+d2h~room+Date,data=subset(hddata,Water_trt=="wet"),FUN=c(mean),keep.names=T)
hddata2.m$dh <-c(NA,diff(hddata2.m$h)) 
hddata2.m$ddiam <-c(NA,diff(hddata2.m$diam)) 
hddata2.m$dd2h <-c(NA,diff(hddata2.m$d2h)) 
hddata2.m$rgr_d2h <-c(NA,diff(log(hddata2.m$d2h)/10)) # assumes a 10-day interval

#- plot growth increments
windows(40,60);par(mfrow=c(3,1),mar=c(5,7,1,1),oma=c(0,0,0,0),cex.lab=2)
plot(dh~as.numeric(room),data=subset(hddata2.m,Date==focaldate),ylim=c(0,20),xlab="room",ylab="dHeight (cm)",pch=16,cex=2)
plot(ddiam~as.numeric(room),data=subset(hddata2.m,Date==focaldate),ylim=c(0,1),xlab="room",ylab="dDiameter (mm)",pch=16,cex=2)
plot(dd2h~as.numeric(room),data=subset(hddata2.m,Date==focaldate),ylim=c(0,2),xlab="room",ylab="dD2h (cm3)",pch=16,cex=2)

#- plot RGR over time. Ontogenetic drift is already apparent.
rgrdat <- subset(hddata2.m,Date>as.Date("2016-01-10"))
plot3d(x=rgrdat$Date,y=rgrdat$room,z=rgrdat$rgr_d2h,size=12)

#------------------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------------------
#- get and plot the growth increments for DRY VS WET in B, ending on a specified date
#- This code will have to change when new RGR estimates are available, as the diff() function will return more values.
focaldate <- max(hddata$Date)
hddata3.m <- summaryBy(diam+h+d2h~room+Water_trt+Date,data=subset(hddata,prov=="B"),FUN=c(mean),keep.names=T)
hddata3.m$dh <-c(NA,diff(hddata3.m$h)) 
hddata3.m$ddiam <-c(NA,diff(hddata3.m$diam)) 
hddata3.m$dd2h <-c(NA,diff(hddata3.m$d2h)) 
toplot <- subset(hddata3.m,Date==focaldate)

windows(40,60);par(mfrow=c(3,1),mar=c(5,7,1,1),oma=c(0,0,0,0),cex.lab=2)
with(toplot,plot(dh~as.numeric(room),col=Water_trt,ylim=c(0,20),xlab="room",ylab="dHeight (cm)",pch=16,cex=2))
legend("topleft",pch=16,col=c("black","red"),legend=c("wet","dry"),cex=1.5)
with(toplot,plot(ddiam~as.numeric(room),col=Water_trt,ylim=c(0,1),xlab="room",ylab="dDiameter (mm)",pch=16,cex=2))
with(toplot,plot(dd2h~as.numeric(room),col=Water_trt,ylim=c(0,2),xlab="room",ylab="dD2h (cm3)",pch=16,cex=2))
#------------------------------------------------------------------------------------------------------------










#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#- process the total plant leaf area data

la <-read.csv("W://WORKING_DATA/GHS39/GREAT/Share/Data/leafarea/GHS39_GREAT_MAIN_LEAFAREA_20160128_L1.csv")
la$prov <- as.factor(substr(la$pot,start=1,stop=1))
la$room <- as.factor(la$room)
la$prov_trt <- as.factor(paste(la$prov,la$room,sep="-"))
la$Date <- as.Date("2016-1-28")

#- assign drought treatments
la$Water_trt <- "wet"
la$Water_trt[grep("Bd",la$pot)] <- "dry"
la$Water_trt <- factor(la$Water_trt,levels=c("wet","dry"))

#- calculate total plant leaf area. This method uses a different average leaf size for each plant
la$canopy <- with(la,leaf_no*lf_area)


#----------------------------------------------------------------------------------------------------
#- calculate total plant leaf area using a room-specific mean value
leaf_size <- summaryBy(lf_area~room+lf_size,data=la,FUN=mean,keep.names=F)


#-- average leaf size is temperature dependent, but not provenance dependent
boxplot(lf_area~lf_size+room,data=la)
boxplot(lf_area~prov+room,data=subset(la,lf_size=="large"))

la2.1 <- merge(la,leaf_size,by=c("room","lf_size"))
la2.1$canopy2 <- with(la2.1,leaf_no*lf_area.mean)

la2 <- summaryBy(canopy+canopy2~room+pot+prov+prov_trt+Date+Water_trt,data=la2.1,FUN=sum,keep.names=T)

#- merge in total plant leaf number
leaf_no <-summaryBy(leaf_no~pot,data=la,FUN=sum,keep.names=T)
la2 <- merge(la2,leaf_no,by="pot")



#- merge total plant leaf area with tree size
la3 <- merge(la2,subset(hddata,Date==as.Date("2016-1-28")),by=c("room","prov","prov_trt","pot","Date","Water_trt"))

#- merge in the growth increments for each plant
hddata.l <- split(hddata,hddata$pot)
dh <- ddiam <- dd2h <- pot <- c()
for(i in 1:length(hddata.l)){
  dat <- hddata.l[[i]]
  
  dh[i] <- diff(dat$h)[nrow(dat)-1] 
  ddiam[i] <- diff(dat$diam)[nrow(dat)-1] 
  dd2h[i] <- diff(dat$d2h)[nrow(dat)-1]
  pot[i] <- as.character(dat$pot[1])
}
inc.df <- data.frame(pot,dh,ddiam,dd2h)
la4 <- merge(la3,inc.df,by="pot")
la4$logLA <- with(la4,log10(canopy))
la4$logd2h <- with(la4,log10(d2h))

#- plot log-log relation between d2h and leaf area, compare to allometry from GLAHD
windows()
with(la4,plot(logLA~logd2h))
abline(a=1.889,b=0.7687) # allometry from GLAHD
