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
Rdat2$prov <- NULL

#- merge in the size data (just to get things like room numbers and drought treatments, etc)
size <- getSize()
size <- size[,c("pot","room","prov","Water_trt","prov_trt")]
size <- unique(size)
Rdat <- merge(Rdat2,size,by=c("pot"))
Rdat$Date <- as.Date(as.character(Rdat$Date),format="%Y%m%d")
Rdat$Rarea <- -1*Rdat$Photo

#- average across provenances in each room
Rdat.m <- summaryBy(Rarea~room+prov+Water_trt+Organ,data=Rdat,FUN=c(mean,standard.error),na.rm=T)
Rdat.m$Rarea.standard.error[which(is.na(Rdat.m$Rarea.standard.error))] <- 0


#- plot treatment averages
toplot <- subset(Rdat.m,Water_trt=="wet" & Organ=="leaf")
toplot$room <- as.numeric(toplot$room)
plotBy(Rarea.mean~room|prov,data=toplot,las=1,xlim=c(1,6),ylim=c(0,2),legend=F,pch=16,
       panel.first=adderrorbars(x=toplot$room,y=toplot$Rarea.mean,SE=toplot$Rarea.standard.error,direction="updown"),
       axes=F,ylab=expression(R[area]~(mu*mol~CO[2]~m^-2~s^-1)),xlab="Room",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
