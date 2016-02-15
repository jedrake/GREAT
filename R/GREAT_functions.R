#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Functions for analysis of GREAT data. 
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- function to read and return the most recent size measurements of height and diameter
getSize <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  #- work out the path
  
  #- find the most recent file
  files <-  list.files(paste(path,"/Share/Data/Height&Diam",sep=""),pattern="HEIGHT&DIAMETER",full.names=T)
  files2 <- files[grep(".csv",files)] # only get the .csv files
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
  hddata$date <- NULL
  
  #- assign drought treatments
  hddata$Water_trt <- "wet"
  hddata$Water_trt[grep("Bd",hddata$pot)] <- "dry"
  hddata$Water_trt <- factor(hddata$Water_trt,levels=c("wet","dry"))
  
  return(hddata)
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to read and process the leaf number and leaf size datasets
getLA <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  #- read in the leaf count and leaf size datasets, measured on two dates.
  la1 <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_LEAFAREA_20160128_L1.csv",sep=""))
  la2 <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_LEAFAREA_20160209_L1.csv",sep=""))
  la1$Date <- as.Date("2016-01-28")
  la2$Date <- as.Date("2016-02-08") #- note that this is a bit of a fib to merge the leaf area with the d2h measures
  
  #- merge into a single file
  la <- rbind(la1,la2)
  la$prov <- as.factor(substr(la$pot,start=1,stop=1))
  la$room <- as.factor(la$room)
  la$prov_trt <- as.factor(paste(la$prov,la$room,sep="-"))
  
  #- assign drought treatments
  la$Water_trt <- "wet"
  la$Water_trt[grep("Bd",la$pot)] <- "dry"
  la$Water_trt <- factor(la$Water_trt,levels=c("wet","dry"))
  return(la)
}
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- function to read and process the leaf punch datasets
#- note, these punches haven't been weighed yet so there basically is no data.
getPunches <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  dat <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_PUNCHES_LEAFAREA_20160202_L1.csv",sep=""))
  
}  
#-----------------------------------------------------------------------------------------  
  
#-----------------------------------------------------------------------------------------
#- function to read and process the Asat and AQ datasets
getAQ <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  aq <-read.csv(paste(path,"/Share/Data/GasEx/AQ/GREAT-AQ-compiled-20160202-20160203-L1.csv",sep=""))
  names(aq)[1:2] <- tolower(names(aq)[1:2])
  aq$prov <- as.factor(substr(aq$pot,start=1,stop=1))
  aq$room <- as.factor(aq$room)
  aq$prov_trt <- as.factor(paste(aq$prov,aq$room,sep="-"))

  #- assign drought treatments
  aq$Water_trt <- "wet"
  aq$Water_trt[grep("Bd",aq$pot)] <- "dry"
  aq$Water_trt <- factor(aq$Water_trt,levels=c("wet","dry"))
  
  #- assign light levels to a factor variable
  aq$LightFac <- NA
  aq$LightFac[which(aq$PARi<150)] <- 1
  aq$LightFac[which(aq$PARi>400 & aq$PARi<600)] <- 2
  aq$LightFac[which(aq$PARi>800 & aq$PARi<1100)] <- 3
  aq$LightFac[which(aq$PARi>1200)] <- 4
  aq$LightFac <- as.factor(aq$LightFac)
  
  
  #- assign the temperature levels
  aq$TleafFac <- cut(aq$Tleaf,breaks=c(15,22,26,29,34,37,45),labels=1:6)
  
  #- average across replicate logs
  aq2 <- summaryBy(.~room+pot+Unit+prov+prov_trt+Water_trt+LightFac+TleafFac,data=aq,FUN=mean,keep.names=T)
  
  return(aq2)
}
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- function to read and process the temperature response curves of photosynthesis
getAvT <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  avt <-read.csv(paste(path,"/Share/Data/GasEx/AvT/GREAT-AvT-compiled-20160205-L1.csv",sep=""))
  names(avt)[1:2] <- tolower(names(avt)[1:2])
  avt$prov <- as.factor(substr(avt$pot,start=1,stop=1))
  avt$room <- as.factor(avt$room)
  avt$prov_trt <- as.factor(paste(avt$prov,avt$room,sep="-"))
  
  #- assign drought treatments
  avt$Water_trt <- "wet"
  
  #- assign light levels to a factor variable
  avt$LightFac <- NA
  avt$LightFac[which(avt$PARi<150)] <- 1
  avt$LightFac[which(avt$PARi>1200)] <- 4
  avt$LightFac <- as.factor(avt$LightFac)
  
  #- assign the temperature levels
  avt$TleafFac <- cut(avt$Tleaf,breaks=c(15,22,26,29,34,37,45),labels=1:6)
  
  avt2 <- summaryBy(.~room+pot+Unit+prov+prov_trt+Water_trt+LightFac+TleafFac,data=avt,FUN=mean,keep.names=T)
  return(avt2)
}
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- function to read and process the temperature response curves of respiration
getRvT <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  rvt <-read.csv(paste(path,"/Share/Data/GasEx/Rdark/GREAT-Rdark-compiled-20160211-L1.csv",sep=""))
  names(rvt)[1:2] <- tolower(names(rvt)[1:2])
  rvt$prov <- as.factor(substr(rvt$pot,start=1,stop=1))
  rvt$room <- as.factor(rvt$room)
  rvt$prov_trt <- as.factor(paste(rvt$prov,rvt$room,sep="-"))
  rvt$Rarea <- rvt$Photo*-1
  
  #- assign drought treatments
  rvt$Water_trt <- "wet"
  
  #- assign the temperature levels
  rvt$TleafFac <- cut(rvt$Tleaf,breaks=c(10,15,20,25,27.5,35),labels=1:5)
  
  #- average across sub-replicate logs
  rvt2 <- summaryBy(.~room+pot+prov+prov_trt+Water_trt+TleafFac,data=rvt,FUN=mean,keep.names=T)
  
  #- got them all? We're missing the fourth temperature for bw-26.
  xtabs(~pot,data=rvt2)
  return(rvt2)
}
#-----------------------------------------------------------------------------------------


