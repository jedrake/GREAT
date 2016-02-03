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
  
  la <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_LEAFAREA_20160128_L1.csv",sep=""))
  la$prov <- as.factor(substr(la$pot,start=1,stop=1))
  la$room <- as.factor(la$room)
  la$prov_trt <- as.factor(paste(la$prov,la$room,sep="-"))
  la$Date <- as.Date("2016-1-28")
  
  #- assign drought treatments
  la$Water_trt <- "wet"
  la$Water_trt[grep("Bd",la$pot)] <- "dry"
  la$Water_trt <- factor(la$Water_trt,levels=c("wet","dry"))
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
  
  return(aq)
}
#-----------------------------------------------------------------------------------------