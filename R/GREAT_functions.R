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
  hddata$Date <- as.Date(hddata$date,format="%d/%m/%Y")
  
  
  #- Bd-78 was mistakenly recorded on 2016-01-28 
  hddata[which(hddata$pot=="Bd-78" & hddata$Date==as.Date("2016-01-28")),"d1"] <- 2.68
  hddata[which(hddata$pot=="Bd-78" & hddata$Date==as.Date("2016-01-28")),"d2"] <- 2.75
  
  hddata$diam <- with(hddata,((d1+d2)/2))
  hddata$d2h <- with(hddata,(diam/10)^2*h) #cm^3

  
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
  la$Date <- as.Date("2016-1-28")
  
  la2 <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_LEAFAREA_20160209_L1.csv",sep=""))
  la2$Date <- as.Date("2016-2-09")
  
  la <- rbind(la,la2)
  
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
getPunches <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  dat1 <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_PUNCHES_LEAFAREA_20160129_L1.csv",sep=""))
  dat1$Date <- as.Date("2016-1-29")
  
  dat2 <-read.csv(paste(path,"/Share/Data/leafarea/GHS39_GREAT_MAIN_PUNCHES_LEAFAREA_20160209_L1.csv",sep=""))
  dat2$Date <- as.Date("2016-2-9")
  
  dat <- rbind(dat1,dat2)
  
  dat$SLA <- with(dat,area_cm2/(mass_mg/1000))         # in cm2 g-1
  dat$LMA <- with(dat,(mass_mg/1000)/(area_cm2/10000)) # in g m-2
  
  dat$prov2 <- dat$prov
  dat$prov <- as.factor(substr(dat$pot,start=1,stop=1)) # overwrite "prov" to be A, B, or C. No Bw or Bd allowed.
  dat$room <- as.factor(dat$room)
  dat$prov_trt <- as.factor(paste(dat$prov,dat$room,sep="-"))
  
  #- assign drought treatments
  dat$Water_trt <- "wet"
  dat$Water_trt[grep("Bd",dat$pot)] <- "dry"
  dat$Water_trt <- factor(dat$Water_trt,levels=c("wet","dry"))
  return(dat)
}  
#-----------------------------------------------------------------------------------------  



#-----------------------------------------------------------------------------------------
#- function to compile and return the three estimates of SLA from the GREAT experiment.
#  (1) leaf punches on 2016-1-29 and 2016-2-9,
#  (2) gas exchange leaves harvested on 2016-2-11 and 2016-2-29,
#  (3) whole-crown average SLA obtained by harvesting (total leaf area / total leaf mass)
#  Returns a list of these three elements
getSLA <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  #- get the punches
  punches <- getPunches()
  
  #---
  #- get the gas exchange leaves
  leaf1 <- read_excel(path=paste(path,"/Share/Data/GasEx/GHS39_GREAT_MAIN_BIOMASS_Gx-leaves_20160211_L1.xlsx",sep=""))
  leaf2 <- read_excel(path=paste(path,"/Share/Data/GasEx/GHS39_GREAT_MAIN_BIOMASS_Gx-leaves_20160229_L1.xlsx",sep=""))
  leaf <- rbind(leaf1,leaf2)
  leaf$SLA <- with(leaf,LA_cm2/(mass_g))         # in cm2 g-1
  leaf$LMA <- with(leaf,(mass_g)/(LA_cm2/10000)) # in g m-2
  leaf$comment <- NULL
  names(leaf)[2] <- tolower(names(leaf)[2])
  
  #- merge in a "key" from the punches dataset 
  key <- subset(punches,Date==as.Date("2016-01-29"))[,c("room","prov2","pot","Water_trt")]
  potnumber <- unlist(strsplit(as.character(key$pot),split="-"))[2*(1:length(strsplit(as.character(key$pot),split="-")))  ]
  potnumber2 <- sprintf("%02d",as.numeric(potnumber))
  key$Pot <- key$pot
  key$pot <- paste(key$prov2,potnumber2,sep="-")
  
  leaf3 <- merge(leaf,key,by=("pot"))
  leaf3$prov2 <- leaf3$Pot <- NULL
  leaf3$Prov <- as.factor(substr(as.character(leaf3$Prov),start=1,stop=1))
  names(leaf3)[2] <- tolower(names(leaf3)[2])
  #---
  
  
  #- get the harvest dataset
  harvest <- getHarvest()
  harvest$SLA <- with(harvest,leafarea/leafdm)           # in cm2 g-1
  harvest$LMA <- with(harvest,leafdm/(leafarea/10000))   # in g m-2
  harvest <- subset(harvest,Date != as.Date("2016-01-07")) # exclude the pre-planting harvest (SLA and LMA very different!)
  harvest2 <- merge(harvest,key,by="Pot")
  return(list(punches,leaf3,harvest2)) # return a list of the three kinds of SLA measurements
}
#-----------------------------------------------------------------------------------------



  
#-----------------------------------------------------------------------------------------
#- function to read and process the Asat and AQ datasets
getAQ <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  #- read in the first set of measurements
  aq1 <-read.csv(paste(path,"/Share/Data/GasEx/AQ/GREAT-AQ-compiled-20160202-20160203-L1.csv",sep=""))
  aq1$campaign = 1
  
  #- read in the second set (prov B only!)
  aq2 <-read.csv(paste(path,"/Share/Data/GasEx/AQ/GREAT-AQ2-compiled-20160225-20160226-L1.csv",sep=""))
  aq2$campaign = 2

  aq <- rbind(aq1,aq2)
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
  
  #- a few pots have strange data. UWS3 didn't seem to actually drop to low PARi on three cases.
  #-   so remove those bad data. This should remove 3 datapoints!
  aq <- aq[complete.cases(aq),]
  
  #- assign the temperature levels
  aq$TleafFac <- cut(aq$Tleaf,breaks=c(15,22,26,29,34,37,45),labels=1:6)
  
  #- average across replicate logs
  aq.means <- summaryBy(.~room+pot+Unit+prov+prov_trt+Water_trt+LightFac+TleafFac+campaign,data=aq,FUN=mean,keep.names=T)
  return(aq.means)
}
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#- function to read the soil moisture data
getVWC_AQ <- function(path="W://WORKING_DATA/GHS39/GREAT"){
  
  #- read in the data
  vwc <- read.csv(paste(path,"/Share/Data/GHS39_GREAT_MAIN_SOILVWC_hydrosense_L1.csv",sep=""))
  names(vwc)[1:3] <- tolower(names(vwc)[1:3])
  vwc$prov <- as.factor(substr(vwc$treat,start=1,stop=1))
  vwc$room <- as.factor(vwc$room)
  vwc$prov_trt <- as.factor(paste(vwc$prov,vwc$room,sep="-"))
  
  #- fix up the "pot" variable to be the same as in the AQ datasets
  vwc$pot <- paste(vwc$treat,sprintf("%02d",vwc$pot),sep="-")
  
  #- assign drought treatments
  vwc$Water_trt <- "wet"
  vwc$Water_trt[grep("Bd",vwc$treat)] <- "dry"
  vwc$Water_trt <- factor(vwc$Water_trt,levels=c("wet","dry"))
  
  #- average across sub-replicate measurements
  vwc.m <- summaryBy(VWC1~pot+Water_trt,data=vwc,FUN=mean,keep.names=T,na.rm=T)
  names(vwc.m)[ncol(vwc.m)] <- "vwc"
  
  return(vwc.m)
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





#-----------------------------------------------------------------------------------------
#- function to get the confidence intervals of an NLS fit
#https://quantitativeconservationbiology.wordpress.com/2013/07/02/confidence-interval-for-a-model-fitted-with-nls-in-r/

as.lm.nls <- function(object, ...) {
  if (!inherits(object, "nls")) {
    w <- paste("expected object of class nls but got object of class:", 
               paste(class(object), collapse = " "))
    warning(w)
  }
  
  gradient <- object$m$gradient()
  if (is.null(colnames(gradient))) {
    colnames(gradient) <- names(object$m$getPars())
  }
  
  response.name <- if (length(formula(object)) == 2) "0" else 
    as.character(formula(object)[[2]])
  
  lhs <- object$m$lhs()
  L <- data.frame(lhs, gradient)
  names(L)[1] <- response.name
  
  fo <- sprintf("%s ~ %s - 1", response.name, 
                paste(colnames(gradient), collapse = "+"))
  fo <- as.formula(fo, env = as.proto.list(L))
  
  do.call("lm", list(fo, offset = substitute(fitted(object))))
  
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to fit the June et al. (2004) FPB model for the temperature response of photosynthesis.
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitAvT <- function(dat){
  try(A_Topt <- nls(Photo~ Jref*exp(-1*((Tleaf-Topt)/theta)^2),data=dat,start=list(Jref=20,Topt=25,theta=20)))
  A_Topt2 <- summary(A_Topt)
  results <- A_Topt2$coefficients[1:6]
  names(results)[1:6] <- c("Aref","Topt","theta","Aref.se","Topt.se","theta.se")
  
  TT <- seq(min(dat$Tleaf),max(dat$Tleaf),length=51)
  predicts <- predictNLS(A_Topt, newdata=data.frame(Tleaf = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df))
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to fit a Q10 model of respiration
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitRvT <- function(dat){
  try(R_Topt <- nls(Rarea~  Rref*Q10^((Tleaf-22.5)/10),data=dat,start=list(Rref=10,Q10=2)))
  R_Topt2 <- summary(R_Topt)
  results <- R_Topt2$coefficients[1:4]
  names(results)[1:4] <- c("Rref","Q10","Rref.se","Q10.se")
  
  TT <- seq(min(dat$Tleaf),max(dat$Tleaf),length=51)
  predicts <- predictNLS(R_Topt, newdata=data.frame(Tleaf = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df))
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to fit the June et al. (2004) FPB model for the temperature response of GROWTH
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitAGRvT <- function(dat){
  try(G_Topt <- nls(AGR.mean~ AGRref*exp(-1*((Tair-Topt)/theta)^2),data=dat,start=list(AGRref=0.5,Topt=30,theta=5)))
  G_Topt2 <- summary(G_Topt)
  results <- G_Topt2$coefficients[1:6]
  names(results)[1:6] <- c("AGRref","Topt","theta","AGRref.se","Topt.se","theta.se")
  
  TT <- seq(min(dat$Tair),max(dat$Tair),length=51)
  predicts <- predictNLS(G_Topt, newdata=data.frame(Tair = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df))
}
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#- function to fit the June et al. (2004) FPB model for the temperature response of GROWTH
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitRGRvT <- function(dat){
  try(G_Topt <- nls(RGR.mean~ RGRref*exp(-1*((Tair-Topt)/theta)^2),data=dat,start=list(RGRref=0.15,Topt=25,theta=20)))
  G_Topt2 <- summary(G_Topt)
  results <- G_Topt2$coefficients[1:6]
  names(results)[1:6] <- c("RGRref","Topt","theta","RGRref.se","Topt.se","theta.se")
  
  TT <- seq(min(dat$Tair),max(dat$Tair),length=51)
  predicts <- predictNLS(G_Topt, newdata=data.frame(Tair = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df))
}
#-----------------------------------------------------------------------------------------









#-----------------------------------------------------------------------------------------
#-  Attempting to write a generic function to fit temperature response curves.
#- function to fit the June et al. (2004) FPB model for the temperature response of GROWTH
#- accepts a dataframe, returns a list with [1] named vector of parameter estiamtes and their se's,
#-   and [2] a dataframe with the predictions and 95% confidence intervals.
fitJuneT <- function(dat,namex,namey,start=list(Rref=2,Topt=20,theta=20)){
  dat$Yvar <- dat[[namey]]
  dat$Xvar <- dat[[namex]]
  
  try(G_Topt <- nls(Yvar~ Rref*exp(-1*((Xvar-Topt)/theta)^2),data=dat,start=list(Rref=0.15,Topt=25,theta=20)))
  G_Topt2 <- summary(G_Topt)
  results <- G_Topt2$coefficients[1:6]
  names(results)[1:6] <- c(paste(namey,"ref",sep=""),"Topt","theta",paste(namey,"ref.se",sep=""),"Topt.se","theta.se")
   
  TT <- seq(min(dat$Xvar),max(dat$Xvar),length=51)
  predicts <- predictNLS(G_Topt, newdata=data.frame(Xvar = TT),interval="confidence",level=0.95)
  predicts.df <- data.frame(predicts$summary)
  predicts.df$Tleaf <- TT
  
  return(list(results,predicts.df))
}
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- function to return the harvest data as a dataframe
getHarvest <- function(path="W://WORKING_DATA/GHS39/GREAT"){

    #- read in the raw data
  files <-  list.files(paste(path,"/Share/Data/Harvests/",sep=""),pattern="GHS39_GREAT_MAIN_BIOMASS",full.names=T)
  files2 <- files[grep(".csv",files)] # only get the .csv files
  
  #- pull out the longer file name that has a different structure
  longfile <- files2[which.max(nchar(files2))]
  files3 <- files2[-which.max(nchar(files2))]
  
  dates <- c()
  dat.i <- list()
  for (i in 1:length(files3)){
    dates[i] <- as.numeric(substr(files3[i],start=75,stop=82)) # extract the date from the file name
    dat.i[[i]] <- read.csv(files2[i])
    dat.i[[i]]$Date <- base::as.Date(as.character(dates[i]),format="%Y%m%d")
    names(dat.i[[i]]) <- c("Pot","h","d1","d2","leafarea","leafno","leafdm","stemdm","rootdm","Date")
  }
  dat <- do.call(rbind,dat.i)
  dat$prov <- as.factor(substr(dat$Pot,start=1,stop=1))
  
  #- read in the longer datafile, fix up names for merging
  dat.long <- read.csv(longfile)
  dat.long <- dat.long[,c("Code","h..cm.","d1..mm.","d2..mm.","leafno....","leafarea..cm2.",
                          "leafsubdm","leaf_add","stemdm","rootdm","rootdm_measured")]
  dat.long$Date <- base::as.Date("2016-02-22")
  
  
  #- calculate total leaf and root dry mass
  dat.long$leafdm <- base::rowSums(dat.long[,c("leafsubdm","leaf_add")],na.rm=T)
  dat.long$rootdm <- base::rowSums(dat.long[,c("rootdm","rootdm_measured")],na.rm=T)
  

  dat.long <- dat.long[,c("Code","h..cm.","d1..mm.","d2..mm.","leafno....","leafarea..cm2.",
                          "leafdm","stemdm","rootdm","Date")]
  names(dat.long) <- c("Pot","h","d1","d2","leafno","leafarea","leafdm","stemdm","rootdm","Date")
  dat.long$prov <- as.factor(substr(dat.long$Pot,start=1,stop=1))
  
  #- these mass values are in mg. Convert to g.
  dat.long$leafdm <- dat.long$leafdm/1000
  dat.long$stemdm <- dat.long$stemdm/1000
  dat.long$rootdm <- dat.long$rootdm/1000
  
  
  dat <- rbind(dat,dat.long)
  #----------------------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------------------
  #- Do some data manipulation
  
  #- height was measured in mm for the first dataset. convert to cm
  firsts <- which(dat$Date == as.Date("2016-01-07"))
  dat[firsts,"h"] <- dat[firsts,"h"]/10
  
  #- do some simple math
  dat$diam <- base::rowMeans(cbind(dat$d1,dat$d2))
  dat$d2h <- with(dat,h*(diam/10)^2) # calculates in units of cubic centimeters
  dat$totdm <- base::rowSums(cbind(dat$leafdm,dat$stemdm,dat$rootdm))
  
  #- total leaf area was recorded incorrectly for C-33. It should be 1079, not 107.9 cm2. 
  dat[which(dat$Pot=="C-33"),"leafarea"] <- 1079
  
  #- log transform (base 10!)
  dat$logd2h <- log10(dat$d2h)
  dat$logtotdm <- log10(dat$totdm)
  return(dat)
}
#----------------------------------------------------------------------------------------------------------------




#- function to return an estimated mass based on tree size (d2h). Takes a vector of d2h (cm3), returns
#-  a vector of estimated total mass (g). Can later be expanded to include other predictors (provenance, etc),
#-  and other predictors (total leaf area, for example).
returnMassFromAllom <- function(d2hdat,plotson=T){
  
  #- get the harvest data
  dat <- getHarvest()
  
  #----------------------------------------------------------------------------------------------------------------
  #- model the allometry, return predicted mass for the vector of d2h values
  lm1 <- lm(logtotdm~logd2h+I(logd2h^2),data=dat)
  predictions <- 10^predict(lm1,newdata=data.frame(logd2h=log10(d2hdat)))
  #----------------------------------------------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------------------------------------------------------
  #- how many of the observations lie outside of the allometry?
  toolow <- which(d2hdat < min(dat$d2h))
  toohigh <- which(d2hdat > max(dat$d2h))
  
  outsides <- sum(length(toolow),length(toohigh))
  total <- length(d2hdat)
  percentage <- round(outsides/total*100,1)
  print(paste(outsides," observations of ",total," outside of allometry (",percentage,"%)",sep=""))
  #----------------------------------------------------------------------------------------------------------------
  
  
  #----------------------------------------------------------------------------------------------------------------
  #- exploratory plotting
  if (plotson==T){
    plotBy(logtotdm~logd2h|prov,data=dat,pch=15)
    coefs <- coef(lm1)
    xval <- seq(min(dat$logd2h),max(dat$logd2h),length=101)
    preds <- coefs[1]+xval*coefs[2]+xval^2*coefs[3]
    lines(preds~xval)
  }
  #----------------------------------------------------------------------------------------------------------------
  
  return(predictions)
}




#- function to caculate and return growth metrics. Returns a list of two dataframes:
#     [1] RGR and AGR caculated for all available data.
#     [2] RGT and AGR merged with canopy leaf area and SLA for the intensive growth interval only
returnRGR <- function(path="W://WORKING_DATA/GHS39/GREAT",plotson=F){
  
  #-----------------------------------------------------------------------------------------
  #- get the size data, estimate mass
  
  hddata <- getSize(path=path) # specify path to "GREAT" share folder on HIE-Data2. Defaults to W://WORKING_DATA/GHS39/GREAT
  hddata$totmass <- returnMassFromAllom(d2hdat=hddata$d2h,plotson=plotson) #- predict mass from d2h allometry
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
  
  
  #- pull out the most important agr and rgr estimates (during our growth interval from Jan 28th to Feb 8th)
  rgrinterval1 <- subset(hddata2,Date==as.Date("2016-02-08"))
  rgrinterval <- rgrinterval1[complete.cases(rgrinterval1),] # remove missing data
  #-----------------------------------------------------------------------------------------
  
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
  
  

  
  la2.1 <- merge(la,leaf_size,by=c("room","lf_size","Date"))
  la2.1$canopy2 <- with(la2.1,leaf_no*lf_area.mean)
  
  #-- average leaf size is temperature dependent, but not provenance dependent
  if(plotson==T){
    windows(40,50);par(mfrow=c(3,1),cex.lab=1.7,mar=c(7,7,2,1))
    boxplot(leaf_no~lf_size+room,data=subset(la,Water_trt=="wet" & Date==as.Date("2016-01-28")),las=2,ylab="Leaf number (n)",col=c("grey","red"))
    legend("topleft",c("small","large"),fill=c("grey","red"))
    boxplot(lf_area~lf_size+room,data=subset(la,Water_trt=="wet"& Date==as.Date("2016-01-28")),las=2,ylab="Average leaf size (cm2)",col=c("grey","red"))
    boxplot(canopy~prov+room,data=subset(la,Water_trt=="wet"& Date==as.Date("2016-01-28")),las=2,ylim=c(0,1000),ylab="Total canopy size (cm2)",col=c("blue","orange","forestgreen"))
    legend("topleft",c("A","B","C"),fill=c("blue","orange","forestgreen"))
    
  }
  
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
  # windows()
  # plotBy(logLA~logd2h|Date,data=la3)
  # abline(a=1.889,b=0.7687) # allometry from GLAHD
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  #---- get the SLA data from punches. SLA increase with temperature, to a point.
  sla1 <- getPunches()
  sla <- summaryBy(SLA+LMA~room+prov+pot+prov_trt+Water_trt,FUN=mean,keep.names=T,data=sla1)
  rgrdat <- merge(la3,sla,by=c("room","prov","pot","prov_trt","Water_trt"))

  
  
  
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  #- okay, so NOW we have a dataframe with all the bits in it for a formal RGR analysis. 
  #    We have growth, canopy leaf area, and SLA. These were measured INDEPENDENTLY.
  #head(rgrdat)
  rgrdat$logmass <- log10(rgrdat$totmass)
  
  #- calculate LAR (m2 kg-1)
  rgrdat$LAR <- with(rgrdat,canopy/10000/(totmass/1000))
  
  #- calculate leaf mass fraction (g g-1)
  rgrdat$LMF <- with(rgrdat,canopy/SLA/totmass)
  
  #- calculate net assimilation rate (NAR; g m-2 d-1) from absolute growth rate and total canopy leaf area
  rgrdat$NAR <- with(rgrdat,AGR/(canopy/10000))
  
  
  #- plot interrelationships
  if(plotson==T){
    windows()
    pairs(subset(rgrdat,Water_trt=="wet")[,c("RGR","AGR","LAR","LMF","NAR","canopy","SLA","logmass","room")],
          col=rev(brewer.pal(6,"RdYlGn"))[subset(rgrdat,Water_trt=="wet")$room])
  }
  
  
  #- get rid of some unwanted variabels to make things simpler
  rgrdat$canopy2 <- rgrdat$date <- rgrdat$d1 <- rgrdat$d2 <- rgrdat$Comment <- rgrdat$leaf_no <- NULL
  
  return(list(hddata2,rgrdat))
  #--------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------
  
}