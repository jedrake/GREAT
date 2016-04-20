#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- This script makes a table of all of the temperature response parameters
#   AGR, RGR, Asat-growth, Asat-short-term, Anet-lowT, Rdark, etc.
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------

#--- Biomass
#- prepare biomass data
dat <- getHarvest()
size <- getSize()

#- pull out just the unique pot and room numbers from teh size dataframe
size2 <- unique(size[,c("pot","room","Water_trt","location","Tair")])

#- merge pot ids and harvest. Note the pre-treatment plants get excluded here
dat2 <- merge(size2,dat,by.x=c("pot","location"),by.y=c("Pot","location"))
massdata <- subset(dat2,Water_trt == "wet")

#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
RGRdat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only

#- get the AvT data (short-term)
avt <- getAvT()

#- get the AQ data (long-term)
aq <- getAQ()
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------

#- prepare datasets into list for fitting

#- final mass
massdata.l <- split(massdata,massdata$location)

#- AGR and RGR v T to estimate Topts
tofit <- subset(RGRdat,Water_trt=="wet")
tofit.l <- split(tofit,tofit$location)

#- AvT to estimate Topts (long-termdata)
tofit.longterm <- subset(aq,campaign==1 & LightFac==4 & Water_trt=="wet")
tofit.tofit.longterm.l <- split(tofit.longterm,tofit.longterm$location)

#- AvT to estimate Topts at high incident par
tofit.AvT <- subset(avt, LightFac==4)
tofit.AvT.l <- split(tofit.AvT,tofit.AvT$location)

#- Avt at low PAR
tofit_lowQ <- subset(avt, LightFac==1)
tofit.l_lowQ <- split(tofit_lowQ,tofit_lowQ$location)


#--- actually fit all teh T-response curves
#- fit final mass
MASSvTfits.l <- lapply(massdata.l,FUN=fitJuneT,start=list(Rref=5,Topt=30,theta=5),namey="totdm",namex="Tair",lengthPredict=20)


#- fit AGR and RGR T response curves
AGRvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.5,Topt=30,theta=5),namey="AGR",namex="Tair",lengthPredict=20)
RGRvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.15,Topt=25,theta=20),namey="RGR",namex="Tair",lengthPredict=20)


#- fit all the curves (long-term photo at high par)
AvTfits.list.longterm <- lapply(tofit.tofit.longterm.l,FUN=fitAvT)

#- fit Tresponse curves at high PAR
AvTfits.list.st <- lapply(tofit.AvT.l,FUN=fitAvT)

#- fit Tresposne curves at low PAR
AvTfits.list2.st <- lapply(tofit.l_lowQ,FUN=fitJuneT,start=list(Rref=6,Topt=12,theta=15),namey="Photo",namex="Tleaf",lengthPredict=20)
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- pull out the parameter means and SE's for plotting

#- final mass
MASSvTfits <- data.frame(do.call(rbind,
                                list(MASSvTfits.l[[1]][[1]],MASSvTfits.l[[2]][[1]],MASSvTfits.l[[3]][[1]])))
MASSvTfits$location <- levels(RGRdat$location)
MASSvTfitsCI <- data.frame(do.call(rbind,
                                list(MASSvTfits.l[[1]][[3]],MASSvTfits.l[[2]][[3]],MASSvTfits.l[[3]][[3]])))
names(MASSvTfitsCI) <- c("totdmref.CI","Topt.CI","theta.CI")
MASSvTfits <- cbind(MASSvTfits,MASSvTfitsCI)

#- AGR
AGRvTfits <- data.frame(do.call(rbind,
                                list(AGRvTfits.l[[1]][[1]],AGRvTfits.l[[2]][[1]],AGRvTfits.l[[3]][[1]])))
AGRvTfits$location <- levels(RGRdat$location)
AGRvTfitsCI <- data.frame(do.call(rbind,
                                   list(AGRvTfits.l[[1]][[3]],AGRvTfits.l[[2]][[3]],AGRvTfits.l[[3]][[3]])))
names(AGRvTfitsCI) <- c("AGRref.CI","Topt.CI","theta.CI")
AGRvTfits <- cbind(AGRvTfits,AGRvTfitsCI)

#-RGR
RGRvTfits <- data.frame(do.call(rbind,
                                list(RGRvTfits.l[[1]][[1]],RGRvTfits.l[[2]][[1]],RGRvTfits.l[[3]][[1]])))
RGRvTfits$location <- levels(RGRdat$location)
RGRvTfitsCI <- data.frame(do.call(rbind,
                                  list(RGRvTfits.l[[1]][[3]],RGRvTfits.l[[2]][[3]],RGRvTfits.l[[3]][[3]])))
names(RGRvTfitsCI) <- c("RGRref.CI","Topt.CI","theta.CI")
RGRvTfits <- cbind(RGRvTfits,RGRvTfitsCI)


#- AvT short term at high PAR
AvTfits <- data.frame(do.call(rbind,
                              list(AvTfits.list.st[[1]][[1]],AvTfits.list.st[[2]][[1]],AvTfits.list.st[[3]][[1]])))
AvTfitsCI <- data.frame(do.call(rbind,
                              list(AvTfits.list.st[[1]][[3]],AvTfits.list.st[[2]][[3]],AvTfits.list.st[[3]][[3]])))
names(AvTfitsCI) <- c("Aref.CI","Topt.CI","theta.CI")
AvTfits <- cbind(AvTfits,AvTfitsCI)
AvTfits$location <- levels(RGRdat$location)

#- AvT short term at low PAR
AvTfits_lowQ <- data.frame(do.call(rbind,
                                   list(AvTfits.list2.st[[1]][[1]],AvTfits.list2.st[[2]][[1]],AvTfits.list2.st[[3]][[1]])))
AvTfits_lowQ$location <- levels(RGRdat$location)
AvTfits_lowQCI <- data.frame(do.call(rbind,
                                  list(AvTfits.list2.st[[1]][[3]],AvTfits.list2.st[[2]][[3]],AvTfits.list2.st[[3]][[3]])))
names(AvTfits_lowQCI) <- c("Photoref.CI","Topt.CI","theta.CI")
AvTfits_lowQ <- cbind(AvTfits_lowQ,AvTfits_lowQCI)

#- AvT long term at high PAR
AvTfits_longterm <- data.frame(do.call(rbind,
                              list(AvTfits.list.longterm[[1]][[1]],AvTfits.list.longterm[[2]][[1]],AvTfits.list.longterm[[3]][[1]])))
AvTfits_longtermQCI <- data.frame(do.call(rbind,
                                     list(AvTfits.list.longterm[[1]][[3]],AvTfits.list.longterm[[2]][[3]],AvTfits.list.longterm[[3]][[3]])))
names(AvTfits_longtermQCI) <- c("Aref.CI","Topt.CI","theta.CI")
AvTfits_longterm <- cbind(AvTfits_longterm,AvTfits_longtermQCI)

#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#function to take vectors of "location", a response variable, and it's error term, and return a
#  bit of a table
mktable <- function(location,yvar,se,nchar1=1,nchar2=1,type="CI"){
  text1 <- ifelse(nchar1==1,"%.1f","%.2f")
  text2 <- ifelse(nchar2==1,"%.1f","%.2f")
  
  if(type == "SE"){
      vec <- paste(sprintf(text1,round(yvar,nchar1))," (",sprintf(text2,round(se,nchar2)),")",sep="")
      #names(vec) <- location
  }
  
  if(type == "CI"){
    vec <- paste(sprintf(text1,round(yvar,nchar1))," (",se,")",sep="")
    #names(vec) <- location
  }
  
  return(vec)
}
#-----------------------------------------------------------------------------------------




# make a table 
table1 <- data.frame(Variable=c("Final mass","AGR","RGR","Asat-growth","Asat-ST","Anet-ST"),
                     Topt_Cold <- NA,Topt_Cent <- NA,Topt_Hot<-NA,
                     Rref_Cold <- NA, Rref_Cent<- NA,Rref_Hot <- NA,
                     Omega_Cold <- NA, Omega_Cent <- NA, Omega_Hot <- NA)
table1[1,c(2,3,4)] <- mktable(location=MASSvTfits$location,yvar=MASSvTfits$Topt,se=MASSvTfits$Topt.CI)                     
table1[1,c(5,6,7)] <- mktable(location=MASSvTfits$location,yvar=MASSvTfits$totdmref,se=MASSvTfits$totdmref.CI,nchar1=2,nchar2=2)
table1[1,c(8,9,10)] <- mktable(location=MASSvTfits$location,yvar=MASSvTfits$theta,se=MASSvTfits$theta.CI)   
table1[2,c(2,3,4)] <- mktable(location=AGRvTfits$location,yvar=AGRvTfits$Topt,se=AGRvTfits$Topt.CI)                     
table1[2,c(5,6,7)] <- mktable(location=AGRvTfits$location,yvar=AGRvTfits$AGRref,se=AGRvTfits$AGRref.CI,nchar1=2,nchar2=2)
table1[2,c(8,9,10)] <- mktable(location=AGRvTfits$location,yvar=AGRvTfits$theta,se=AGRvTfits$theta.CI)                    
table1[3,c(2,3,4)] <- mktable(location=RGRvTfits$location,yvar=RGRvTfits$Topt,se=RGRvTfits$Topt.CI)                     
table1[3,c(5,6,7)] <- mktable(location=RGRvTfits$location,yvar=RGRvTfits$RGRref,se=RGRvTfits$RGRref.CI,nchar1=2,nchar2=2)
table1[3,c(8,9,10)] <- mktable(location=RGRvTfits$location,yvar=RGRvTfits$theta,se=RGRvTfits$theta.CI)                    
table1[4,c(2,3,4)] <- mktable(location=AvTfits_longterm$location,yvar=AvTfits_longterm$Topt,se=AvTfits_longterm$Topt.CI)                    
table1[4,c(5,6,7)] <- mktable(location=AvTfits_longterm$location,yvar=AvTfits_longterm$Aref,se=AvTfits_longterm$Aref.CI)                    
table1[4,c(8,9,10)] <- mktable(location=AvTfits_longterm$location,yvar=AvTfits_longterm$theta,se=AvTfits_longterm$theta.CI)                    
table1[5,c(2,3,4)] <- mktable(location=AvTfits$location,yvar=AvTfits$Topt,se=AvTfits$Topt.CI)                    
table1[5,c(5,6,7)] <- mktable(location=AvTfits$location,yvar=AvTfits$Aref,se=AvTfits$Aref.CI)                    
table1[5,c(8,9,10)] <- mktable(location=AvTfits$location,yvar=AvTfits$theta,se=AvTfits$theta.CI)                    
table1[6,c(2,3,4)] <- mktable(location=AvTfits_lowQ$location,yvar=AvTfits_lowQ$Topt,se=AvTfits_lowQ$Topt.CI)                    
table1[6,c(5,6,7)] <- mktable(location=AvTfits_lowQ$location,yvar=AvTfits_lowQ$Photoref,se=AvTfits_lowQ$Photoref.CI )                    
table1[6,c(8,9,10)] <- mktable(location=AvTfits_lowQ$location,yvar=AvTfits_lowQ$theta,se=AvTfits_lowQ$theta.CI) 


table2 <- data.frame(Variable=c(rep("Final mass",3),rep("AGR",3),
                                rep("RGR",3),rep("Asat-growth",3),
                                rep("Asat-ST",3),rep("Anet-ST",3)),
                     location=rep(c("Cold","Central","Warm"),6),
                     Topt = NA, Rref = NA, omega=NA)
table2[1:3,3] <- mktable(location=MASSvTfits$location,yvar=MASSvTfits$Topt,se=MASSvTfits$Topt.CI)                     
table2[1:3,4] <- mktable(location=MASSvTfits$location,yvar=MASSvTfits$totdmref,se=MASSvTfits$totdmref.CI,nchar1=2,nchar2=2)
table2[1:3,5] <- mktable(location=MASSvTfits$location,yvar=MASSvTfits$theta,se=MASSvTfits$theta.CI)   
table2[4:6,3] <- mktable(location=AGRvTfits$location,yvar=AGRvTfits$Topt,se=AGRvTfits$Topt.CI)                     
table2[4:6,4] <- mktable(location=AGRvTfits$location,yvar=AGRvTfits$AGRref,se=AGRvTfits$AGRref.CI,nchar1=2,nchar2=2)
table2[4:6,5] <- mktable(location=AGRvTfits$location,yvar=AGRvTfits$theta,se=AGRvTfits$theta.CI)                    
table2[7:9,3] <- mktable(location=RGRvTfits$location,yvar=RGRvTfits$Topt,se=RGRvTfits$Topt.CI)                     
table2[7:9,4] <- mktable(location=RGRvTfits$location,yvar=RGRvTfits$RGRref,se=RGRvTfits$RGRref.CI,nchar1=2,nchar2=2)
table2[7:9,5] <- mktable(location=RGRvTfits$location,yvar=RGRvTfits$theta,se=RGRvTfits$theta.CI)                    
table2[10:12,3] <- mktable(location=AvTfits_longterm$location,yvar=AvTfits_longterm$Topt,se=AvTfits_longterm$Topt.CI)                    
table2[10:12,4] <- mktable(location=AvTfits_longterm$location,yvar=AvTfits_longterm$Aref,se=AvTfits_longterm$Aref.CI)                    
table2[10:12,5] <- mktable(location=AvTfits_longterm$location,yvar=AvTfits_longterm$theta,se=AvTfits_longterm$theta.CI)                    
table2[13:15,3] <- mktable(location=AvTfits$location,yvar=AvTfits$Topt,se=AvTfits$Topt.CI)                    
table2[13:15,4] <- mktable(location=AvTfits$location,yvar=AvTfits$Aref,se=AvTfits$Aref.CI)                    
table2[13:15,5] <- mktable(location=AvTfits$location,yvar=AvTfits$theta,se=AvTfits$theta.CI)                    
table2[16:18,3] <- mktable(location=AvTfits_lowQ$location,yvar=AvTfits_lowQ$Topt,se=AvTfits_lowQ$Topt.CI)                    
table2[16:18,4] <- mktable(location=AvTfits_lowQ$location,yvar=AvTfits_lowQ$Photoref,se=AvTfits_lowQ$Photoref.CI )                    
table2[16:18,5] <- mktable(location=AvTfits_lowQ$location,yvar=AvTfits_lowQ$theta,se=AvTfits_lowQ$theta.CI) 


write.csv(table2,file="output/Table1.csv",row.names=F)
