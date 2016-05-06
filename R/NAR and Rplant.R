#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Script to explore the relationship between whole-plant respiraiton and elements of RGR
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#- get the growth and harvest data, process it for RGR.
dat.list <- returnRGR(plotson=F)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.

#- get the respiratory component data
Rdat <- returnRcomponents()
#- average across organs
Rdat_mean <- summaryBy(Rmass_insitu~Code,data=Rdat,FUN=mean,keep.names=T)


#- merge RGR and Rmass data
dat2 <- subset(merge(dat,Rdat_mean,by="Code"),W_treatment=="w")
dat2$Rtotal <- with(dat2,totmass*Rmass_insitu) # estimate total plant R, in mmol d-1
dat2$Tair <- factor(dat2$Tair)

windows(40,20);par(mfrow=c(1,3),cex=1.5,cex.lab=1.5,cex.axis=1.2,mar=c(5,6,1,1))
plotBy(NAR~LAR|Tair,data=dat2,pch=16,legend=F)
plotBy(LAR~Rtotal|Tair,data=dat2,pch=16,legend=F)
plotBy(NAR~Rtotal|Tair,data=dat2,pch=16,legend=F)
plotBy(RGR~Rtotal|Tair,data=dat2,pch=16,legend=F)

windows(50,50)
pairs(dat2[,c("totmass","LAR","NAR","Rtotal","Rmass_insitu","Tair")],col=dat2$Tair,pch=16)
