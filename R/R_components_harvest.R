#------------------------------------------------------------------------------------------------
#- This script process the leaf, stem, and root respiration data we measured at the end of the study
#  and makes two plots. (1) The actual respiration data measured at 25 deg C.
#  (2) Estiamted tissue-specific and whole-plant respiration rates at in-situ temperatures
#------------------------------------------------------------------------------------------------



#- read in the respiration data
path <- "W://WORKING_DATA/GHS39/GREAT"
Rdat1 <- read.csv(paste(path,"/Share/Data/GasEx/R/GHS39_GREAT_MAIN_GX-R_20160217-20160224_L2.csv",sep=""))

#- average over subreplicate logs
Rdat2 <- summaryBy(dCO2+Photo+Cond+Ci+Trmmol+VpdL+Area~Date+Organ+Code+Unit+W_treatment,data=Rdat1,FUN=mean,keep.names=T)

# #- reformat the "Pot" variable. E.g, change "A-01" to "A-1" to match the size datasets.
# crap <- unlist(strsplit(as.character(Rdat2$Pot), "-"))
# Rdat2$prov <- as.factor(crap[seq(1,length(crap)-1,2)])
# Rdat2$pot <- as.factor(paste(Rdat2$prov,as.numeric(crap[seq(0,length(crap),2)]),sep="-"))
# Rdat2$prov <- NULL #- remove the "prov" variable. It gets added later with getSize().
# Rdat2$Pot <- NULL

#------------------------------------------------------------------------------------------------
#- merge in the harvest mass data, to calculate mass-specific respiration rates
finalHarvest <- read.csv(paste(path,"/Share/Data/Harvests/GHS39_GREAT_MAIN_BIOMASS_20160217-20160224_L2.csv",sep=""))
finalHarvest <- finalHarvest[,c("Code","Leafarea","Leafarea_sub","Leafmass","Leafmass_sub","Stemmass","Rootmass","Rootmass_sub")]
finalHarvest$totalRoot <- rowSums(finalHarvest[,c("Rootmass","Rootmass_sub")],na.rm=T)
finalHarvest$leafdm <- rowSums(finalHarvest[,c("Leafmass","Leafmass_sub")],na.rm=T)

#names(finalHarvest)[2] <- "leafArea"

Rdat3 <- merge(Rdat2,finalHarvest,by.x=c("Code"),by.y=c("Code"))

#- calculate mass-based respiration rates
Rdat3$Rmass <- NA
leaves <- which(Rdat3$Organ=="leaf") #- find the indexes of the leaf measurements
stems <- which(Rdat3$Organ=="stem") #- find the indexes of the stem measurements
roots <- which(Rdat3$Organ=="root") #- find the indexes of the root measurements

#- sort out the samples where the measured root mass was not different than the root total mass
rootstofix <- which(is.na(Rdat3$Rootmass_sub)==T & is.na(Rdat3$Rootmass)==F)
Rdat3$Rootmass_sub[rootstofix] <- Rdat3$Rootmass[rootstofix]  

Rdat3$Rmass[leaves] <- Rdat3$Photo[leaves]*-1*Rdat3$Leafarea_sub[leaves]/10000/Rdat3$Leafmass_sub[leaves]*1000*1000 # nmol CO2 g-1 s-1
Rdat3$Rmass[stems] <- Rdat3$Photo[stems]*-1*Rdat3$Area[stems]/10000/Rdat3$Stemmass[stems]*1000*1000 # nmol CO2 g-1 s-1
Rdat3$Rmass[roots] <- Rdat3$Photo[roots]*-1*Rdat3$Area[roots]/10000/Rdat3$Rootmass_sub[roots]*1000*1000 # nmol CO2 g-1 s-1

#------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------
#- merge in the size data (just to get things like room numbers and drought treatments, etc)
size <- getSize()
size <- size[,c("Code","Room","Prov","W_treatment","location")]
size <- unique(size)

#- merge in room temperature key
key <- data.frame(Room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
size2 <- merge(size,key)
Rdat <- merge(Rdat3,size2,by=c("Code","W_treatment"))
Rdat$Date <- as.Date(as.character(Rdat$Date),format="%Y%m%d")
Rdat$Rarea <- -1*Rdat$Photo

#- predict respiration at growth temperature
Rdat$Rmass_insitu <- with(Rdat,Rmass*2^((Tair-25)/10))
Rdat$Rarea_insitu <- with(Rdat,Rarea*2^((Tair-25)/10))

#- average across provenances in each room
Rdat.m <- summaryBy(Rarea+Rmass+Rmass_insitu+Rarea_insitu~Room+Tair+Prov+W_treatment+Organ+location,data=Rdat,FUN=c(mean,standard.error),na.rm=T)
#Rdat.m$Rarea.standard.error[which(is.na(Rdat.m$Rarea.standard.error))] <- 0
#------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#- plot treatment average respiration rates
toplot <- subset(Rdat.m,W_treatment=="w")
toplot$Room <- as.numeric(toplot$Room)

windows(30,70);par(mfrow=c(4,1),mar=c(2,7,1,0),oma=c(3,1,1,2),cex.lab=2)
palette(rev(brewer.pal(6,"Spectral")))
COL=palette()[c(1,2,6)]

#- Rarea for leaves
plotBy(Rarea.mean~Tair|location,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,2.5),legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rarea.mean,
                                SE=subset(toplot,Organ=="leaf")$Rarea.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(Leaf~R[area],
                                   (mu*mol~CO[2]~m^-2~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",xpd=NA,legend=levels(toplot$location),pch=16,col=COL[1:3],ncol=1,bty="n",cex=1.2)
legend("bottomleft",letters[1],cex=1.2,bty="n")

#- Rmass for leaves
plotBy(Rmass.mean~Tair|location,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rmass.mean,
                                SE=subset(toplot,Organ=="leaf")$Rmass.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(Leaf~R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("bottomleft",letters[2],cex=1.2,bty="n")

#- Rmass for stems
plotBy(Rmass.mean~Tair|location,data=subset(toplot,Organ=="stem"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=subset(toplot,Organ=="stem")$Tair,
                                y=subset(toplot,Organ=="stem")$Rmass.mean,
                                SE=subset(toplot,Organ=="stem")$Rmass.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(Stem~R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("bottomleft",letters[3],cex=1.2,bty="n")


#- Rmass for roots
plotBy(Rmass.mean~Tair|location,data=subset(toplot,Organ=="root"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=subset(toplot,Organ=="root")$Tair,
                                y=subset(toplot,Organ=="root")$Rmass.mean,
                                SE=subset(toplot,Organ=="root")$Rmass.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(Root~R[mass],
                                   (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("bottomleft",letters[4],cex=1.2,bty="n")

title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.9,line=1)
dev.copy2pdf(file="output/Rcomponents.pdf")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------







# 
# #------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------
# #- plot treatment average respiration rates, estimated for in-situ conditions
# 
# windows(30,70);par(mfrow=c(4,1),mar=c(2,7,1,0),oma=c(3,1,3,2),cex.lab=2)
# 
# #- Rarea for leaves
# plotBy(Rarea_insitu.mean~Tair|prov,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,2),legend=F,pch=16,cex=2,
#        panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
#                                 y=subset(toplot,Organ=="leaf")$Rarea_insitu.mean,
#                                 SE=subset(toplot,Organ=="leaf")$Rarea_insitu.standard.error,direction="updown"),
#        axes=F,ylab=expression(atop(R[area],
#                                    (mu*mol~CO[2]~m^-2~s^-1))),xlab="Tair",cex.lab=1.5)
# magaxis(side=1:4,labels=c(1,1,0,1),las=1)
# legend("topleft",legend="Leaf",bty="n",cex=2)
# legend(x=22,y=2.7,xpd=NA,legend=c("A","B","C"),pch=16,col=palette()[1:3],ncol=3,bty="n",cex=1.5)
# #- Rmass for leaves
# plotBy(Rmass_insitu.mean~Tair|prov,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
#        panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
#                                 y=subset(toplot,Organ=="leaf")$Rmass_insitu.mean,
#                                 SE=subset(toplot,Organ=="leaf")$Rmass_insitu.standard.error,direction="updown"),
#        axes=F,ylab=expression(atop(R[mass],
#                                    (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
# magaxis(side=1:4,labels=c(1,1,0,1),las=1)
# legend("topleft",legend="Leaf",bty="n",cex=2)
# 
# #- Rmass for stems
# plotBy(Rmass_insitu.mean~Tair|prov,data=subset(toplot,Organ=="stem"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
#        panel.first=adderrorbars(x=subset(toplot,Organ=="stem")$Tair,
#                                 y=subset(toplot,Organ=="stem")$Rmass_insitu.mean,
#                                 SE=subset(toplot,Organ=="stem")$Rmass_insitu.standard.error,direction="updown"),
#        axes=F,ylab=expression(atop(R[mass],
#                                    (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
# magaxis(side=1:4,labels=c(1,1,0,1),las=1)
# legend("topleft",legend="Stem",bty="n",cex=2)
# 
# 
# #- Rmass for roots
# plotBy(Rmass_insitu.mean~Tair|prov,data=subset(toplot,Organ=="root"),las=1,ylim=c(0,40),legend=F,pch=16,cex=2,
#        panel.first=adderrorbars(x=subset(toplot,Organ=="root")$Tair,
#                                 y=subset(toplot,Organ=="root")$Rmass_insitu.mean,
#                                 SE=subset(toplot,Organ=="root")$Rmass_insitu.standard.error,direction="updown"),
#        axes=F,ylab=expression(atop(R[mass],
#                                    (nmol~CO[2]~g^-1~s^-1))),xlab="Tair",cex.lab=1.5)
# magaxis(side=1:4,labels=c(1,1,0,1),las=1)
# legend("topleft",legend="root",bty="n",cex=2)
# title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.7,line=1)
# dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/Rcomponents_insituRates.pdf")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------







#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#- sum the respiration rates of leaves, stems, and roots predicted at the growth temperature
#-  to estimate total daily R

leaves <- which(Rdat$Organ=="leaf") #- find the indexes of the leaf measurements
stems <- which(Rdat$Organ=="stem") #- find the indexes of the stem measurements
roots <- which(Rdat$Organ=="root") #- find the indexes of the root measurements

Rdat$Rtotal_insitu <- NA
Rdat$Rtotal_insitu[leaves] <- Rdat$Rmass_insitu[leaves]*Rdat$leafdm[leaves]/1000  # all masses were recorded in mg
Rdat$Rtotal_insitu[stems] <- Rdat$Rmass_insitu[stems]*Rdat$Stemmass[stems]/1000
Rdat$Rtotal_insitu[roots] <- Rdat$Rmass_insitu[roots]*Rdat$totalRoot[roots]/1000

#- sum (or average) across organs for the estimated in-situ rates for each plant
Rdat_sum <- summaryBy(Rtotal_insitu~Code+Prov+W_treatment+location+Tair,data=Rdat,FUN=sum,keep.names=T)
Rdat_mean <- summaryBy(Rmass_insitu~Code+Prov+W_treatment+location+Tair,data=Rdat,FUN=mean,keep.names=T)

Rdat_sum$Rtotal_insitu_mol <- Rdat_sum$Rtotal_insitu*60*60*24*1e-6 # convert to mmol CO2 day-1
Rdat_mean$Rmass_insitu_mmol <- Rdat_mean$Rmass_insitu*60*60*24*1e-6  # convert to mmol CO2 g-1 day-1

boxplot(Rtotal_insitu_mol~location+Tair,data=subset(Rdat_sum,W_treatment=="w"))
boxplot(Rmass_insitu_mmol~location+Tair,data=subset(Rdat_mean,W_treatment=="w"))

#- average across locations etc.
Rdat_sum_mean <- summaryBy(Rtotal_insitu_mol~prov+Water_trt+location+Tair,
                           FUN=c(mean,standard.error),data=subset(Rdat_sum,W_treatment=="w"),na.rm=T)
Rdat_mean_mean <- summaryBy(Rmass_insitu_mmol~prov+Water_trt+location+Tair,
                           FUN=c(mean,standard.error),data=subset(Rdat_mean,W_treatment=="w"),na.rm=T)


#---------------------
#- make the plot
windows(40,70);par(mfrow=c(2,1),mar=c(2,7,0,0),oma=c(3,1,3,2),cex.lab=2)

#- Tissue specific Rmass
plotBy(Rmass_insitu_mmol.mean~Tair|location,data=Rdat_mean_mean,las=1,ylim=c(0,3),xlim=c(15,37),
       legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=Rdat_mean_mean$Tair,
                                y=Rdat_mean_mean$Rmass_insitu_mmol.mean,
                                SE=Rdat_mean_mean$Rmass_insitu_mmol.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[mass],
                                   (mmol~CO[2]~g^-1~d^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topleft",xpd=NA,legend=levels(Rdat_sum_mean$location),pch=16,col=COL[1:3],ncol=1,bty="n",cex=1.2)
legend("topright",letters[1],bty="n",cex=1.2)

#- Actual total Rmass, given total mass
plotBy(Rtotal_insitu_mol.mean~Tair|location,data=Rdat_sum_mean,las=1,ylim=c(0,25),xlim=c(15,37),
       legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=Rdat_sum_mean$Tair,
                                y=Rdat_sum_mean$Rtotal_insitu_mol.mean,
                                SE=Rdat_sum_mean$Rtotal_insitu_mol.standard.error,direction="updown"),
       axes=F,ylab=expression(atop(R[total],
                                   (mmol~CO[2]~d^-1))),xlab="Tair",cex.lab=1.5)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",letters[2],bty="n",cex=1.2)

title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.95,line=1)
dev.copy2pdf(file="output/Rtotal_daily.pdf")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------