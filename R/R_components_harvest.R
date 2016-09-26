#------------------------------------------------------------------------------------------------
#- This script process the leaf, stem, and root respiration data we measured at the end of the study
#  and makes two plots. (1) The actual respiration data measured at 25 deg C.
#  (2) Estiamted tissue-specific and whole-plant respiration rates at in-situ temperatures
#------------------------------------------------------------------------------------------------

#- get the data
Rdat <- returnRcomponents()

#- average across provenances in each room
Rdat.m <- summaryBy(Rarea+Rmass+Rmass_insitu+Rarea_insitu~Room+Tair+Prov+W_treatment+Organ+location,data=Rdat,FUN=c(mean,standard.error),na.rm=T)
#Rdat.m$Rarea.standard.error[which(is.na(Rdat.m$Rarea.standard.error))] <- 0




#- process data for in-situ estimates. Convert rates to mmol CO2 g-1 day-1
# Rdat$Rtotal_insitu <- NA
# Rdat$Rtotal_insitu[leaves] <- Rdat$Rmass_insitu[leaves]*Rdat$leafdm[leaves]/1000*60*60*24*1e-6  # all masses were recorded in mg
# Rdat$Rtotal_insitu[stems] <- Rdat$Rmass_insitu[stems]*Rdat$Stemmass[stems]/1000*60*60*24*1e-6
# Rdat$Rtotal_insitu[roots] <- Rdat$Rmass_insitu[roots]*Rdat$totalRoot[roots]/1000*60*60*24*1e-6

#- average across locations etc.
Rdat_mean_insitu <- summaryBy(Rmass_insitu~Prov+location+Tair+Organ,
                            FUN=c(mean,standard.error),data=subset(Rdat,W_treatment=="w"),na.rm=T)


#------------------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#- plot treatment average respiration rates
toplot <- subset(Rdat.m,W_treatment=="w")
toplot$Room <- as.numeric(toplot$Room)

windows(60,60);par(mfrow=c(3,2),mar=c(0.5,0.5,0.5,0.5),oma=c(5,8,1,8),cex.lab=2)
palette(rev(brewer.pal(6,"Spectral")))
COL=palette()[c(1,2,6)]



#----------------------------------
#--- Leaves

#- plot Rmass for leaves, smoothplots
smoothplot(Tair, Rmass, location,kgam=6,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           #polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,40),xlab="",ylab="",
           data=subset(Rdat, Organ == "leaf"), axes=F)

#- overlay points
plotBy(Rmass.mean~Tair|location,data=subset(toplot,Organ=="leaf"),las=1,ylim=c(0,40),legend=F,pch=16,cex=0.5,col=COL,add=T,
       panel.first=adderrorbars(x=subset(toplot,Organ=="leaf")$Tair,
                                y=subset(toplot,Organ=="leaf")$Rmass.mean,
                                SE=subset(toplot,Organ=="leaf")$Rmass.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=2)
title(ylab=expression(atop(Leaf~R[mass]~at~25~degree*C,
                     (nmol~CO[2]~g^-1~s^-1))),xpd=NA)
palette(COL) 
points(Rmass.mean~Tair,data=subset(toplot,Organ=="leaf"),add=T,pch=21,cex=2,legend=F,col="black",bg=location)


magaxis(side=1:4,labels=c(0,1,0,0),las=1)
legend("bottomleft",letters[1],cex=1.2,bty="n")
legend("topright",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1.2,title="Provenance",bty="n")



#- Tissue specific Rmass for leaf
smoothplot(Tair, Rmass_insitu.mean , location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,40),xlab="",ylab="",
           data=subset(Rdat_mean_insitu,Organ=="leaf"), kgam=4,axes=F)
#- overlay points
plotBy(Rmass_insitu.mean~Tair|location,data=subset(Rdat_mean_insitu,Organ=="leaf"),las=1,ylim=c(0,3),xlim=c(15,37),add=T,
       legend=F,pch=16,cex=0.5,col=COL,
       panel.first=adderrorbars(x=subset(Rdat_mean_insitu,Organ=="leaf")$Tair,
                                y=subset(Rdat_mean_insitu,Organ=="leaf")$Rmass_insitu.mean,
                                SE=subset(Rdat_mean_insitu,Organ=="leaf")$Rmass_insitu.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=1.5)
points(Rmass_insitu.mean~Tair,data=subset(Rdat_mean_insitu,Organ=="leaf"),add=T,pch=21,cex=2,legend=F,col="black",bg=location)

title(ylab=expression(atop(Leaf~R[mass]~at~T[growth],
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA,line=-27,srt=90)
magaxis(side=1:4,labels=c(0,0,0,1),las=1)
legend("bottomleft",letters[4],bty="n",cex=1.2)
#----------------------------------



#----------------------------------
#--- stems

#----
#- Rmass for stems smoothplots
smoothplot(Tair, Rmass, location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,40),xlab="",ylab="",
           data=subset(Rdat, Organ == "stem"), kgam=6,axes=F)
#- overlay points
plotBy(Rmass.mean~Tair|location,data=subset(toplot,Organ=="stem"),las=1,ylim=c(0,40),legend=F,pch=16,cex=0.5,col=COL,add=T,
       panel.first=adderrorbars(x=subset(toplot,Organ=="stem")$Tair,
                                y=subset(toplot,Organ=="stem")$Rmass.mean,
                                SE=subset(toplot,Organ=="stem")$Rmass.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=2)
points(Rmass.mean~Tair,data=subset(toplot,Organ=="stem"),add=T,pch=21,cex=2,legend=F,col="black",bg=location)
title(ylab=expression(atop(Stem~R[mass]~at~25~degree*C,
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA)
magaxis(side=1:4,labels=c(0,1,0,0),las=1)
legend("bottomleft",letters[2],cex=1.2,bty="n")




#- Tissue specific Rmass for stem
smoothplot(Tair, Rmass_insitu.mean , location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,40),xlab="",ylab="",
           data=subset(Rdat_mean_insitu,Organ=="stem"), kgam=6,axes=F)
#- overlay points
plotBy(Rmass_insitu.mean~Tair|location,data=subset(Rdat_mean_insitu,Organ=="stem"),las=1,ylim=c(0,3),xlim=c(15,37),add=T,
       legend=F,pch=16,cex=0.5,col=COL,
       panel.first=adderrorbars(x=subset(Rdat_mean_insitu,Organ=="stem")$Tair,
                                y=subset(Rdat_mean_insitu,Organ=="stem")$Rmass_insitu.mean,
                                SE=subset(Rdat_mean_insitu,Organ=="stem")$Rmass_insitu.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=1.5)
points(Rmass_insitu.mean~Tair,data=subset(Rdat_mean_insitu,Organ=="stem"),add=T,pch=21,cex=2,legend=F,col="black",bg=location)

title(ylab=expression(atop(Stem~R[mass]~at~T[growth],
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA,line=-27,srt=90)
magaxis(side=1:4,labels=c(0,0,0,1),las=1)
legend("bottomleft",letters[5],bty="n",cex=1.2)
#----------------------------------





#----------------------------------
#--- Roots

#- Rmass for roots
smoothplot(Tair, Rmass, location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,40),xlab="",ylab="",
           data=subset(Rdat, Organ == "root"), kgam=4,axes=F)
#- overlay points
plotBy(Rmass.mean~Tair|location,data=subset(toplot,Organ=="root"),las=1,ylim=c(0,40),legend=F,pch=16,cex=0.5,col=COL,add=T,
       panel.first=adderrorbars(x=subset(toplot,Organ=="root")$Tair,
                                y=subset(toplot,Organ=="root")$Rmass.mean,
                                SE=subset(toplot,Organ=="root")$Rmass.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=2)
points(Rmass.mean~Tair,data=subset(toplot,Organ=="root"),add=T,pch=21,cex=2,legend=F,col="black",bg=location)

title(ylab=expression(atop(Root~R[mass]~at~25~degree*C,
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA)
magaxis(side=1:4,labels=c(1,1,0,0),las=1)
legend("bottomleft",letters[3],cex=1.2,bty="n")

#- Tissue specific Rmass for roots
smoothplot(Tair, Rmass_insitu.mean , location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,40),xlab="",ylab="",
           data=subset(Rdat_mean_insitu,Organ=="root"), kgam=5,axes=F)
#- overlay points
plotBy(Rmass_insitu.mean~Tair|location,data=subset(Rdat_mean_insitu,Organ=="root"),las=1,ylim=c(0,3),xlim=c(15,37),add=T,
       legend=F,pch=16,cex=0.5,col=COL,
       panel.first=adderrorbars(x=subset(Rdat_mean_insitu,Organ=="root")$Tair,
                                y=subset(Rdat_mean_insitu,Organ=="root")$Rmass_insitu.mean,
                                SE=subset(Rdat_mean_insitu,Organ=="root")$Rmass_insitu.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=1.5)
points(Rmass_insitu.mean~Tair,data=subset(Rdat_mean_insitu,Organ=="root"),add=T,pch=21,cex=2,legend=F,col="black",bg=location)

title(ylab=expression(atop(Root~R[mass]~at~T[growth],
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA,line=-27,srt=90)
magaxis(side=1:4,labels=c(1,0,0,1),las=1)
legend("bottomleft",letters[6],bty="n",cex=1.2)
#----------------------------------


title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.2,line=3)

title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.9,line=3)
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

#boxplot(Rtotal_insitu_mol~location+Tair,data=subset(Rdat_sum,W_treatment=="w"))
#boxplot(Rmass_insitu_mmol~location+Tair,data=subset(Rdat_mean,W_treatment=="w"))

#- average across locations etc. Improve the averaging to reflect a mean weighted by the relative proportion of each organ.
Rdat_sum_mean <- summaryBy(Rtotal_insitu_mol~prov+Water_trt+location+Tair,
                           FUN=c(mean,standard.error),data=subset(Rdat_sum,W_treatment=="w"),na.rm=T)
Rdat_mean_mean <- summaryBy(Rmass_insitu_mmol~prov+Water_trt+location+Tair,
                           FUN=c(mean,standard.error),data=subset(Rdat_mean,W_treatment=="w"),na.rm=T)

if(plotboth==T){
#---------------------
#- make the plot
windows(40,70);par(mfrow=c(2,1),mar=c(2,7,0,0),oma=c(3,1,3,2),cex.lab=1.5)


#------
#- Tissue specific Rmass
smoothplot(Tair, Rmass_insitu_mmol.mean, location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,2.5),xlab="",ylab="",
           data=Rdat_mean_mean, kgam=4,axes=F)
#- overlay points
plotBy(Rmass_insitu_mmol.mean~Tair|location,data=Rdat_mean_mean,las=1,ylim=c(0,3),xlim=c(15,37),add=T,
       legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=Rdat_mean_mean$Tair,
                                y=Rdat_mean_mean$Rmass_insitu_mmol.mean,
                                SE=Rdat_mean_mean$Rmass_insitu_mmol.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=1.5)
title(ylab=expression(atop(R[mass],
                           (mmol~CO[2]~g^-1~d^-1))),xpd=NA)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",letters[1],bty="n",cex=1.2)
legend("bottomright",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1,title="Provenance",bty="n")

#legend("bottomright",xpd=NA,legend=levels(Rdat_sum_mean$location),pch=16,col=COL[1:3],ncol=1,bty="n",cex=1.2)

#------
#- Actual total Rmass, given total mass
smoothplot(Tair, Rtotal_insitu_mol.mean, location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
           polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
           linecol=c("black","red"),pointcols=NA,
           cex=1,main="",
           xlim=c(15,40),ylim=c(0,25),xlab="",ylab="",
           data=Rdat_sum_mean, kgam=4,axes=F)

plotBy(Rtotal_insitu_mol.mean~Tair|location,data=Rdat_sum_mean,las=1,ylim=c(0,25),xlim=c(15,37),add=T,
       legend=F,pch=16,cex=2,col=COL,
       panel.first=adderrorbars(x=Rdat_sum_mean$Tair,
                                y=Rdat_sum_mean$Rtotal_insitu_mol.mean,
                                SE=Rdat_sum_mean$Rtotal_insitu_mol.standard.error,direction="updown"),
       axes=F,xlab="Tair",cex.lab=1.5)
title(ylab=expression(atop(R[total],
                      (mmol~CO[2]~d^-1))),xpd=NA)
magaxis(side=1:4,labels=c(1,1,0,1),las=1)
legend("topright",letters[2],bty="n",cex=1.2)

title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.95,line=1)

dev.copy2pdf(file="output/Rtotal_daily.pdf")
}
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------







if(plotboth==F){
  #---------------------
  #- make the plot
  windows(40,50);par(mar=c(6,7,0,1),oma=c(0,0,1,1),cex.lab=1.5)
  
  
  #------
  #- Tissue specific Rmass
  smoothplot(Tair, Rmass_insitu_mmol.mean, location,linecols=c(alpha(COL[1],1),alpha(COL[2],1),alpha(COL[3],1)),
             polycolor=c(alpha(COL[1],0.3),alpha(COL[2],0.3),alpha(COL[3],0.3)),
             linecol=c("black","red"),pointcols=NA,
             cex=1,main="",
             xlim=c(15,40),ylim=c(0,2.5),xlab="",ylab="",
             data=Rdat_mean_mean, kgam=4,axes=F)
  #- overlay points
  plotBy(Rmass_insitu_mmol.mean~Tair|location,data=Rdat_mean_mean,las=1,ylim=c(0,3),xlim=c(15,37),add=T,
         legend=F,pch=16,cex=2,col=COL,
         panel.first=adderrorbars(x=Rdat_mean_mean$Tair,
                                  y=Rdat_mean_mean$Rmass_insitu_mmol.mean,
                                  SE=Rdat_mean_mean$Rmass_insitu_mmol.standard.error,direction="updown"),
         axes=F,xlab="Tair",cex.lab=1.5)
  title(ylab=expression(atop(R[mass],
                             (mmol~CO[2]~g^-1~d^-1))),xpd=NA)
  magaxis(side=1:4,labels=c(1,1,0,1),las=1)
  #legend("topright",letters[1],bty="n",cex=1.2)
  legend("bottomright",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1,title="Provenance",bty="n")
  
  title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=F,adj=0.5,line=3)
  
  dev.copy2pdf(file="output/Rtotal_daily_tissue_specific.pdf")
}