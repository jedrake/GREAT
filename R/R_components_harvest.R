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

#windows(60,60);
pdf(file="output/Figure3-respiration.pdf",width=7.3,height=8)
par(mfrow=c(3,2),mar=c(0.5,0.5,0.5,0.5),oma=c(5,8,1,8),cex.lab=2)
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
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA,line=-26,srt=90)
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
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA,line=-26,srt=90)
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
                           (nmol~CO[2]~g^-1~s^-1))),xpd=NA,line=-26,srt=90)
magaxis(side=1:4,labels=c(1,0,0,1),las=1)
legend("bottomleft",letters[6],bty="n",cex=1.2)
#----------------------------------


title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.2,line=3)

title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,outer=T,adj=0.9,line=3)
dev.off()
#dev.copy2pdf(file="output/Figure3-respiration.pdf")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------



