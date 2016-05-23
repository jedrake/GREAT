#-------------------------------------------------------------------------------------------------------------------
# This script is basically identical to "photo_AvT.R", except that it analyzes MASS-specific photosynthetic rates
#-------------------------------------------------------------------------------------------------------------------

#- get the AvT data
avt <- getAvT()


#- merge in the leaf mass data
leaf <- read.csv("data/GHS39_GREAT_MAIN_GX-LEAVES_20160211_L2.csv")
leaf$Comment <- NULL
#leaf <- leaf[,c("Code","Leafarea","Leafmass")]# 

avt <- merge(avt,leaf,by=c("Code","Prov","W_treatment"))
avt$Photo_m <- avt$Photo*1000/10000*(avt$Leafarea/avt$Leafmass)



#-------------------------------------------------------
#-------------------------------------------------------
#- fit AvT to estimate Topts at high light only
tofit <- subset(avt, LightFac==4)
tofit.l <- split(tofit,tofit$location)

#- fit all the curves
AvTfits.list.st <- lapply(tofit.l,FUN=fitJuneT,namex="Tleaf",namey="Photo_m",lengthPredict=20,
                          start=list(Rref=500,Topt=30,theta=20))


#- pull out the parameter means and SE's for plotting
AvTfits <- data.frame(do.call(rbind,
                              list(AvTfits.list.st[[1]][[1]],AvTfits.list.st[[2]][[1]],AvTfits.list.st[[3]][[1]])))

windows(30,60);par(mfrow=c(3,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
#- plot Asat at Topt
barplot2(height=AvTfits$Photo_mref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,800),las=1,
         ylab=expression(A[ref]~(mmol~g^-1~s^-1)),
         ci.l=AvTfits$Photo_mref-AvTfits$Photo_mref.se,ci.u=AvTfits$Photo_mref+AvTfits$Photo_mref.se)
title(main="Short-term data")

#- plot Topt
barplot2(height=AvTfits$Topt,names.arg=c("A","B","C"),plot.ci=T,las=1,ylim=c(0,30),
         ylab=expression(T[opt]~(degree*C)),
         ci.l=AvTfits$Topt-AvTfits$Topt.se,ci.u=AvTfits$Topt+AvTfits$Topt.se)
#- plot Theta
barplot2(height=AvTfits$theta,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,25),las=1,
         ylab=expression(Omega~(degree*C)),
         ci.l=AvTfits$theta-AvTfits$theta.se,ci.u=AvTfits$theta+AvTfits$theta.se)
title(xlab="Provenance",outer=T,cex.lab=2,adj=0.6)
dev.copy2pdf(file="output/AvTshorttermfits_massSpecific.pdf")


#- pull out the predictions and confidence intervals for plotting
highQ <- data.frame(do.call(rbind,
                            list(AvTfits.list.st[[1]][[2]],AvTfits.list.st[[2]][[2]],AvTfits.list.st[[3]][[2]])))
highQ$Prov <- c(rep("A",nrow(highQ)/3),rep("C",nrow(highQ)/3),rep("B",nrow(highQ)/3))
highQ$location <- c(rep("Cold-edge",nrow(highQ)/3),rep("Central",nrow(highQ)/3),rep("Warm-edge",nrow(highQ)/3))
highQ$location <- factor(highQ$location,levels=c("Cold-edge","Central","Warm-edge"))  





#--- fit the observations at low PAR
tofit_lowQ <- subset(avt, LightFac==1)
tofit.l_lowQ <- split(tofit_lowQ,tofit_lowQ$Prov)

#- fit Tresponse curves at low PAR
AvTfits.list2.st <- lapply(tofit.l_lowQ,FUN=fitJuneT,start=list(Rref=100,Topt=12,theta=15),namey="Photo_m",namex="Tleaf",lengthPredict=20)


#- pull out the parameter means and SE's for plotting
AvTfits_lowQ <- data.frame(do.call(rbind,
                                   list(AvTfits.list2.st[[1]][[1]],AvTfits.list2.st[[2]][[1]],AvTfits.list2.st[[3]][[1]])))
#- pull out the predictions and confidence intervals for plotting
lowQ <- data.frame(do.call(rbind,
                           list(AvTfits.list2.st[[1]][[2]],AvTfits.list2.st[[2]][[2]],AvTfits.list2.st[[3]][[2]])))
lowQ$prov <- c(rep("A",nrow(lowQ)/3),rep("B",nrow(lowQ)/3),rep("C",nrow(lowQ)/3))
lowQ$location <- c(rep("Cold-edge",nrow(lowQ)/3),rep("Warm-edge",nrow(lowQ)/3),rep("Central",nrow(lowQ)/3))
lowQ$location <- factor(lowQ$location,levels=c("Cold-edge","Central","Warm-edge"))  
#-------------------------------------------------------
#-------------------------------------------------------








#-------------------------------------------------------
#-------------------------------------------------------
#- set up the plot, with both light levels
windows(40,50);par(mar=c(3,7,1,1),mfrow=c(2,1),cex.lab=2,cex.axis=1.2,oma=c(4,0,0,0))
palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]
xlims <- c(12,40)

#-------
#- plot Asat
plotBy(Sim.Mean~Tleaf|location,data=highQ,legend=F,type="l",las=1,ylim=c(0,800),lwd=3,cex.lab=1.5,col=COL,
       ylab="",axes=F,
       xlab="")
as <- subset(highQ,Prov=="A")
bs <- subset(highQ,Prov=="B")
cs <- subset(highQ,Prov=="C")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5, rev(as$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5, rev(bs$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5, rev(cs$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)

legend("bottomright",levels(highQ$location),fill=COL,cex=1,title="Provenance")
legend("bottomleft","PPFD = 1500",bty="n")
legend("topright",letters[1],bty="n")
magaxis(side=1:4,labels=c(1,1,0,0),frame.plot=T,las=1)
title(ylab=expression(A[sat]~(mmol~g^-1~s^-1)))

#- add TREATMENT MEANS
aq.m <- summaryBy(Photo_m+Tleaf+PARi~TleafFac+LightFac+W_treatment+Prov+location,data=avt,FUN=c(mean,standard.error))
plotmeans <- subset(aq.m,LightFac==4 & W_treatment=="w")
plotBy(Photo_m.mean~Tleaf.mean|location,data=plotmeans,add=T,pch=16,cex=2,legend=F,col=COL,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Photo_m.mean,
                                 SE=plotmeans$Photo_m.standard.error,direction="updown")))
#-------



#-------
#- plot A at low Q
plotBy(Sim.Mean~Tleaf|location,data=lowQ,legend=F,type="l",las=1,ylim=c(0,150),lwd=3,cex.lab=1.5,col=COL,
       axes=F,ylab="",
       xlab="")

as2 <- subset(lowQ,prov=="A")
bs2 <- subset(lowQ,prov=="B")
cs2 <- subset(lowQ,prov=="C")
title(xlab=expression(T[leaf]~(degree*C)),outer=T,adj=0.6,line=2)
legend("bottomleft","PPFD = 100",bty="n")
legend("topright",letters[2],bty="n")
magaxis(side=1:4,labels=c(1,1,0,0),frame.plot=T,las=1)
title(ylab=expression(A[net]~(mmol~g^-1~s^-1)))

polygon(x = c(as2$Tleaf, rev(as2$Tleaf)), y = c(as2$Sim.97.5, rev(as2$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs2$Tleaf, rev(bs2$Tleaf)), y = c(bs2$Sim.97.5, rev(bs2$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)
polygon(x = c(cs2$Tleaf, rev(cs2$Tleaf)), y = c(cs2$Sim.97.5, rev(cs2$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)

#- add TREATMENT MEANS
plotmeans2 <- subset(aq.m,LightFac==1 & W_treatment=="w")
plotBy(Photo_m.mean~Tleaf.mean|location,data=plotmeans2,add=T,pch=16,cex=2,legend=F,col=COL,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Photo_m.mean,
                                 SE=plotmeans$Photo_m.standard.error,direction="updown")))
dev.copy2pdf(file="output/Asat_Anet_CIs_massSpecific.pdf")
#-------------------------------------------------------
#-------------------------------------------------------







