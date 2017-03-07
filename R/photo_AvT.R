#-------------------------------------------------------
#- get the photosynthesis vs. temperature data (AvT)
avt <- getAvT()


#- merge in the leaf mass data
leaf <- read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS39_GREAT_MAIN_GX-LEAVES_20160211_L2.csv")
leaf$Comment <- NULL
#leaf <- leaf[,c("Code","Leafarea","Leafmass")]# 

avt <- merge(avt,leaf,by=c("Code","Prov","W_treatment"))
avt$Photo_m <- avt$Photo*1000/10000*(avt$Leafarea/avt$Leafmass)
#-------------------------------------------------------





#-------------------------------------------------------
#- fit AvT to estimate Topts at high light, AREA BASED
tofit <- subset(avt, LightFac==4)
tofit.l <- split(tofit,tofit$location)

#- fit all the curves
AvTfits.list.st <- lapply(tofit.l,FUN=fitJuneT,namex="Tleaf",namey="Photo",lengthPredict=20,
                          start=list(Rref=25,Topt=30,theta=20))

#- pull out the parameter means and SE's for plotting
AvTfits <- data.frame(do.call(rbind,
                              list(AvTfits.list.st[[1]][[2]],AvTfits.list.st[[2]][[2]],AvTfits.list.st[[3]][[2]])))
AvTfits$location <- c(rep("Cold-edge",nrow(AvTfits)/3),rep("Central",nrow(AvTfits)/3),rep("Warm-edge",nrow(AvTfits)/3))
AvTfits$location <- factor(AvTfits$location,levels=c("Cold-edge","Central","Warm-edge"))  

#-------------------------------------------------------



#-------------------------------------------------------
#- fit AvT to estimate Topts at high light, MASS BASED

#- fit all the curves
AvTfits.list.m <- lapply(tofit.l,FUN=fitJuneT,namex="Tleaf",namey="Photo_m",lengthPredict=20,
                          start=list(Rref=500,Topt=30,theta=20))


#- pull out the parameter means and SE's for plotting
AvTfits.m <- data.frame(do.call(rbind,
                              list(AvTfits.list.m[[1]][[2]],AvTfits.list.m[[2]][[2]],AvTfits.list.m[[3]][[2]])))
AvTfits.m$location <- c(rep("Cold-edge",nrow(AvTfits.m)/3),rep("Central",nrow(AvTfits.m)/3),rep("Warm-edge",nrow(AvTfits.m)/3))
AvTfits.m$location <- factor(AvTfits.m$location,levels=c("Cold-edge","Central","Warm-edge"))  

#-------------------------------------------------------


# windows(30,60);par(mfrow=c(3,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
# #- plot Asat at Topt
# barplot2(height=AvTfits$Aref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,30),las=1,
#          ylab=expression(A[ref]~(mu*mol~m^-2~s^-1)),
#          ci.l=AvTfits$Aref-AvTfits$Aref.se,ci.u=AvTfits$Aref+AvTfits$Aref.se)
# title(main="Short-term data")
# 
# #- plot Topt
# barplot2(height=AvTfits$Topt,names.arg=c("A","B","C"),plot.ci=T,las=1,ylim=c(0,30),
#          ylab=expression(T[opt]~(degree*C)),
#          ci.l=AvTfits$Topt-AvTfits$Topt.se,ci.u=AvTfits$Topt+AvTfits$Topt.se)
# #- plot Theta
# barplot2(height=AvTfits$theta,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,25),las=1,
#          ylab=expression(Omega~(degree*C)),
#          ci.l=AvTfits$theta-AvTfits$theta.se,ci.u=AvTfits$theta+AvTfits$theta.se)
# title(xlab="Provenance",outer=T,cex.lab=2,adj=0.6)
# dev.copy2pdf(file="output/AvTshorttermfits.pdf")




#-------------------------------------------------------
#-------------------------------------------------------
#- set up the plot, with Area and Mass based rates
pdf(file="output/FigureS3_Asat_vsT_area_mass.pdf",width=7.3,height=9)
par(mar=c(3,7,1,1),mfrow=c(2,1),cex.lab=2,cex.axis=1.2,oma=c(2,0,0,0))
palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]
xlims <- c(12,40)

#-------
#- plot Asat AREA based
plotBy(Sim.Mean~Tleaf|location,data=AvTfits,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=1.5,col=COL,
       ylab="",axes=F,
       xlab="")
as <- subset(AvTfits,location=="Cold-edge")
bs <- subset(AvTfits,location=="Central")
cs <- subset(AvTfits,location=="Warm-edge")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5, rev(as$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5, rev(bs$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5, rev(cs$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)
#legend("bottomright",levels(highQ$location),fill=COL,cex=1,title="Provenance")
legend("bottomright",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=1.2,title="Provenance",bty="n")
#legend("bottomleft","PPFD = 1500",bty="n")
legend("topright",letters[1],bty="n")
magaxis(side=1:4,labels=c(1,1,0,0),frame.plot=T,las=1)
title(ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)))

#- add TREATMENT MEANS
aq.m <- summaryBy(Photo+Photo_m+Tleaf+PARi~TleafFac+LightFac+W_treatment+prov+location,data=avt,FUN=c(mean,standard.error))
plotmeans <- subset(aq.m,LightFac==4 & W_treatment=="w")
plotBy(Photo.mean~Tleaf.mean|location,data=plotmeans,add=T,pch=16,cex=1.5,legend=F,col=COL,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Photo.mean,
                                 SE=plotmeans$Photo.standard.error,direction="updown")))
#-------


#-------
#- plot Asat MASS based
plotBy(Sim.Mean~Tleaf|location,data=AvTfits.m,legend=F,type="l",las=1,ylim=c(0,700),lwd=3,cex.lab=1.5,col=COL,
       ylab="",axes=F,
       xlab="")
as <- subset(AvTfits.m,location=="Cold-edge")
bs <- subset(AvTfits.m,location=="Central")
cs <- subset(AvTfits.m,location=="Warm-edge")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5, rev(as$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5, rev(bs$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5, rev(cs$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)
legend("topright",letters[2],bty="n")
magaxis(side=1:4,labels=c(1,1,0,0),frame.plot=T,las=1)
title(ylab=expression(A[sat]~(mmol~g^-1~s^-1)))

#- add TREATMENT MEANS
plotBy(Photo_m.mean~Tleaf.mean|location,data=plotmeans,add=T,pch=16,cex=1.5,legend=F,col=COL,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Photo_m.mean,
                                 SE=plotmeans$Photo_m.standard.error,direction="updown")))
title(xlab=expression(Measurement~T[leaf]~(degree*C)),outer=T,adj=0.7,line=0.5)
dev.off()
#dev.copy2pdf(file="output/FigureS4_Asat_vsT_area_mass.pdf")

#-------








#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Attempt to add a non-linear mixed effects model to directly test for differences
#   across provenances.

dat <- subset(avt, LightFac==4)

AvtFUN <- function(Tleaf,Jref,Topt,theta)Jref*exp(-1*((Tleaf-Topt)/theta)^2)

head(dat)
     
#--- Popt
#-fit with no fixed effect on Popt
fitnlme0 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=27,Topt=28,theta=20)),
                 data=dat)

#- refit with fixed effect on Popt
fitnlme1 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref~location,Topt +  theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=c(27,27,27),Topt=28,theta=20)),
                 data=dat)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)

#--- Topt
#-fit with no fixed effect on Topt
fitnlme0 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=27,Topt=28,theta=20)),
                 data=dat)

#- refit with fixed effect on Topt
fitnlme1 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref +  theta ~ 1,Topt~location),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=27,Topt=c(28,28,28),theta=20)),
                 data=dat)

# Likelihood ratio test
anova(fitnlme0, fitnlme1) #- not different


# K <- diag(5)
# rownames(K) <- names(coef(fitnlme1))
# out <- glht(fitnlme1, linfct = K)
# summary(out)


#--- theta
#-fit with no fixed effect on theta
fitnlme0 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=27,Topt=28,theta=20)),
                 data=dat)

#- refit with fixed effect on theta
fitnlme1 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt ~ 1,theta~location),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=27,Topt=28,theta=c(20,20,20))),
                 data=dat)

# Likelihood ratio test
anova(fitnlme0, fitnlme1) #- not different

#K <- diag(5)
#rownames(K) <- names(coef(fitnlme1))
#out <- glht(fitnlme1, linfct = K)
#ummary(out)