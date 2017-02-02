#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- This plots temperature response curves forlight-saturated photosynthetic rates
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------




#--- Asat
#- get the AQ data (i.e., "long-term" photosynthesis dataset)
aq <- getAQ()

Asatdata <- subset(aq,campaign==1 & LightFac==4 & W_treatment=="w")
Asatdata.l <- split(Asatdata,Asatdata$location)

#- fit all the curves
AvTfits.list <- lapply(Asatdata.l,FUN=fitAvT)



#---- Final mass
#- pull out the predictions and confidence intervals for plotting
A.pred <- data.frame(do.call(rbind,
                                list(AvTfits.list[[1]][[2]],AvTfits.list[[2]][[2]],AvTfits.list[[3]][[2]])))
A.pred$location <- c(rep("Cold-edge",nrow(A.pred)/3),rep("Central",nrow(A.pred)/3),rep("Warm-edge",nrow(A.pred)/3))
A.pred$location <- factor(A.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  


#windows();
pdf(file="output/Figure2-Photo_vs_Temperature.pdf",width=3.5,height=3)
par(mar=c(3.5,4,0.5,0.5),oma=c(0,0,0,0))

#- plot Asat
plotBy(Sim.Mean~Tleaf|location,data=A.pred,legend=F,type="l",las=1,xlim=c(18,42),ylim=c(0,30),lwd=3,cex.lab=2,,xaxt="n",yaxt="n",
       ylab="",col=COL,
       xlab="")
as.m <- subset(A.pred,location=="Cold-edge")
bs.m <- subset(A.pred,location=="Central")
cs.m <- subset(A.pred,location=="Warm-edge")

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
#legend("bottomleft",levels(A.pred$location),fill=COL,cex=1.2,title="Provenance",bty="n")
legend("bottomleft",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=0.9,title="Provenance",bty="n")

#- add TREATMENT MEANS for mass
dat3 <- summaryBy(Photo+Tair+Tleaf~Room+location,FUN=c(mean,standard.error),data=Asatdata,na.rm=T)

adderrorbars(x=dat3$Tleaf.mean,y=dat3$Photo.mean,SE=dat3$Photo.standard.error,direction="updown")
adderrorbars(x=dat3$Tleaf.mean,y=dat3$Photo.mean,SE=dat3$Tleaf.standard.error,direction="leftright")

#plotBy(Photo.mean~Tleaf.mean|location,data=dat3,add=T,pch=21,cex=2,legend=F,col="black")
palette(COL) 
points(Photo.mean~Tleaf.mean,data=dat3,add=T,pch=21,cex=1.2,legend=F,col="black",bg=location)


#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.1,ratio=0.4,tcl=0.2)
title(xlab=expression(Measurement~T[leaf]~(degree*C)),cex.lab=1.3,line=2)
title(ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),cex.lab=1.3,line=2)

dev.off()
#dev.copy2pdf(file="output/Figure2-PhotoVsT.pdf")

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------






#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Attempt to add a non-linear mixed effects model to directly test for differences
#   across provenances.

names(Asatdata)

#- remove one obseration with a missing value
Asatdata <- Asatdata[-which(is.na(Asatdata$Photo)),]

#- here is the function to fit
AvtFUN <- function(Tleaf,Jref,Topt,theta)Jref*exp(-1*((Tleaf-Topt)/theta)^2)


#--- Popt
#-fit with no fixed effect on Popt
fitnlme0 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=30,Topt=25,theta=20)),
                 data=Asatdata)

#- refit with fixed effect on Popt
fitnlme1 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref~location,Topt +  theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=c(25,25,25),Topt=25,theta=20)),
                 data=Asatdata)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)
confint(fitnlme1)[1]

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)



#--- Topt
#-fit with no fixed effect on Topt
fitnlme0 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | Prov,
                 start=list(fixed=c(Jref=30,Topt=25,theta=20)),
                 data=Asatdata)

#- refit with fixed effect on Topt
fitnlme1 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Topt~Prov,Jref +  theta ~ 1),
                 random = Jref ~ 1 | Prov,
                 start=list(fixed=c(Topt=c(25,25,25),Jref=30,theta=20)),
                 data=Asatdata)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)



#--- Theta
#-fit with no fixed effect on Popt
fitnlme0 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=30,Topt=25,theta=20)),
                 data=Asatdata)

#- refit with fixed effect on Popt
fitnlme1 <- nlme(Photo ~ AvtFUN(Tleaf, Jref, Topt,theta),
                 fixed=list(theta~location,Topt +  Jref ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=25,Topt=25,theta=c(20,20,20))),
                 data=Asatdata)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)



#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------