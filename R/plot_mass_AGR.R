#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- This plots temperature response curves for final mass and absolute growth rate
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- prepare the three datasets, fit temperature response curves in preparation for plotting

#--- Biomass
#- prepare biomass data, fit and plot the first panel
dat <- getHarvest()
size <- getSize()

#- pull out just the unique pot and room numbers from teh size dataframe
size2 <- unique(size[,c("Code","Room","W_treatment","location","Tair")])

#- merge pot ids and harvest. Note the pre-treatment plants get excluded here
dat2 <- merge(size2,dat,by.x=c("Code","location","W_treatment"),by.y=c("Code","location","W_treatment"))
massdata <- subset(dat2,W_treatment == "w")
massdata.l <- split(massdata,massdata$location)

#- fit final mass
MASSvTfits.l <- lapply(massdata.l,FUN=fitJuneT,start=list(Rref=5,Topt=30,theta=5),namey="totdm",namex="Tair",lengthPredict=20)




#-----------------------------------------------------------------------------------------
#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=F)
agr <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
#agr <- subset(agr,Date==as.Date("2016-02-08"))
agr.all <- dat.list[[1]] #RGR and AGR caculated for all available data.
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- fit AGR T to estimate Topts
tofit <- subset(agr,W_treatment=="w") # not needed, as this is done in returnRGR() now.
tofit.l <- split(tofit,tofit$location)

#- fit AGR and RGR T response curves
AGRvTfits.l <- lapply(tofit.l,FUN=fitJuneT,start=list(Rref=0.5,Topt=30,theta=5),namey="AGR",namex="Tair",lengthPredict=20)

agr2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF~Room+Tair+location,FUN=c(mean,standard.error),data=subset(agr,W_treatment=="w"),na.rm=T)

#-----------------------------------------------------------------------------------------
# 
# 
# 
# 
# #--- Asat
# #- get the AQ data (i.e., "long-term" photosynthesis dataset)
# aq <- getAQ()
# 
# Asatdata <- subset(aq,campaign==1 & LightFac==4 & W_treatment=="w")
# Asatdata.l <- split(Asatdata,Asatdata$location)
# 
# #- fit all the curves
# AvTfits.list <- lapply(Asatdata.l,FUN=fitAvT)
# 
# 
# 
# #----- Respiration
# #- get the RvT data
# rvt <- getRvT()
# 
# rvt.l <- split(rvt,rvt$location)
# 
# #- fit all the curves
# RvTfits.list.st <- lapply(rvt.l,FUN=fitRvT,namex="Tleaf",namey="Rmass")
# 
# #-----------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------
# 








#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- set up the plot
#windows(2*40,40);
pdf(file="output/Figure4-Absolute_growth.pdf",width=7.3,height=4)

par(mar=c(3.5,4,1,1),mfrow=c(1,2),cex.lab=1.5,cex.axis=1.2,oma=c(0.5,0.5,0,0))
palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]
palette(COL) 

xlims <- c(15,40)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#-------- Make plots

#---- Final mass
#- pull out the predictions and confidence intervals for plotting
mass.pred <- data.frame(do.call(rbind,
                             list(MASSvTfits.l[[1]][[2]],MASSvTfits.l[[2]][[2]],MASSvTfits.l[[3]][[2]])))
mass.pred$prov <- c(rep("A",nrow(mass.pred)/3),rep("B",nrow(mass.pred)/3),rep("C",nrow(mass.pred)/3))
mass.pred$location <- c(rep("Cold-edge",nrow(mass.pred)/3),rep("Warm-edge",nrow(mass.pred)/3),rep("Central",nrow(mass.pred)/3))
mass.pred$location <- factor(mass.pred$location,levels=c("Cold-edge","Central","Warm-edge"))  
  
#- plot mass
plotBy(Sim.Mean~Tleaf|location,data=mass.pred,legend=F,type="l",las=1,ylim=c(0,12),lwd=3,cex.lab=1.4,xlim=xlims,axes=F,
       ylab="",
       xlab="")
as.m <- subset(mass.pred,prov=="A")
bs.m <- subset(mass.pred,prov=="B")
cs.m <- subset(mass.pred,prov=="C")
title(ylab=expression(Final~mass~(g)),line=2,cex.lab=1.4)

polygon(x = c(as.m$Tleaf, rev(as.m$Tleaf)), y = c(as.m$Sim.97.5., rev(as.m$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.m$Tleaf, rev(bs.m$Tleaf)), y = c(bs.m$Sim.97.5., rev(bs.m$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.m$Tleaf, rev(cs.m$Tleaf)), y = c(cs.m$Sim.97.5., rev(cs.m$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)

#- add TREATMENT MEANS for mass
dat3 <- summaryBy(totdm+Tair~Room+location,FUN=c(mean,standard.error),data=massdata,na.rm=T)

plotBy(totdm.mean~Tair.mean|location,data=dat3,add=T,pch=16,cex=0.5,legend=F,col=COL,
       panel.first=(adderrorbars(x=dat3$Tair.mean,y=dat3$totdm.mean,
                                 SE=dat3$totdm.standard.error,direction="updown")))
points(totdm.mean~Tair.mean,data=dat3,add=T,pch=21,cex=1.2,legend=F,col="black",bg=location)


#- gussy up the graph
magaxis(side=c(1,2,4),labels=c(1,1,0),frame.plot=T,las=1,cex.axis=1.1,ratio=0.4,tcl=0.2)
title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=1.5,line=2.5)
legend("topright",letters[1],bty="n",cex=1.2)



#---
#--- plot AGR vs. T (panel b)
#- pull out the predictions and confidence intervals for plotting
AGRplot <- data.frame(do.call(rbind,
                              list(AGRvTfits.l[[1]][[2]],AGRvTfits.l[[2]][[2]],AGRvTfits.l[[3]][[2]])))
AGRplot$prov <- c(rep("A",nrow(AGRplot)/3),rep("B",nrow(AGRplot)/3),rep("C",nrow(AGRplot)/3))
AGRplot$location <- c(rep("Cold-edge",nrow(AGRplot)/3),rep("Warm-edge",nrow(AGRplot)/3),rep("Central",nrow(AGRplot)/3))
AGRplot$location <- factor(AGRplot$location,levels=c("Cold-edge","Central","Warm-edge"))  

plotBy(Sim.Mean~Tleaf|prov,data=AGRplot,legend=F,type="l",las=1,ylim=c(0,0.5),lwd=3,cex.lab=1.4,col=COL,xlim=xlims,
       ylab="",axes=F,
       xlab="")
as.agr <- subset(AGRplot,prov=="A")
bs.agr <- subset(AGRplot,prov=="B")
cs.agr <- subset(AGRplot,prov=="C")
title(ylab=expression(AGR~(g~d^-1)),line=2,cex.lab=1.4)

polygon(x = c(as.agr$Tleaf, rev(as.agr$Tleaf)), y = c(as.agr$Sim.97.5., rev(as.agr$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs.agr$Tleaf, rev(bs.agr$Tleaf)), y = c(bs.agr$Sim.97.5., rev(bs.agr$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs.agr$Tleaf, rev(cs.agr$Tleaf)), y = c(cs.agr$Sim.97.5., rev(cs.agr$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)


#- add TREATMENT MEANS
plotBy(AGR.mean~Tair|location,data=agr2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,
       axes=F,xlab="",ylab="",cex=0.5,col=COL,add=T,
       panel.first=adderrorbars(x=agr2$Tair,y=agr2$AGR.mean,SE=agr2$AGR.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(1,1,0),las=1,cex.axis=1.1,ratio=0.4,tcl=0.2,frame.plot=T)
points(AGR.mean~Tair,data=agr2,add=T,pch=21,cex=1.2,legend=F,col="black",bg=location)
legend("topleft",c("Cold-origin","Central","Warm-origin"),fill=COL,cex=0.8,title="Provenance",bty="n")


#legend("bottomright",levels(dat2$location),fill=COL,cex=1.2,title="Provenance",bty="n")
legend("topright",letters[2],bty="n",cex=1.2)
title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=1.5,line=2.5)


#- export the graph
dev.off()
#dev.copy2pdf(file="output/Figure4-FinalMass_AGR.pdf")








#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#- Attempt to add a non-linear mixed effects model to directly test for differences
#   across provenances.
head(massdata)
head(agr)

#- here is the function to fit
GvtFUN <- function(Tair,Jref,Topt,theta)Jref*exp(-1*((Tair-Topt)/theta)^2)


#---- final mass data
#--- Popt
#-fit with no fixed effect on Popt
fitnlme0 <- nlme(totdm ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=8,Topt=28,theta=9)),
                 data=massdata)

#- refit with fixed effect on Popt
fitnlme1 <- nlme(totdm ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref~location,Topt +  theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=c(8,8,8),Topt=28,theta=9)),
                 data=massdata)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)


#--- Topt
#-fit with no fixed effect on Topt
fitnlme0 <- nlme(totdm ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=8,Topt=28,theta=9)),
                 data=massdata)

#- refit with fixed effect on Topt
fitnlme1 <- nlme(totdm ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Topt~location,Jref +  theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=8,Topt=c(28,28,28),theta=9)),
                 data=massdata)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)


#--- theta
#-fit with no fixed effect on theta
fitnlme0 <- nlme(totdm ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=8,Topt=28,theta=9)),
                 data=massdata)

#- refit with fixed effect on theta
fitnlme1 <- nlme(totdm ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt ~ 1,theta~location),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=8,Topt=28,theta=c(9,9,9))),
                 data=massdata)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)






#---- absolute growth rate

#--- Popt
#-fit with no fixed effect on Popt
fitnlme0 <- nlme(AGR ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=0.4,Topt=25,theta=20)),
                 data=agr)

#- refit with fixed effect on Popt
fitnlme1 <- nlme(AGR ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref~location,Topt +  theta ~ 1),
                 random = Topt ~ 1 | location,
                 start=list(fixed=c(Jref=c(0.4,0.4,0.4),Topt=25,theta=20)),
                 data=agr)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)

#--- Topt
#-fit with no fixed effect on Topt
fitnlme0 <- nlme(AGR ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=0.4,Topt=25,theta=20)),
                 data=agr)

#- refit with fixed effect on Topt
fitnlme1 <- nlme(AGR ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Topt~location,Jref +  theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=0.4,Topt=c(25,25,25),theta=20)),
                 data=agr)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)


#--- theta
#-fit with no fixed effect on theta
fitnlme0 <- nlme(AGR ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt + theta ~ 1),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=0.4,Topt=28,theta=8)),
                 data=agr)

#- refit with fixed effect on theta
fitnlme1 <- nlme(AGR ~ GvtFUN(Tair, Jref, Topt,theta),
                 fixed=list(Jref + Topt ~ 1,theta~location),
                 random = Jref ~ 1 | location,
                 start=list(fixed=c(Jref=0.4,Topt=28,theta=c(8,8,8))),
                 data=agr)

# Likelihood ratio test
anova(fitnlme0, fitnlme1)

K <- diag(5)
rownames(K) <- names(coef(fitnlme1))
out <- glht(fitnlme1, linfct = K)
summary(out)
