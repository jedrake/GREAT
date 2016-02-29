library(plotBy)
library(doBy)
library(magicaxis)
library(rgl)
source("R/generic_functions.R")
source("R/GREAT_functions.R")

#- get the AvT data
avt <- getAvT()


#-------------------------------------------------------
#- Plot RAW data

#- plot each light level's temperature response
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
avt.l <- split(avt,avt$LightFac)
ylims <- list(c(-2,8),c(2,35))
for(i in 1:length(avt.l)){
  toplot <- avt.l[[i]]
  plotBy(Photo~Tleaf|prov,data=subset(toplot,Water_trt=="wet"),las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  abline(h=0,lty=2)
  
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")
#-------------------------------------------------------






#-------------------------------------------------------
#- plot TREATMENT MEANS

avt.m <- summaryBy(Photo+Cond+Tleaf+PARi~TleafFac+LightFac+prov,data=avt,FUN=c(mean,standard.error))

#- plot each light level's temperature response
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
avt.m.l <- split(avt.m,avt.m$LightFac)
ylims <- list(c(0,8),c(10,35))
for(i in 1:length(avt.l)){
  toplot <- avt.m.l[[i]]
  plotBy(Photo.mean~Tleaf.mean|prov,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Photo.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  plotBy(Photo.mean~Tleaf.mean|prov,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F,add=T)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")
#-------------------------------------------------------









#-------------------------------------------------------
#-------------------------------------------------------
#- fit AvT to estimate Topts again, for the DIRECT 
#   short term fits only!
tofit <- subset(avt, LightFac==4)
tofit.l <- split(tofit,tofit$prov)

#- fit all the curves
AvTfits.list.st <- lapply(tofit.l,FUN=fitAvT)


#- pull out the parameter means and SE's for plotting
AvTfits <- data.frame(do.call(rbind,
                              list(AvTfits.list.st[[1]][[1]],AvTfits.list.st[[2]][[1]],AvTfits.list.st[[3]][[1]])))

windows(30,60);par(mfrow=c(3,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
#- plot Asat at Topt
barplot2(height=AvTfits$Aref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,30),las=1,
         ylab=expression(A[ref]~(mu*mol~m^-2~s^-1)),
         ci.l=AvTfits$Aref-AvTfits$Aref.se,ci.u=AvTfits$Aref+AvTfits$Aref.se)
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
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/AvTshorttermfits.pdf")


#- pull out the predictions and confidence intervals for plotting
toplot <- data.frame(do.call(rbind,
                             list(AvTfits.list.st[[1]][[2]],AvTfits.list.st[[2]][[2]],AvTfits.list.st[[3]][[2]])))
toplot$prov <- c(rep("A",51),rep("B",51),rep("C",51))

windows(30,30);par(mar=c(5,7,1,1))
COL=palette()[1:3]

plotBy(Sim.Mean~Tleaf|prov,data=toplot,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=2,
       ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),
       xlab=expression(T[leaf]~(degree*C)))
as <- subset(toplot,prov=="A")
bs <- subset(toplot,prov=="B")
cs <- subset(toplot,prov=="C")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5, rev(as$Sim.2.5)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5, rev(bs$Sim.2.5)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5, rev(cs$Sim.2.5)), col = alpha(COL[3],0.5), border = NA)
legend("bottomleft",c("A","B","C"),fill=COL,cex=2,title="Provenance")
title(main="Short-term data, predictions and 95% CI")

#- add TREATMENT MEANS
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+Water_trt+prov,data=avt,FUN=c(mean,standard.error))
plotmeans <- subset(aq.m,LightFac==4 & Water_trt=="wet")
plotBy(Photo.mean~Tleaf.mean|prov,data=plotmeans,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=plotmeans$Tleaf.mean,y=plotmeans$Photo.mean,
                                 SE=plotmeans$Photo.standard.error,direction="updown")))
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/AvTshortterm_predictions.pdf")

#-------------------------------------------------------
#-------------------------------------------------------








#-------------------------------------------------------
#- merge the short and long-term temperature response curves and plot them
#- get the AQ data
aq <- getAQ()
aq.m <- summaryBy(Photo+Cond+Tleaf+PARi~TleafFac+LightFac+prov,data=aq,FUN=c(mean,standard.error))
names(aq.m)[4:11] <- paste(names(aq.m)[4:11],"LT",sep=".")

at.all <- merge(avt.m,aq.m,by=c("prov","TleafFac","LightFac"))


#- make a plot comparing the long-term and short-term temperature responses
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
at.all.l <- split(at.all,at.all$LightFac)
ylims <- list(c(0,8),c(10,35))
for(i in 1:length(at.all.l)){
  toplot <- at.all.l[[i]]
  
  #- plot short-term
  plotBy(Photo.mean~Tleaf.mean|prov,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Photo.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  plotBy(Photo.mean~Tleaf.mean|prov,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F,add=T)
  
  #- add long-term
  plotBy(Photo.mean.LT~Tleaf.mean.LT|prov,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=1,
         axes=F,add=T)
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Photo.mean.LT,SE=toplot$Photo.standard.error.LT,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Photo.mean.LT,SE=toplot$Tleaf.standard.error.LT,direction="leftright")
  plotBy(Photo.mean.LT~Tleaf.mean.LT|prov,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=1,
         axes=F,add=T)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")
legend("bottomright",c("short-term","long-term"),col="black",pch=c(16,1),ncol=2,bg="white")

#-------------------------------------------------------












#-------------------------------------------------------
#- merge the short and long-term temperature response curves and plot them
#- of OVERALL MEANS (average across provenances.)

avt <- getAvT()
avt.m2 <- summaryBy(Photo+Cond+Tleaf+PARi+Ci~TleafFac+LightFac,data=avt,FUN=c(mean,standard.error))


#- get the AQ data
aq <- getAQ()
aq.m2 <- summaryBy(Photo+Cond+Tleaf+PARi+Ci~TleafFac+LightFac,data=aq,FUN=c(mean,standard.error))
names(aq.m2)[3:ncol(aq.m2)] <- paste(names(aq.m2)[3:ncol(aq.m2)],"LT",sep=".")

at.all2 <- merge(avt.m2,aq.m2,by=c("TleafFac","LightFac"))


#- plot PHOTO
#- make a plot comparing the long-term and short-term temperature responses
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2),cex=1.5)
at.all2.l <- split(at.all2,at.all2$LightFac)
ylims <- list(c(0,8),c(10,35))
for(i in 1:length(at.all2.l)){
  toplot <- at.all2.l[[i]]
  
  #- plot short-term
  plot(Photo.mean~Tleaf.mean,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         xlab="",ylab="",
         axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Photo.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Photo.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  
  #- add long-term
  points(Photo.mean.LT~Tleaf.mean.LT,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F)
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Photo.mean.LT,SE=toplot$Photo.standard.error.LT,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Photo.mean.LT,SE=toplot$Tleaf.standard.error.LT,direction="leftright")
  points(Photo.mean.LT~Tleaf.mean.LT,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,col="blue",
         axes=F)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(Photo~(mu*mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomright",c("short-term","long-term"),col=c("black","blue"),pch=c(16,16),ncol=2,bty="n",cex=0.7)




#plot CONDUCTANCE
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2),cex=1.5)
at.all2.l <- split(at.all2,at.all2$LightFac)
ylims <- list(c(0,3),c(0,3))
for(i in 1:length(at.all2.l)){
  toplot <- at.all2.l[[i]]
  
  #- plot short-term
  plot(Cond.mean~Tleaf.mean,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
       xlab="",ylab="",
       axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Cond.mean,SE=toplot$Cond.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Cond.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  
  #- add long-term
  points(Cond.mean.LT~Tleaf.mean.LT,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F)
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Cond.mean.LT,SE=toplot$Cond.standard.error.LT,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Cond.mean.LT,SE=toplot$Tleaf.standard.error.LT,direction="leftright")
  points(Cond.mean.LT~Tleaf.mean.LT,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,col="blue",
         axes=F)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(g[s]~(mol~m^-2~s^-1)),outer=T,cex.lab=2)
legend("bottomright",c("short-term","long-term"),col=c("black","blue"),pch=c(16,16),ncol=2,bty="n",cex=0.7)





#plot Ci
windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2),cex=1.5)
at.all2.l <- split(at.all2,at.all2$LightFac)
ylims <- list(c(250,500),c(250,500))
for(i in 1:length(at.all2.l)){
  toplot <- at.all2.l[[i]]
  
  #- plot short-term
  plot(Ci.mean~Tleaf.mean,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
       xlab="",ylab="",
       axes=F)
  legend("topright",paste("PAR = ",round(mean(toplot$PARi.mean),0)),bty="n")
  magaxis(side=1:4,labels=c(0,1,0,1),las=1)
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Ci.mean,SE=toplot$Ci.standard.error,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean,y=toplot$Ci.mean,SE=toplot$Tleaf.standard.error,direction="leftright")
  
  #- add long-term
  points(Ci.mean.LT~Tleaf.mean.LT,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,
         axes=F)
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Ci.mean.LT,SE=toplot$Ci.standard.error.LT,direction="updown")
  adderrorbars(x=toplot$Tleaf.mean.LT,y=toplot$Ci.mean.LT,SE=toplot$Tleaf.standard.error.LT,direction="leftright")
  points(Ci.mean.LT~Tleaf.mean.LT,data=toplot,las=1,xlim=c(15,45),ylim=ylims[[i]],legend=F,pch=16,col="blue",
         axes=F)
}
magaxis(side=1,labels=c(1),las=1)
title(xlab=expression(T[leaf]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(C[i]~(mu*mol~mol^-1)),outer=T,cex.lab=2)
legend("bottomright",c("short-term","long-term"),col=c("black","blue"),pch=c(16,16),ncol=2,bty="n",cex=0.7)

#-------------------------------------------------------

