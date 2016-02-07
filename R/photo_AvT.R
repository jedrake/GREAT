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

avt.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+prov,data=avt,FUN=c(mean,standard.error))

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
#- merge the short and long-term temperature response curves and plot them
#- get the AQ data
aq <- getAQ()
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+prov,data=aq,FUN=c(mean,standard.error))
names(aq.m)[4:9] <- paste(names(aq.m)[4:9],"LT",sep=".")

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



