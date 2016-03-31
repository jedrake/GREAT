#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Growth analysis script. returnRGR() does most of the heavy data-manipulation work
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")


#- get the data, process it for RGR.
dat.list <- returnRGR(plotson=T)
dat <- dat.list[[2]]     # RGR and AGR merged with canopy leaf area and SLA for the intensive growth interval only
dat.all <- dat.list[[1]] #RGR and AGR caculated for all available data.


#-----------------------------------------------------------------------------------------
#- plot mean AGR and RGR over time in 3-d manner

dat.m <- summaryBy(AGR+RGR+totmass~Date+Water_trt+room+prov,data=dat.all,FUN=mean,keep.names=T,na.rm=T)

library(rgl)
palette(rev(brewer.pal(6,"Spectral")))
plot3d(x=subset(dat.m,Water_trt=="wet")$room,y=subset(dat.m,Water_trt=="wet")$Date,
       z=subset(dat.m,Water_trt=="wet")$AGR,
       col="black",#col=palette()[as.factor(subset(dat.m,Water_trt=="wet")$prov)],
       xlab="Room",ylab="Date",zlab="AGR",size=15)

plot3d(x=subset(dat.m,Water_trt=="wet")$room,y=subset(dat.m,Water_trt=="wet")$Date,
       z=subset(dat.m,Water_trt=="wet")$RGR,type="p",
       col="black",#col=palette()[as.factor(subset(dat.m,Water_trt=="wet")$prov)],
       xlab="Room",ylab="Date",zlab="RGR",size=15)

plot3d(x=subset(dat.m,Water_trt=="wet")$room,y=subset(dat.m,Water_trt=="wet")$Date,
       z=subset(dat.m,Water_trt=="wet")$totmass,type="p",
       col=palette()[as.factor(subset(dat.m,Water_trt=="wet")$prov)],
       xlab="Room",ylab="Date",zlab="totmass",size=15)

#- plot again in 2d, with panels
dat.m <- summaryBy(AGR+RGR+totmass~Date+Water_trt+room+prov,data=dat.all,FUN=mean,keep.names=T,na.rm=T)
dat.m$logtotmass <- log10(dat.m$totmass)

dat.m.l <- split(dat.m,dat.m$Date)
palette(rev(brewer.pal(3,"Set2")))

#- plot AGR
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(7,7,1,2))
for(i in 2:length(dat.m.l)){
  toplot <- subset(dat.m.l[[i]],Water_trt=="wet")
  
  plotBy(AGR~as.numeric(room)|prov,data=toplot,pch=16,cex=2,ylim=c(0,max(toplot$AGR)+0.01),legend=F)
  if(i==2) legend("topleft",legend=LETTERS[1:3],pch=16,cex=1.5,col=palette()[1:3])
}
title(xlab=expression(Room),outer=T,cex.lab=2)
title(ylab=expression(AGR~(g~d^-1)),outer=T,cex.lab=2,adj=0.5)
#dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/NAR_LMF_SLA.pdf")

#- plot RGR
windows(30,60);par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(7,7,1,2))
for(i in 2:length(dat.m.l)){
  toplot <- subset(dat.m.l[[i]],Water_trt=="wet")
  
  plotBy(RGR~as.numeric(room)|prov,data=toplot,pch=16,cex=2,ylim=c(0,max(toplot$RGR)+0.01),legend=F)
  if(i==2) legend("topleft",legend=LETTERS[1:3],pch=16,cex=1.5,col=palette()[1:3])
}
title(xlab=expression(Room),outer=T,cex.lab=2)
title(ylab=expression(RGR~(g~g^-1~d^-1)),outer=T,cex.lab=2,adj=0.5)






#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- plot totalmass over time, as Belinda requested
dat.m.l2 <- split(dat.m,dat.m$room)


#- plot linear-scale
windows(40,70);par(mfrow=c(6,1),mar=c(0,0,0,0),oma=c(7,7,1,2),las=1)
for(i in 1:length(dat.m.l2)){
  toplot <- subset(dat.m.l2[[i]],Water_trt=="wet")
  
  plotBy(totmass~Date|prov,data=toplot,pch=16,cex=2,ylim=c(0,15),legend=F)
  if(i==1) legend("topright",legend=LETTERS[1:3],pch=16,cex=1.5,col=palette()[1:3],ncol=3)
  legend("topleft",legend=paste("Room",i,sep=" "),bty="n",cex=2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2016-01-01"),to=max(dat.m$Date),by="week"),labels=F,tck=0.05,las=2,cex.axis=1.5)
  
}
title(xlab=Date),outer=T,cex.lab=2)
title(ylab=expression(Total~mass~(g)),outer=T,cex.lab=2,adj=0.5)
axis.Date(side=1,at=seq.Date(from=as.Date("2016-01-01"),to=max(dat.m$Date),by="week"),labels=T,tck=0.05,las=2,cex.axis=1.5)

#- plot log-scale
windows(40,70);par(mfrow=c(6,1),mar=c(0,0,0,0),oma=c(7,7,1,2),las=1)
for(i in 1:length(dat.m.l2)){
  toplot <- subset(dat.m.l2[[i]],Water_trt=="wet")
  
  plotBy(logtotmass~Date|prov,data=toplot,pch=16,cex=2,ylim=c(-1,1.5),legend=F)
  if(i==1) legend("bottomright",legend=LETTERS[1:3],pch=16,cex=1.5,col=palette()[1:3],ncol=3)
  legend("topleft",legend=paste("Room",i,sep=" "),bty="n",cex=2)
  axis.Date(side=1,at=seq.Date(from=as.Date("2016-01-01"),to=max(dat.m$Date),by="week"),labels=F,tck=0.05,las=2,cex.axis=1.5)
  
}
title(xlab=Date),outer=T,cex.lab=2)
title(ylab=expression(log[10](Total~mass~(g))),outer=T,cex.lab=2,adj=0.5)
axis.Date(side=1,at=seq.Date(from=as.Date("2016-01-01"),to=max(dat.m$Date),by="week"),labels=T,tck=0.05,las=2,cex.axis=1.5)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------
windows(20,30);par(mfrow=c(3,1))
plotBy(NAR~RGR|room,data=dat,log="xy",pch=16)
plotBy(SLA~RGR|room,data=dat,log="xy",legend=F,pch=16)
plotBy(LMF~RGR|room,data=dat,log="xy",legend=F,pch=16)


library(rgl)
palette(rev(brewer.pal(6,"Spectral")))
plot3d(x=subset(dat,Water_trt=="wet")$canopy,y=subset(dat,Water_trt=="wet")$NAR,
       z=subset(dat,Water_trt=="wet")$RGR,
       col=palette()[as.numeric(subset(dat,Water_trt=="wet")$room)],
       xlab="Canopy (cm2)",ylab="NAR",zlab="AGR",size=10)






#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data, and plot temperature response curves
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+canopy~room,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)




windows();par(mar=c(6,7,1,1))
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF+canopy+logmass+totmass~room,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)

#- AGR vs. canopy leaf area
plotBy(AGR~canopy|room,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,
       xlab="Total crown leaf area (cm2)",ylab="Absolute growth rate (AGR; g day-1)",cex.lab=2)
adderrorbars(x=dat2$canopy.mean,y=dat2$AGR.mean,SE=dat2$AGR.standard.error,direction="updown")
adderrorbars(x=dat2$canopy.mean,y=dat2$AGR.mean,SE=dat2$canopy.standard.error,direction="leftright")
plotBy(AGR.mean~canopy.mean|room,data=dat2,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3)
arrows(x0=dat2$canopy.mean[1:5],x1=dat2$canopy.mean[2:6],y0=dat2$AGR.mean[1:5],y1=dat2$AGR.mean[2:6])

#- AGR vs. mass
plotBy(AGR~totmass|room,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,
       xlab="Total mass (g)",ylab="Absolute growth rate (AGR; g day-1)",cex.lab=2)
adderrorbars(x=dat2$totmass.mean,y=dat2$AGR.mean,SE=dat2$AGR.standard.error,direction="updown")
adderrorbars(x=dat2$totmass.mean,y=dat2$AGR.mean,SE=dat2$totmass.standard.error,direction="leftright")
plotBy(AGR.mean~totmass.mean|room,data=dat2,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3)
arrows(x0=dat2$totmass.mean[1:5],x1=dat2$totmass.mean[2:6],y0=dat2$AGR.mean[1:5],y1=dat2$AGR.mean[2:6])


#- RGR vs. mass
plotBy(RGR~totmass|room,data=subset(dat,Water_trt=="wet"),col=rev(brewer.pal(6,"Spectral")),pch=16,
       xlab="Total mass (g)",ylab="Relative growth rate (RGR; g day-1)",cex.lab=2)
adderrorbars(x=dat2$totmass.mean,y=dat2$RGR.mean,SE=dat2$RGR.standard.error,direction="updown")
adderrorbars(x=dat2$totmass.mean,y=dat2$RGR.mean,SE=dat2$totmass.standard.error,direction="leftright")
plotBy(RGR.mean~totmass.mean|room,data=dat2,col=rev(brewer.pal(6,"Spectral")),pch=15,add=T,cex=3)
arrows(x0=dat2$totmass.mean[1:5],x1=dat2$totmass.mean[2:6],y0=dat2$RGR.mean[1:5],y1=dat2$RGR.mean[2:6])




#- merge in room temperature key
key <- data.frame(room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
dat2 <- merge(dat2,key)





#-----------------------------------------------------------------------------------------
#- so it seems that an increase in SLA with temperature contributes to increased growth
#  at teh low end of temperature. How robust is that conclusion? Let's look at the four times
#  we estimated SLA (punches twice, gas exchange leaves, and harvest).
sla <- getSLA()

#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- plot temperature response curves for AGR and RGR
# 
# windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2))
# 
# #- agr
# plotBy(AGR.mean~Tair|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.8),legend=F,pch=16,
#        axes=F,xlab="",ylab="")
# magaxis(side=1:4,labels=c(0,1,0,1),las=1)
# adderrorbars(x=dat2$Tair,y=dat2$AGR.mean,SE=dat2$AGR.standard.error,direction="updown")
# plotBy(AGR.mean~Tair|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.8),legend=F,pch=16,
#        axes=F,xlab="",ylab="",add=T)
# magaxis(side=1,labels=c(1),las=1)
# legend("topleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white")
# 
# 
# #- RGR
# plotBy(RGR.mean~Tair|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.2),legend=F,pch=16,
#        axes=F,xlab="",ylab="")
# magaxis(side=1:4,labels=c(0,1,0,1),las=1)
# adderrorbars(x=dat2$Tair,y=dat2$RGR.mean,SE=dat2$RGR.standard.error,direction="updown")
# plotBy(RGR.mean~Tair|prov,data=dat2,legend=F,pch=16,
#        axes=F,xlab="",ylab="",add=T)
# magaxis(side=1,labels=c(1),las=1)
# 
# 
# 
# title(xlab=expression(T[air]~(degree*C)),outer=T,cex.lab=2)
# title(ylab=expression(AGR~(g~d^-1)),outer=T,cex.lab=2,adj=0.9)
# title(ylab=expression(RGR~(g~g^-1~d^-1)),outer=T,cex.lab=2,adj=0.15)



#-----------------------------------------------------------------------------------------
#- average across provenances, ignore the dry data, and plot temperature response curves
dat2 <- summaryBy(RGR+AGR+SLA+LAR+NAR+LMF~room+prov,FUN=c(mean,standard.error),data=subset(dat,Water_trt=="wet"),na.rm=T)

#- merge in room temperature key
key <- data.frame(room=1:6,Tair= c(18,21.5,25,28.5,32,35.5)) # could be improved with real data
dat2 <- merge(dat2,key)




#-----------------------------------------------------------------------------------------
#- plot temperature response curves for NAR, LMF, and SLA
palette(rev(brewer.pal(3,"Set2")))

windows(30,60);par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(5,7,1,2))

#- NAR
plotBy(NAR.mean~Tair|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,10),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$NAR.mean,SE=dat2$NAR.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,1),las=1)
magaxis(side=1,labels=c(1),las=1)
legend("bottomleft",c("A","B","C"),col=palette()[1:3],pch=16,ncol=3,bg="white",cex=2)


#- LMF
plotBy(LMF.mean~Tair|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(0,0.5),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$LMF.mean,SE=dat2$LMF.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,1),las=1)
magaxis(side=1,labels=c(1),las=1)


#- SLA
plotBy(SLA.mean~Tair|prov,data=dat2,las=1,xlim=c(17,37),ylim=c(250,500),legend=F,pch=16,cex=2,
       axes=F,xlab="",ylab="",
       panel.first=adderrorbars(x=dat2$Tair,y=dat2$SLA.mean,SE=dat2$SLA.standard.error,direction="updown"))
magaxis(side=1:4,labels=c(0,1,0,1),las=1)
magaxis(side=1,labels=c(1),las=1)


title(xlab=expression(T[air]~(degree*C)),outer=T,cex.lab=2)
title(ylab=expression(NAR~(g~m^-2~d^-1)),outer=T,cex.lab=2,adj=0.95)
title(ylab=expression(LMF~(g~g^-1)),outer=T,cex.lab=2,adj=0.5)
title(ylab=expression(SLA~(cm^2~g^-1)),outer=T,cex.lab=2,adj=0.1)
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/NAR_LMF_SLA.pdf")
#-----------------------------------------------------------------------------------------







#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- fit AGR v T to estimate Topts
tofit <- dat2
tofit.l <- split(tofit,tofit$prov)

#- fit all the curves
AGRvTfits.l <- lapply(tofit.l,FUN=fitAGRvT)


#- pull out the parameter means and SE's for plotting
AGRvTfits <- data.frame(do.call(rbind,
                              list(AGRvTfits.l[[1]][[1]],AGRvTfits.l[[2]][[1]],AGRvTfits.l[[3]][[1]])))

windows(30,60);par(mfrow=c(3,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
#- plot Asat at Topt
barplot2(height=AGRvTfits$AGRref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,0.5),las=1,
         ylab=expression(AGR~(g~d^-1)),
         ci.l=AGRvTfits$AGRref-AGRvTfits$AGRref.se,ci.u=AGRvTfits$AGRref+AGRvTfits$AGRref.se)

#- plot Topt
barplot2(height=AGRvTfits$Topt,names.arg=c("A","B","C"),plot.ci=T,las=1,ylim=c(0,30),
         ylab=expression(T[opt]~(degree*C)),
         ci.l=AGRvTfits$Topt-AGRvTfits$Topt.se,ci.u=AGRvTfits$Topt+AGRvTfits$Topt.se)
#- plot Theta
barplot2(height=AGRvTfits$theta,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,10),las=1,
         ylab=expression(Omega~(degree*C)),
         ci.l=AGRvTfits$theta-AGRvTfits$theta.se,ci.u=AGRvTfits$theta+AGRvTfits$theta.se)
title(xlab="Provenance",outer=T,cex.lab=2,adj=0.6)
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/AGRvTfits.pdf")


#- pull out the predictions and confidence intervals for plotting
toplot <- data.frame(do.call(rbind,
                             list(AGRvTfits.l[[1]][[2]],AGRvTfits.l[[2]][[2]],AGRvTfits.l[[3]][[2]])))
toplot$prov <- c(rep("A",51),rep("B",51),rep("C",51))

windows(30,30);par(mar=c(5,7,1,1))
COL=palette()[1:3]

plotBy(Sim.Mean~Tleaf|prov,data=toplot,legend=F,type="l",las=1,ylim=c(0,0.5),lwd=3,cex.lab=2,
       ylab=expression(AGR~(g~d^-1)),
       xlab=expression(T[air]~(degree*C)))
as <- subset(toplot,prov=="A")
bs <- subset(toplot,prov=="B")
cs <- subset(toplot,prov=="C")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5., rev(as$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5., rev(bs$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5., rev(cs$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
legend("topleft",c("A","B","C"),fill=COL,cex=2,title="Provenance")

#- add TREATMENT MEANS
plotBy(AGR.mean~Tair|prov,data=dat2,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=dat2$Tair,y=dat2$AGR.mean,
                                 SE=dat2$AGR.standard.error,direction="updown")))
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/AGRvT_predictions.pdf")
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------










#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- repeat the analysis for AGR (above), but for RGR

windows(30,40);par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(5,7,1,2))

#- fit AGR v T to estimate Topts
tofit <- dat2
tofit.l <- split(tofit,tofit$prov)

#- fit all the curves
RGRvTfits.l <- lapply(tofit.l,FUN=fitRGRvT)


#- pull out the parameter means and SE's for plotting
RGRvTfits <- data.frame(do.call(rbind,
                                list(RGRvTfits.l[[1]][[1]],RGRvTfits.l[[2]][[1]],RGRvTfits.l[[3]][[1]])))

windows(30,60);par(mfrow=c(3,1),mar=c(2,6,1,0),oma=c(5,1,1,2),cex.lab=2)
#- plot Asat at Topt
barplot2(height=RGRvTfits$RGRref,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,0.15),las=1,
         ylab=expression(RGR~(g~g^-1~d^-1)),
         ci.l=RGRvTfits$RGRref-RGRvTfits$RGRref.se,ci.u=RGRvTfits$RGRref+RGRvTfits$RGRref.se)

#- plot Topt
barplot2(height=RGRvTfits$Topt,names.arg=c("A","B","C"),plot.ci=T,las=1,ylim=c(0,30),
         ylab=expression(T[opt]~(degree*C)),
         ci.l=RGRvTfits$Topt-RGRvTfits$Topt.se,ci.u=RGRvTfits$Topt+RGRvTfits$Topt.se)
#- plot Theta
barplot2(height=RGRvTfits$theta,names.arg=c("A","B","C"),plot.ci=T,ylim=c(0,20),las=1,
         ylab=expression(Omega~(degree*C)),
         ci.l=RGRvTfits$theta-RGRvTfits$theta.se,ci.u=RGRvTfits$theta+RGRvTfits$theta.se)
title(xlab="Provenance",outer=T,cex.lab=2,adj=0.6)
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/RGRvTfits.pdf")


#- pull out the predictions and confidence intervals for plotting
toplot <- data.frame(do.call(rbind,
                             list(RGRvTfits.l[[1]][[2]],RGRvTfits.l[[2]][[2]],RGRvTfits.l[[3]][[2]])))
toplot$prov <- c(rep("A",51),rep("B",51),rep("C",51))

windows(30,30);par(mar=c(5,7,1,1))
COL=palette()[1:3]

plotBy(Sim.Mean~Tleaf|prov,data=toplot,legend=F,type="l",las=1,ylim=c(0,0.15),lwd=3,cex.lab=2,
       ylab=expression(RGR~(g~g^-1~d^-1)),
       xlab=expression(T[air]~(degree*C)))
as <- subset(toplot,prov=="A")
bs <- subset(toplot,prov=="B")
cs <- subset(toplot,prov=="C")

polygon(x = c(as$Tleaf, rev(as$Tleaf)), y = c(as$Sim.97.5., rev(as$Sim.2.5.)), col = alpha(COL[1],0.5), border = NA)
polygon(x = c(bs$Tleaf, rev(bs$Tleaf)), y = c(bs$Sim.97.5., rev(bs$Sim.2.5.)), col = alpha(COL[2],0.5), border = NA)
polygon(x = c(cs$Tleaf, rev(cs$Tleaf)), y = c(cs$Sim.97.5., rev(cs$Sim.2.5.)), col = alpha(COL[3],0.5), border = NA)
legend("bottomleft",c("A","B","C"),fill=COL,cex=2,title="Provenance")

#- add TREATMENT MEANS
plotBy(RGR.mean~Tair|prov,data=dat2,add=T,pch=16,cex=2,legend=F,
       panel.first=(adderrorbars(x=dat2$Tair,y=dat2$RGR.mean,
                                 SE=dat2$RGR.standard.error,direction="updown")))
dev.copy2pdf(file="W://WORKING_DATA/GHS39/GREAT/Share/Output/RGRvT_predictions.pdf")

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------






#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Biomass fractions
dat <- getHarvest()
size <- getSize()

#- pull out just the unique pot and room numbers from teh size dataframe
size2 <- unique(size[,c("pot","room","Water_trt")])

#- merge pot ids and harvest. Note the pre-treatment plants get excluded here
dat2 <- merge(size2,dat,by.x=c("pot"),by.y="Pot")

#- calculate leaf, stem, and root mass fractions
dat2$LMF <- with(dat2,leafdm/totdm)
dat2$SMF <- with(dat2,stemdm/totdm)
dat2$RMF <- with(dat2,rootdm/totdm)

#- plot
windows(40,60);par(mfrow=c(3,1),mar=c(5,7,1,1),cex.lab=2,cex.axis=1.3,las=1)
plotBy(LMF~logtotdm|room,data=subset(dat2,Water_trt=="wet"),pch=15,cex=1.5,ylim=c(0,1),col=rev(brewer.pal(6,"RdYlGn")),
       legend=F,xlab="log(Total mass, g)",ylab="Leaf mass fraction")
legend("bottom",legend=1:6,col=rev(brewer.pal(6,"RdYlGn")),pch=15,cex=1.5,ncol=6)
plotBy(SMF~logtotdm|room,data=subset(dat2,Water_trt=="wet"),pch=15,cex=1.5,ylim=c(0,1),col=rev(brewer.pal(6,"RdYlGn")),
       legend=F,xlab="log(Total mass, g)",ylab="Stem mass fraction")
plotBy(RMF~logtotdm|room,data=subset(dat2,Water_trt=="wet"),pch=15,cex=1.5,ylim=c(0,1),col=rev(brewer.pal(6,"RdYlGn")),
       legend=F,xlab="log(Total mass, g)",ylab="Root mass fraction")

#- plot total final biomass
dat2.m <- summaryBy(leafdm+stemdm+rootdm+totdm~room+prov,data=subset(dat2,Water_trt=="wet"),FUN=c(mean,standard.error),keep.names=F)
windows();par(cex.lab=1.5)
plotBy(totdm.mean~as.numeric(room)|prov,data=dat2.m,pch=16,cex=2,ylim=c(0,12),xlab="Room",ylab="Total dry mass (g)",
       panel.first=adderrorbars(x=as.numeric(dat2.m$room),y=dat2.m$totdm.mean,SE=dat2.m$totdm.standard.error,direction="updown"))



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------