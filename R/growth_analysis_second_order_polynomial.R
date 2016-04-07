
#-------------------------------------------------------------------------------------
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")
#-------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------
#- get the data, process it for log-polynomial fitting of total mass over time.
dat.list <- returnRGR(plotson=F)
d <- dat.list[[1]]
d$Time <- unname(as.numeric(d$Date-min(d$Date-7))) # calculate "Time" in days
names(d)[which(names(d)=="totmass")] <- "TotMass"  # rename it "TotMass"

#- remove pots with less than 5 good d2h measurements
d.l <- summaryBy(TotMass~pot,data=subset(d,is.na(TotMass)==F),FUN=length)
keeps <- droplevels(subset(d.l,TotMass.length>=5))
d2 <- droplevels(subset(d,pot %in% keeps$pot))
d2$Code <- d2$pot
#-------------------------------------------------------------------------------------





#-------------------------------------------------------------------------------------
#- fit log-polynomial growth model to each plant one at a time, extract parameters, and analyze them.
#  Creates a pdf of plots for each plant

pdf(file="output/Growth_poly_allplants.pdf", paper = "a4", width = 14/2.54, height = 14/2.54)
growth.l <- split(d2,d2$Code)
loglinfits <- output <- data.out <- list()
for(i in 1:length(growth.l)){
  tofit <- growth.l[[i]]
  loglinfits[[i]] <- lm(log(TotMass)~Time+I(Time^2),data=tofit) #- fit log-polynomial

  #- extract the output using a new function which calculates AGR and RGR from the polynomial fit parameters
  output[[i]] <- output.log_lin(X=tofit$Time,Y=tofit$TotMass,
                              params=unname(coef(loglinfits[[i]])),times=7:60,Code=tofit$Code[1])$rates
  data.out[[i]] <- output.log_lin(X=tofit$Time,Y=tofit$TotMass,
                                params=unname(coef(loglinfits[[i]])),times=7:60,Code=tofit$Code[1])$data
  data.out[[i]]$Code <- output[[i]]$Code <- tofit$Code[1]
  data.out[[i]]$Water_trt <-output[[i]]$Water_trt <- tofit$Water_trt[1]
  data.out[[i]]$room <- output[[i]]$room <-tofit$room[1]
  
  #- make plot (growing pdf)
  plot(observed~time,data=data.out[[i]],cex=1.5,ylab="Total mass (g)",xlab="Time (days)",ylim=c(0,20))
  lines(M~times,data=output[[i]])
  mtext(text=tofit$Code[1],side=3,line=-1)
  legend("topleft",paste("r2 = ",round(summary(loglinfits[[i]])$r.squared,digits=3)),bty="n")
  
}
dev.off() # close pdf
#- put rates dataframe together. This has the timecourse of mass, AGR, and RGR for each plant
rates.df <-do.call(rbind,output)
data.df <- do.call(rbind,data.out)
data.df$fit_type <- "log-polynomial"
params <- as.data.frame(do.call(rbind,lapply(loglinfits,coef))) #- get polynomial parameters. Perhaps not that useful alone
params$Code <- unique(rates.df$Code) # add the code variable to the parameter estimate
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- average across rooms
rates.m <- summaryBy(M+AGR+RGR~times+room,data=subset(rates.df,Water_trt=="wet"),FUN=c(mean,standard.error))
d.m <- summaryBy(TotMass~Time+room,data=subset(d2,Water_trt=="wet"),FUN=c(mean,standard.error))


#- plot the complex evolution of growth over time
windows(50,60);par(mfrow=c(3,1),mar=c(0,5,0,1),oma=c(5,2,2,2),las=1,cex=1.1,cex.lab=1.5)
palette(rev(brewer.pal(6,"Spectral")))

plotBy(M.mean~times|room,data=rates.m,type="o",ylab=expression(Mass~(g)),pch=16,cex=0,lwd=4,axes=F,
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$M.mean,SE=rates.m$M.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(0,1,1),frame.plot=T)
plotBy(TotMass.mean~Time|room,data=d.m,type="p",ylab=expression(Mass~(g)),pch=16,add=T,cex=1.5,
       panel.first=adderrorbars(x=d.m$Time,y=d.m$TotMass.mean,SE=d.m$TotMass.standard.error,direction="updown"))
plotBy(AGR.mean~times|room,data=rates.m,type="o",ylab=expression(AGR~(g~d^-1)),pch=16,legend=F,cex=0,lwd=4,axes=F,
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$AGR.mean,SE=rates.m$AGR.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(0,1,1),frame.plot=T)
plotBy(RGR.mean~times|room,data=rates.m,type="o",ylab=expression(RGR~(g~g^-1~d^-1)),pch=16,legend=F,cex=0,lwd=4,axes=F,
       ylim=c(0,0.2),
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$RGR.mean,SE=rates.m$RGR.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(1,1,1),frame.plot=T)
title(xlab="Time (days)",outer=T,adj=0.6)
dev.copy2pdf(file="output/Growth_analysis_2ndorderpoly.pdf")

#-------------------------------------------------------------------------------------








#-------------------------------------------------------------------------------------
#- Make 3-d plot of mass, time, and temperature.
library(rgl)
library(akima)
rates.m$room <- as.numeric(rates.m$room)

#- make an interpolated matrix with colors for plotting
s <- interp(x=rates.m$room,y=rates.m$times,z=rates.m$M.mean)
zlim <- range(s$z)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen) # height color lookup table
colors <- colorlut[ s$z - zlim[1] + 1 ] # assign colors to heights for each point

#- plot
plot3d(x=rates.m$room,y=rates.m$times,z=rates.m$M.mean,axes=F,
       xlab="",ylab="",zlab="",aspect=c(0.8,0.8,1))
rgl.viewpoint(  zoom = 1.3 )
surface3d(x=s$x,y=s$y,z=s$z,color=colors,back="lines")
axes3d( edges=c("y--", "z+"))
axis3d(edge='x--',at =c(1,2,3,4,5,6),
       labels =c("18","21.5","25","28.5","32","35.5"))
box3d()
rgl.snapshot(filename="output/Mass_3d.png",fmt="png")
#-------------------------------------------------------------------------------------










#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#- an alternative approach, using generalizied linear models (GAMs)


#- load libraries from script
source("R/fitGAM/derivSimulCI.R")
source("R/fitGAM/plotCIdate.R")
source("R/fitGAM/smoothplot.R")

# for GAM
library(mgcv)


#-------------------------------------------------------------------------------------
#- fit gam model to each plant one at a time, predict the derivatives
growth.l <- split(d2,d2$Code)
pdf(file="output/Growth_GAM_allplants.pdf", paper = "a4", width = 14/2.54, height = 14/2.54)

log3fits <- output <- data.out <- list()
kgam=5
gamfits <- list()
for(i in 1:length(growth.l)){
  tofit <- growth.l[[i]]
  tofit$lnTotMass <- log(tofit$TotMass)
  
  #- fit the gam
  g <- gam(lnTotMass ~ s(Time, k=kgam), data=tofit)
  
  #- plot fit
  smoothplot(Time, lnTotMass, data=tofit, kgam=kgam)
  title(main=tofit$Code[1])
  
  #- create a vector of "dates" on which to estimate the derivative 
  dates <- seq(min(tofit$Time), max(tofit$Time), by=1)
  
  #- extract the derivative 
  fd <- derivSimulCI(g, samples = 10000, n=length(dates))
  dydt <- fd[[1]]$deriv[,1]
  
  #- put derivatives into a list of dataframes
  gamfits[[i]] <- data.frame(Code=tofit$Code[1],Time=dates,dydt=dydt)
  
  #- get the predicted mass
  newDF <- data.frame(Time=dates) ## needs to be a data frame for predict
  X0 <- exp(predict(g, newDF))    ## exp() needed to convert from ln(mass) to mass
  
  #- put mass into the dataframe
  gamfits[[i]]$predMass <- X0
}
dev.off() # close pdf

#- merge dataframes, combine with treatment key
gamfits.df <- do.call(rbind,gamfits)
key <- unique(subset(d2,select=c("Code","room","prov","Water_trt")))
gamfits2 <- merge(gamfits.df,key,by=c("Code"),all.x=T)
gamfits2$AGR <- gamfits2$dydt*gamfits2$predMass
names(gamfits2)[3] <- "RGR"
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- average across rooms
gamfits2.m <- summaryBy(predMass+AGR+RGR~Time+room,data=subset(gamfits2,Water_trt=="wet"),FUN=c(mean,standard.error))
d.m <- summaryBy(TotMass~Time+room,data=subset(d2,Water_trt=="wet"),FUN=c(mean,standard.error))


#- plot the complex evolution of growth over time
windows(50,60);par(mfrow=c(3,1),mar=c(0,5,0,1),oma=c(5,2,2,2),las=1,cex=1.1,cex.lab=1.5)
palette(rev(brewer.pal(6,"Spectral")))

plotBy(predMass.mean~Time|room,data=gamfits2.m,type="o",ylab=expression(Mass~(g)),pch=16,cex=0,lwd=4,axes=F,
       panel.first=adderrorbars(x=gamfits2.m$Time,y=gamfits2.m$predMass.mean,SE=gamfits2.m$predMass.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(0,1,1),frame.plot=T)
plotBy(TotMass.mean~Time|room,data=d.m,type="p",ylab=expression(Mass~(g)),pch=16,add=T,cex=1.5,
       panel.first=adderrorbars(x=d.m$Time,y=d.m$TotMass.mean,SE=d.m$TotMass.standard.error,direction="updown"))
plotBy(AGR.mean~Time|room,data=gamfits2.m,type="o",ylab=expression(AGR~(g~d^-1)),pch=16,legend=F,cex=0,lwd=4,axes=F,
       panel.first=adderrorbars(x=gamfits2.m$Time,y=gamfits2.m$AGR.mean,SE=gamfits2.m$AGR.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(0,1,1),frame.plot=T)
plotBy(RGR.mean~Time|room,data=gamfits2.m,type="o",ylab=expression(RGR~(g~g^-1~d^-1)),pch=16,legend=F,cex=0,lwd=4,axes=F,
       ylim=c(0,0.2),
       panel.first=adderrorbars(x=gamfits2.m$Time,y=gamfits2.m$RGR.mean,SE=gamfits2.m$RGR.standard.error,direction="updown"))
magaxis(side=c(1,2,4),labels=c(1,1,1),frame.plot=T)
title(xlab="Time (days)",outer=T,adj=0.6)
dev.copy2pdf(file="output/Growth_analysis_GAM.pdf")

#-------------------------------------------------------------------------------------