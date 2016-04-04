
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")
palette(rev(brewer.pal(6,"Spectral")))



#-------------------------------------------------------------------------------------
#-- function to interpret 2nd order log-polynomial curve fits
output.log_lin <- function(X, Y, params, times,Code){
  a <- params[1]; b <- params[2]; c <- params[3]
  #fitted <- (M0^(1-beta)   + r  * X *(1-beta))^(1/(1-beta))
  fitted <- exp(a + b*X + c*X^2)
  resid  <- Y - fitted
  data <- data.frame(time=X,observed = Y, fitted = fitted, resid = resid)
  #eq   <- bquote(paste((.(round(r * (1-beta), 3))*t)^.(round(1/(1-beta), 2))))
  eq <- bquote(a+bx+cx^2)
  mss  <- sum((fitted - mean(fitted))^2)
  rss  <- sum(resid^2)
  R2   <- mss/(mss + rss)
  rmse <- sqrt(rss)
  N <- length(X)
  logLik <- -N * (log(2 * pi) + 1 - log(N) + log(sum(resid^2)))/2
  AIC  <- -2 * logLik  + 2 * 3 # three parameters
  summary <- c(R2 = R2, AIC = AIC, RMSE = rmse)
  temp <- a + b*X + c*X^2 # fix from here down
  rates = data.frame(
    times = times,
    M    =  exp(a+b*times+c*times^2))                  # from Hunt- Plant Growth Curves
  rates$AGR  =  with(rates,M*(b+2*c*times))            # from Hunt- Plant Growth Curves
  rates$RGR <- rates$AGR/rates$M
  rates$Code <- Code
  out <- list(params = params[-4], summary = summary, equation = eq, data = data, rates = rates)
  return(out)
}
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


#- plot the complex evolution of growth over time
windows(50,60);par(mfrow=c(3,1),mar=c(0,5,0,1),oma=c(5,2,2,2),las=1,cex=1.1,cex.lab=1.5)
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
colorlut <- terrain.colors(zlen) # height color lookup table
zlim <- range(s$z)
zlen <- zlim[2] - zlim[1] + 1
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





