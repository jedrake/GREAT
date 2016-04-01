
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")


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
}
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

#- overlay predicted and measured mass values. Does the model work? Yes it works pretty good.
windows(60,60);par(mfrow=c(1,1),mar=c(6,7,0,0),oma=c(0,0,0,0),las=1,cex=1.1,cex.lab=1.5)
plotBy(M.mean~times|room,data=rates.m,type="o",ylab=expression(Mass~(g)),pch=1,cex=1,lwd=2,
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$M.mean,SE=rates.m$M.standard.error,direction="updown"))
d.m <- summaryBy(TotMass~Time+room,data=subset(d2,Water_trt=="wet"),FUN=c(mean,standard.error),na.rm=T)
plotBy(TotMass.mean~Time|room,data=d.m,type="p",ylab=expression(Mass~(g)),pch=15,add=T,cex=2.5,
       panel.first=adderrorbars(x=d.m$Time,y=d.m$TotMass.mean,SE=d.m$TotMass.standard.error,direction="updown"))


#- plot the complex evolution of growth over time
windows(50,60);par(mfrow=c(3,1),mar=c(0,6,0,0),oma=c(2,2,2,2),las=1,cex=1.1,cex.lab=1.5)
plotBy(M.mean~times|room,data=rates.m,type="o",ylab=expression(Mass~(g)),pch=15,
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$M.mean,SE=rates.m$M.standard.error,direction="updown"))
plotBy(AGR.mean~times|room,data=rates.m,type="o",ylab=expression(AGR~(g~d^-1)),pch=15,legend=F,
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$AGR.mean,SE=rates.m$AGR.standard.error,direction="updown"))
plotBy(RGR.mean~times|room,data=rates.m,type="o",ylab=expression(RGR~(g~g^-1~d^-1)),pch=15,legend=F,ylim=c(0,0.2),
       panel.first=adderrorbars(x=rates.m$times,y=rates.m$RGR.mean,SE=rates.m$RGR.standard.error,direction="updown"))
#-------------------------------------------------------------------------------------








