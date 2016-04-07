
setwd("~/Work/R/fitGAM")

# for GAM
library(mgcv)

# My 'smoothplot' function, similar to plot(g) but much nicer
source("smoothplot.R")

# functions from 
# http://www.fromthebottomoftheheap.net/2014/06/16/simultaneous-confidence-intervals-for-derivatives/
source("derivSimulCI.R")

# My function to plot the derivative and color significantly positive increases, etc.
# NOTE: this function assumes that the model was fit by Date, and that Date is available in the 
# dataframe (as a proper Date object). 
source("plotCIdate.R")


# Some example data.
dat <- data.frame(Date=seq.Date(as.Date("2015-1-1"), as.Date("2015-12-1"), by="1 day"))
dat$DOY <- as.numeric(dat$Date - as.Date("2015-1-1"))+1
dat$Yvar <- 1.5 + sin(dat$DOY / 50) + rnorm(nrow(dat), sd=0.4, mean=0.0)

# Fit gam or gamm.

# GAMM : mixed-effects gam, has a random effect. In this case, FACE Ring.
#g <- gamm(LAI ~ s(Time, k=kgam), random = list(Ring=~1), data=df)
# GAM - no random effects.
# Your GAM cannot have a Date object; as above make a simple numeric date.
# Set k to a 'reasonable' value, so that it is not too smooth, and not too flat.
g <- gam(Yvar ~ s(DOY, k=8), data=dat)

# Try also plot(g), but this is nicer:
smoothplot(DOY, Yvar, data=dat, kgam=8)

# The derivatives
# Green means significantly positive; red significantly negative. See code to change appearance.
plotCIdate(g, dat)
