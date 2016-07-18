#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- Attempt to merge the GREAT and GHS30 experimental data regarding final mass
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#--- Biomass in GREAT
dat.gr1 <- getHarvest()
size <- getSize()

#- pull out just the unique pot and room numbers from teh size dataframe
size2 <- unique(size[,c("Code","Room","W_treatment","location","Tair")])

#- merge pot ids and harvest. Note the pre-treatment plants get excluded here
dat2 <- merge(size2,dat.gr1,by.x=c("Code","location","W_treatment"),by.y=c("Code","location","W_treatment"))
dat.gr <- subset(dat2,Date==as.Date("2016-02-22") & W_treatment=="w")
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- biomass in the GHS30 experiment (my first glasshouse experiment in Oz)
dat.s30 <- read.csv("data/GHS_1_finalharvest.csv")
dat.s30$Tair <- NA

#- add the air temperature of the home plants
dat.s30$Tair[which(dat.s30$treat=="h")] <- dat.s30$homet[which(dat.s30$treat=="h")]

#- add the air temperature of the warmed plants
dat.s30$Tair[which(dat.s30$treat=="w")] <- dat.s30$homet[which(dat.s30$treat=="w")]+3.5

#- get rid of the grandis or recipricol transplanted plants
dat.s30 <- subset(dat.s30,sp=="t" & treat !="r")
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- the origional experiment ran longer, so the plants were a lot larger.
#   So I have to normalize each dataset before comparing them, or some such.

#- normalize great
dat.gr$totdm_norm <- (dat.gr$totdm-min(dat.gr$totdm))/(max(dat.gr$totdm)-min(dat.gr$totdm))

#- normalize S30
dat.s30$totdm_norm <- (dat.s30$totalDM-min(dat.s30$totalDM))/(max(dat.s30$totalDM)-min(dat.s30$totalDM))

#- average the normalized data for both datasets
dat.gr.n <- summaryBy(totdm+totdm_norm~Tair,data=dat.gr,FUN=c(mean,standard.error))
dat.s30.n <- summaryBy(totalDM+totdm_norm~Tair,data=dat.s30,FUN=c(mean,standard.error))
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- plot normalized data relative to the growth temperature

windows(40,50);par(mar=c(6,8,1,1),mfrow=c(1,1),cex.lab=1.5,cex.axis=1.2,oma=c(2,0,0,0))
palette(c("black","darkgrey"))

#- plot the GREAT data
plot(totdm_norm.mean~Tair,data=dat.gr.n,type="o",axes=F,xlab="",ylab="",pch=16,col=palette()[1],
     ylim=c(0,1),cex=2)
adderrorbars(x=dat.gr.n$Tair,y=dat.gr.n$totdm_norm.mean,
                              SE=dat.gr.n$totdm_norm.standard.error,direction="updown")
magaxis(side=c(1,2),labels=c(1,1),las=1,frame.plot=T)

#- overlay the S30 data
adderrorbars(x=dat.s30.n$Tair,y=dat.s30.n$totdm_norm.mean,
             SE=dat.s30.n$totdm_norm.standard.error,direction="updown")
points(totdm_norm.mean~Tair,data=dat.s30.n,type="o",axes=F,xlab="",ylab="",pch=16,col=palette()[2],
     ylim=c(0,1),cex=2)

title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,line=4)
title(ylab=expression(Final~total~mass~(normalized)),cex.lab=2,line=4)

legend("topright",legend=c("This study",expression(Drake~italic(et~al.)~(2015))),pch=16,col=palette()[1:2],cex=1.5)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


