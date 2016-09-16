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
#- biomass in the GHS30 experiment (Drake 2015 GCB)
#dat.s30 <- read.csv("data/GHS_1_finalharvest.csv")
dat.s30 <- read.csv("data/GHS30_PCS_BIOG_harvest_130107-130111_L1.csv")

#- add in the home temperatures. Assumes the datafile is in the original sort order!
dat.s30$homet <- c(rep(21.5,39),rep(25,28),rep(28.5,30),
                   rep(18,20),rep(21.5,37),rep(25,30),rep(28.5,29))

#- add the air temperature of the home plants
dat.s30$Tair <- NA
dat.s30$Tair[which(dat.s30$treat=="h")] <- dat.s30$homet[which(dat.s30$treat=="h")]

#- add the air temperature of the warmed plants
dat.s30$Tair[which(dat.s30$treat=="w")] <- dat.s30$homet[which(dat.s30$treat=="w")]+3.5

#- get rid of the grandis or recipricol transplanted plants
dat.s30 <- subset(dat.s30,sp=="t" & treat !="r")

#- make a new factor variable that is a combination of homet and treat
dat.s30$combotrt <- factor(paste(dat.s30$treat,dat.s30$homet,sep="-"))
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------





#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- the origional experiment ran longer, so the plants were a lot larger.
#   So I have to normalize each dataset before comparing them, or some such.

#- normalize great
#dat.gr$totdm_norm <- (dat.gr$totdm-min(dat.gr$totdm))/(max(dat.gr$totdm)-min(dat.gr$totdm))
dat.gr$totdm_norm <- dat.gr$totdm/11.345076 # normalize by average mass in 28.5 degree room

#- normalize S30
#dat.s30$totdm_norm <- (dat.s30$totalDM-min(dat.s30$totalDM))/(max(dat.s30$totalDM)-min(dat.s30$totalDM))
dat.s30$totdm_norm <- dat.s30$totalDM/30.27427 # normalize by average mass in 28.5 degree room

#- average the normalized data for both datasets
dat.gr.n <- summaryBy(totdm+totdm_norm~Tair,data=dat.gr,FUN=c(mean,standard.error))
dat.s30.n <- summaryBy(totalDM+totdm_norm~Tair+combotrt,data=dat.s30,FUN=c(mean,standard.error))
dat.s30.n$treat <- factor(substr(as.character(dat.s30.n$combotrt),start=1,stop=1))
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- plot normalized data relative to the growth temperature

windows(40,50);par(mar=c(6,8,1,1),mfrow=c(1,1),cex.lab=1.5,cex.axis=1.2,oma=c(2,0,0,0))
palette(c("red","green","blue"))
palette(c("red","green","blue",rev(brewer.pal(11,"Spectral"))))

#- plot the GREAT data
plot(totdm_norm.mean~Tair,data=dat.gr.n,type="o",axes=F,xlab="",ylab="",pch=16,col="black",
     ylim=c(0,1.2),cex=2,legend=F)
adderrorbars(x=dat.gr.n$Tair,y=dat.gr.n$totdm_norm.mean,
                              SE=dat.gr.n$totdm_norm.standard.error,direction="updown")
magaxis(side=c(1,2),labels=c(1,1),las=1,frame.plot=T)

#- overlay the S30 data
adderrorbars(x=dat.s30.n$Tair,y=dat.s30.n$totdm_norm.mean,
             SE=dat.s30.n$totdm_norm.standard.error,direction="updown")
plotBy(totdm_norm.mean~Tair|combotrt,data=dat.s30.n,type="o",axes=F,xlab="",ylab="",pch=21,col=c("black"),
     ylim=c(0,1),cex=2,add=T,legend=F)
palette(c("blue","red"))
points(totdm_norm.mean~Tair,data=dat.s30.n,add=T,pch=21,cex=2,legend=F,col="black",bg=treat)

arrows(x0=dat.s30.n$Tair[c(1,2,4,6)],y0=dat.s30.n$totdm_norm.mean[c(1,2,4,6)],
       x1=dat.s30.n$Tair[c(3,5,7,8)],y1=dat.s30.n$totdm_norm.mean[c(3,5,7,8)],
       code=2,lty=2)


title(xlab=expression(Growth~T[air]~(degree*C)),cex.lab=2,line=4)
title(ylab=expression(Final~total~mass~(normalized)),cex.lab=2,line=4)

legend("bottomright",legend=c("This study",expression(Drake~italic(et~al.)~(2015)~Home),
                              expression(Drake~italic(et~al.)~(2015)~Warmed)),pch=c(21,21,21)
       ,col="black",cex=1.5,pt.bg=c("black","blue","red"))

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


