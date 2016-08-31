#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- make a climate envelope figure for E. tereticornis
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

library(dismo)
library(raster)

#-----------------------------------------------------------------------------------------
#- Download the climate data
biodat <- raster::getData("worldclim", var="bio", res=2.5, path="data/Climate/")
biodat5 <- subset(biodat,5)#mean max T of warmest month
biodat6<-subset(biodat,6) #mean min T of coldest month

#- merge climate data with spatial points of E. tereticornis occurances
dist <- read.csv("data/distribution_climate.csv")

xy <- SpatialPoints(cbind(dist$Longitude...processed,dist$Latitude...processed))
dist$bio5 <- extract(biodat5/10,xy,method="bilinear",fun=mean, buffer=15000) #mean max T of warmest month
dist$bio6 <- extract(biodat6/10,xy,method="bilinear",fun=mean, buffer=15000) #mean min T of coldest month

#- my seedlots
myseed <- data.frame(Latitude=c(-35.4,-15.5,-26.57),Longitude=c(150.07,145.14,152.01))
myseed.sp <- SpatialPoints(cbind(myseed$Longitude,myseed$Latitude))
myseed$bio5 <- extract(biodat5/10,myseed.sp,method="bilinear",fun=mean, buffer=15000) #mean max T of warmest month
myseed$bio6 <- extract(biodat6/10,myseed.sp,method="bilinear",fun=mean, buffer=15000) #mean min T of coldest month

#- seedlots from Drake et al. 2015
drakeseed <- read.csv("data/prov_locations_meanT2.csv")
drakeseed <- subset(drakeseed,sp=="t")
drakeseed.sp <- SpatialPoints(cbind(drakeseed$long,drakeseed$lat))
drakeseed$bio5 <- extract(biodat5/10,drakeseed.sp,method="bilinear",fun=mean, buffer=15000) #mean max T of warmest month
drakeseed$bio6 <- extract(biodat6/10,drakeseed.sp,method="bilinear",fun=mean, buffer=15000) #mean min T of coldest month

#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#- plot

#- find the elements of "dist" with tereticornis in the species name
toplot <- dist[grep(pattern="tereticornis",x=dist$Scientific.Name),]
toplot <- toplot[grep(pattern="Australia",x=toplot$Country...parsed),]


windows(55,40);par(mfrow=c(1,2),oma=c(5,3,1,1))
#- plot spatial distribution
plotAussie(export=F)
title(ylab=expression(Latitude~(degree)),outer=F,cex.lab=1.5)
title(xlab=expression(Longitude~(degree)),outer=F,cex.lab=1.5,line=4,xpd=NA)
legend("topright",bty="n",legend=letters[1],cex=2)

#- plot climate distributions
plot(bio6~bio5,dat=toplot,col="grey",axes=F,ylab="",xlab="")
points(bio6~bio5,data=drakeseed,col="black",bg="white",pch=21,cex=2)
points(bio6~bio5,data=myseed,col="black",pch=16,cex=2)
title(ylab=expression(bio6~":"~Mean~minimum~T~of~coldest~month~(degree*C)),xpd="NA",cex.lab=1.5)
title(xlab=expression(bio5~":"~Mean~maximum~T~of~warmest~month~(degree*C)),xpd="NA",cex.lab=1.5)
magaxis(side=c(1,2),labels=c(1,1),frame.plot=T)
legend("topright",bty="n",legend=letters[2],cex=2)
abline(h=0,lty=2,col="grey")
#-----------------------------------------------------------------------------------------
