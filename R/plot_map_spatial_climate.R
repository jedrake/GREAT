#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#- make a climate envelope figure for E. tereticornis
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#- Download the climate data
dir.create(file.path("data/Climate"),showWarnings=F)
biodat <- raster::getData("worldclim", var="bio", res=2.5, path="data/Climate/")
biodat5 <- subset(biodat,5)#mean max T of warmest month
biodat6<-subset(biodat,6) #mean min T of coldest month

#- merge climate data with spatial points of E. tereticornis occurances
dist <- read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS30_GREAT_MAIN_EUTE-ALA.csv")

xy <- SpatialPoints(cbind(dist$Longitude...processed,dist$Latitude...processed))
dist$bio5 <- extract(biodat5/10,xy,method="bilinear",fun=mean, buffer=15000) #mean max T of warmest month
dist$bio6 <- extract(biodat6/10,xy,method="bilinear",fun=mean, buffer=15000) #mean min T of coldest month

#- my seedlots
myseed <- data.frame(Latitude=c(-35.4,-15.5,-26.57),Longitude=c(150.07,145.14,152.01))
myseed.sp <- SpatialPoints(cbind(myseed$Longitude,myseed$Latitude))
myseed$bio5 <- extract(biodat5/10,myseed.sp,method="bilinear",fun=mean, buffer=15000) #mean max T of warmest month
myseed$bio6 <- extract(biodat6/10,myseed.sp,method="bilinear",fun=mean, buffer=15000) #mean min T of coldest month

#- seedlots from Drake et al. 2015
drakeseed <- read.csv("Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data/GHS30_PCS_BIOG_PROV-LOCATIONS.csv")
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

pdf(file="output/FigureS1-Map.pdf",width=7.3,height=6)
par(mfrow=c(1,2),mar=c(3,8,1,1),oma=c(6,6,1,5))
#- plot spatial distribution
plotAussie(export=F)
title(ylab=expression(Latitude~(degree)),outer=F,cex.lab=1.2,xpd=NA)
title(xlab=expression(Longitude~(degree)),outer=F,cex.lab=1.2,line=4,xpd=NA)
legend("topright",bty="n",legend=letters[1],cex=1.2)

#- plot climate distributions
plot(bio6~bio5,dat=toplot,col="grey",axes=F,ylab="",xlab="")
points(bio6~bio5,data=drakeseed,col="black",bg="white",pch=21,cex=2)
points(bio6~bio5,data=myseed,col="black",pch=16,cex=2)
title(ylab=expression(atop(bio6~":"~Mean~minimum~T~of,
                           coldest~month~(degree*C))),xpd="NA",cex.lab=1.2,line=-13,xpd=NA)
title(xlab=expression(atop(bio5~":"~Mean~maximum~T,
                           of~warmest~month~(degree*C))),xpd="NA",cex.lab=1.2,line=4)
magaxis(side=c(1,2,4),labels=c(1,0,1),frame.plot=T)
legend("topright",bty="n",legend=letters[2],cex=1.2)
abline(h=0,lty=2,col="grey")
dev.off()
#dev.copy2pdf(file="output/FigureS1-Map.pdf")
#-----------------------------------------------------------------------------------------
