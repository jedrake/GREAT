#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- This script plots the light-response curves in 3d.
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#- get the AQ data
aq <- getAQ()



#-------------------------------------------------------
#- plot TREATMENT MEANS in 3d
aq.m <- summaryBy(Photo+Tleaf+PARi~TleafFac+LightFac+prov+location,data=subset(aq,campaign==1 & Water_trt=="wet"),FUN=c(mean,standard.error))

palette(rev(brewer.pal(6,"Spectral")))

COL=palette()[c(1,2,6)]
plot3d(x=aq.m$Tleaf.mean,y=aq.m$PARi.mean,z=aq.m$Photo.mean,col=COL[as.numeric(aq.m$location)],size=15,type="p",xlab="",ylab="",zlab="",
       xlim=c(15,40),ylim=c(0,1600),zlim=c(1,30))

aq.l <- split(aq.m,paste(aq.m$prov,aq.m$TleafFac,sep="-"))
for(i in 1:length(aq.l)){
  plot3d(x=aq.l[[i]]$Tleaf.mean,y=aq.l[[i]]$PARi.mean,z=aq.l[[i]]$Photo.mean,col=COL[aq.l[[i]]$location],size=15,type="l",add=T)
  
}
aq.l2 <- split(aq.m,paste(aq.m$prov,aq.m$LightFac,sep="-"))
for(i in 1:length(aq.l2)){
  plot3d(x=aq.l2[[i]]$Tleaf.mean,y=aq.l2[[i]]$PARi.mean,z=aq.l2[[i]]$Photo.mean,col=COL[aq.l2[[i]]$location],size=15,type="l",add=T)
  
}
