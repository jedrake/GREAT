#- this script compares the short and long-term photosynthetic T-responses
#-  Run BOTH photo_AvT.R and photo_AQ.R first!

#- get the short term data and predictions
ST <- data.frame(do.call(rbind,
                              list(AvTfits.list.st[[1]][[2]],AvTfits.list.st[[2]][[2]],AvTfits.list.st[[3]][[2]])))
names(ST) <- paste(names(ST),"ST",sep=".")
ST$prov <- c(rep("A",51),rep("B",51),rep("C",51))

#- get the long term data and predictions
LT <- data.frame(do.call(rbind,
                             list(AvTfits.list[[1]][[2]],AvTfits.list[[2]][[2]],AvTfits.list[[3]][[2]])))
names(LT) <- paste(names(LT),"LT",sep=".")
LT$prov <- c(rep("A",51),rep("B",51),rep("C",51))

#- merge predictions for plotting
dat <- merge(ST,LT,by="prov")

#-- plot
windows(60,30);par(mfrow=c(1,3),mar=c(6,6,1,0),oma=c(1,1,1,3),cex.lab=2)
COL=palette()[3:2]

dat.l <- split(dat,dat$prov)
for(i in 1:length(dat.l)){
  toplot <- dat.l[[i]]
  
  #- plot short term
  plotBy(Sim.Mean.ST~Tleaf.ST|prov,data=toplot,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=2,
         ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),
         xlab=expression(T[leaf]~(degree*C)))
  title(main=paste("Provenance",toplot$prov[1],sep=" "),cex.main=2,line=-2)
  polygon(x = c(toplot$Tleaf.ST, rev(toplot$Tleaf.ST)), 
          y = c(toplot$Sim.97.5..ST, rev(toplot$Sim.2.5..ST)), col = alpha(COL[1],0.5), border = NA)
  if(i==1) legend("bottomleft",c("Short-term","Long-term"),fill=COL[1:2],ncol=1,cex=2)
  
  #- add long-term
  plotBy(Sim.Mean.LT~Tleaf.LT|prov,data=toplot,legend=F,type="l",las=1,ylim=c(0,30),lwd=3,cex.lab=2,
         ylab=expression(A[sat]~(mu*mol~m^-2~s^-1)),add=T,
         xlab=expression(T[leaf]~(degree*C)))
  polygon(x = c(toplot$Tleaf.LT, rev(toplot$Tleaf.LT)), 
          y = c(toplot$Sim.97.5..LT, rev(toplot$Sim.2.5..LT)), col = alpha(COL[2],0.5), border = NA)
}
