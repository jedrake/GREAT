#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- This script makes a table of the temperature response parameters
#   for area- and mass-based leaf respiration
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")
#-----------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#- get the RvT data
rvt <- getRvT()
#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#- fit RvT to estimate Q10 and Rref, for the DIRECT 
#   short term fits only!
tofit <- rvt
tofit.l <- split(tofit,tofit$location)

#- fit all the curves for mass and area based rates
RvTfits.list.mass <- lapply(tofit.l,FUN=fitRvT,namex="Tleaf",namey="Rmass",lengthPredict=20,start=list(Rref=10,Q10=2))
RvTfits.list.area <- lapply(tofit.l,FUN=fitRvT,namex="Tleaf",namey="Rarea",lengthPredict=20,start=list(Rref=1,Q10=2))


#---- pull out the parameter means and SE's for plotting. Add confidence intervals.
#- Rmass
RvTfits.mass <- data.frame(do.call(rbind,
                              list(RvTfits.list.mass[[1]][[1]],RvTfits.list.mass[[2]][[1]],RvTfits.list.mass[[3]][[1]])))
RvTfits.mass$location <- levels(rvt$location)
RvTfits.massCI <- data.frame(do.call(rbind,
                                   list(RvTfits.list.mass[[1]][[3]],RvTfits.list.mass[[2]][[3]],RvTfits.list.mass[[3]][[3]])))
names(RvTfits.massCI) <- c("Rmassref.CI","Q10.CI")
RvTfits.mass <- cbind(RvTfits.mass,RvTfits.massCI)

#- Rarea
RvTfits.area <- data.frame(do.call(rbind,
                                   list(RvTfits.list.area[[1]][[1]],RvTfits.list.area[[2]][[1]],RvTfits.list.area[[3]][[1]])))
RvTfits.area$location <- levels(rvt$location)
RvTfits.areaCI <- data.frame(do.call(rbind,
                                     list(RvTfits.list.area[[1]][[3]],RvTfits.list.area[[2]][[3]],RvTfits.list.area[[3]][[3]])))
names(RvTfits.areaCI) <- c("Rarearef.CI","Q10.CI")
RvTfits.area <- cbind(RvTfits.area,RvTfits.areaCI)



#- make table
table2 <- data.frame(Variable=c(rep("Rmass",3),rep("Rarea",3)),
                                location=rep(c("Cold","Central","Warm"),2),
                                Rref = NA, Q10=NA)
table2[1:3,3] <- mktable(location=RvTfits.mass$location,yvar=RvTfits.mass$Rmassref,se=RvTfits.mass$Rmassref.CI)                     
table2[1:3,4] <- mktable(location=RvTfits.mass$location,yvar=RvTfits.mass$Q10,se=RvTfits.mass$Q10.CI,nchar1=2,nchar2=2)
table2[4:6,3] <- mktable(location=RvTfits.area$location,yvar=RvTfits.area$Rarearef,se=RvTfits.area$Rarearef.CI,nchar1=2,nchar2=2)                     
table2[4:6,4] <- mktable(location=RvTfits.area$location,yvar=RvTfits.area$Q10,se=RvTfits.area$Q10.CI,nchar1=2,nchar2=2)

write.csv(table2,file="output/Table2_Rmass_Rarea.csv",row.names=F)
