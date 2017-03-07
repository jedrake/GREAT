#---------------------------------------------------------------------
#- function to load a package, and install it if necessary
Library <- function(pkg, ...){
  
  PACK <- .packages(all.available=TRUE)
  pkgc <- deparse(substitute(pkg))
  
  if(pkgc %in% PACK){
    library(pkgc, character.only=TRUE)
  } else {
    install.packages(pkgc, ...)
    library(pkgc, character.only=TRUE)
  }
  
}
#---------------------------------------------------------------------





#---------------------------------------------------------------------
#- load all the libraries (and install them if needed)
Library(doBy)
Library(magicaxis)
Library(RColorBrewer)
Library(propagate)
Library(gplots)
Library(readxl)
Library(maps)
Library(mapdata)
Library(rgeos)
Library(sp)
Library(raster)
Library(nlstools)
Library(rgl)
Library(mgcv)
Library(scales)
Library(data.table)
Library(dplyr)
Library(dismo)
Library(multcomp)
#---------------------------------------------------------------------



# do this once
r <- require(plotBy)
if(!r)stop("Install the plotBy R package from bitbucket.org/remkoduursma/plotby")
# devtools::install_bitbucket("remkoduursma/plotby")

r <- require(HIEv)
if(!r)stop("Install the HIEv R package from bitbucket.org/remkoduursma/hiev")
# devtools::install_bitbucket("remkoduursma/hiev")




#---------------------------------------------------------------------
#- load the custom functions that do most of the heavy lifting

source("R/generic_functions.R")
source("R/GREAT_functions.R")
#source("R/gamplotfunctions.R")
#---------------------------------------------------------------------
