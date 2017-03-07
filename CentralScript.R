#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#- Central analysis script for the GREAT local adaptation manuscript, entitled
#  "A common and broad thermal niche among geographically diverse populations of the 
#      widely distributed tree species Eucalyptus tereticornis: 
#      no evidence for adaptation to climate of origin".
#  The idea is to keep this script nice and tidy, but reproducibly do all the
#  analysis and make all of the figures for the manuscript. Data will be placed in
#  the "data" folder, while figures and tables will be placed in the "output" folder.
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- load the packages and custom functions that do all the work.
#   This will install a bunch of required libraries, including some non-standard stuff.
#   You may need to open this file and install some packages using Rtools and the devtools package.
source("R/loadLibraries.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- download all of the required data. DOI: http://doi.org/10.4225/35/57e4bf22dd3ec.
# Data are placed in "Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data
get_zipdata()
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Figure 1. A simple conceptual model figure (one curve or different curves)
source("R/plotConceptualFig.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure 2. Map of provenances
#- This can take a long time (~5 minutes) to download and interpolate all of the climate data
source("R/plot_map_spatial_climate.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Figure 3. Long-term photosynthesis vs. temperature response curves.
source("R/plot_AT_longterm.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure 4. The respiration of leaves, stems, and roots
source("R/R_components_harvest.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figure 5. Temperature response of final mass and absolute growth rate
source("R/plot_mass_AGR.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure 6. Growth analysis
source("R/RGR_growth_analysis_interval.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure 7. LAR decomposition
source("R/plot_LAR_decomposition.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure 8. A direct comparison of GREAT and Drake et al. (2015) growth responses
source("R/compare GREAT and S30.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Table 1. Temperature response parameters for lots of things
#  This takes a few minutes to fit all the temperature response curves
#  Exports the table as "output/Table1.csv"
source("R/make_table_Topt.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Table 2. Temperature response parameters for Rmass and Rarea
#  Exports the table as "output/Table2_Rmass_Rmarea.csv"
source("R/make_table_Rdark_Tresponses.R")
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make Figure S1. Environmental data
source("R/S39_climate_data.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure S2. Allometry of d2h relative to total plant mass.
returnMassFromAllom(d2hdat=NA,plotson=T,droughtdat=F)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#- Make Figure S3. Temperature response curves of photosynthesis
source("R/photo_AvT.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure S4. Leaf short-term respiration response functions
source("R/plotRleafvsT.R")
#-------------------------------------------------------------------------------------
