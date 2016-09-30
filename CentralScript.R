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
source("R/loadLibraries.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- download all of the required data. DOI: http://doi.org/10.4225/35/57e4bf22dd3ec.
# Data are placed in "Data/Glasshouse_DRAKE_EUTE_THERMAL-NICHE/data
get_zipdata()
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make figure 1. A simple conceptual model figure (one curve or different curves)
source("R/plotConceptualFig.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Figure 2. Long-term photosynthesis vs. temperature response curves.
source("R/plot_AT_longterm.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figures 3. The respiration across tissue components measured at the end.
source("R/R_components_harvest.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figure 4. Temperature response of final mass and absolute growth rate
source("R/plot_mass_AGR.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figure 5. Growth analysis of the intensive interval
# Plots RGR, LAR, and NAR versus temperature. Why did this change when switching to published data??
source("R/RGR_growth_analysis_interval.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figure 6. LAR decomposition of the growth interval. Leaf mass fraction is way too big!
source("R/plot_LAR_decomposition.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figure 7. A direct comparison of GREAT and Drake et al. (2015) growth responses
source("R/compare GREAT and S30.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Table 1. Temperature response parameters for lots of things
#  This takes a little while to fit all the temperature response curves
#  Exports the table as "output/Table1.csv"
source("R/make_table_Topt.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Table 2. Temperature response parameters for Rmass and Rarea
#  Exports the table as "output/Table2_Rmass_Rmarea.csv"
source("R/make_table_Rdark_Tresponses.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Figure S1. Map of provenances
#plotAussie(export=F)

#- The following can take a long time to download and interpolate all of the climate data
source("R/plot_map_spatial_climate.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Run the code to make Fig. S2 (environmental data over time).
source("R/S39_climate_data.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure S3. Allometry of d2h relative to total plant mass.
returnMassFromAllom(d2hdat=NA,plotson=T,droughtdat=F)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#- Make Figure S4. Temperature response curves of photosynthesis
source("R/photo_AvT.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure S5. Leaf short-term respiration response functions
source("R/plotRleafvsT.R")
#-------------------------------------------------------------------------------------












#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#-- other potentially interesting bits that were not included as figures in the 
#  main text or supplemental




#-------------------------------------------------------------------------------------
#- Check the estimation of total crown leaf area (48 plants were harvested on the day
#   following the leaf counts and average leaf size measurements)
checkLeafAreaEst()
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make a big plot showing the hysteresis in the growth interval data.
# This takes a little while to fit all the temperature response curves
source("R/growth_analysis_interval.R")
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------




