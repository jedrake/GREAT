#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#- Central analysis script for the GREAT local adaptation manuscript.
#  The idea is to keep this script nice and tidy, but reproducibly do all the
#  analysis and make all of the figures for the manuscript.
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- load the packages and custom functions that do all the work
source("R/loadLibraries.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- download all of the required data from HIEv.
setToken(tokenfile="HIEv_token.txt") #- set HIEv token. See ?setToken
source("R/downloadData.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make figure 1. A simple conceptual model figure (one curve or different curves)
source("R/plotConceptualFig.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Figure 2. Long-term photosynthesis vs. temperaure response curves.
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
# Plots RGR, LAR, and NAR versus temperature
source("R/RGR_growth_analysis_interval.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make figure 4. LAR decomposition of the growth interval
source("R/plot_LAR_decomposition.R")
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
#  Note that this takes quite some time to run, as the met files are huge.
source("R/S39_climate_data.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure S3. Allometry of d2h relative to total plant mass.
returnMassFromAllom(d2hdat=25,plotson=T,droughtdat=F)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
#- Make Fig S4. Make a big plot showing the hysteresis in the growth interval for the supplemental.
# This takes a little while to fit all the temperature response curves
source("R/growth_analysis_interval.R")
#-------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------
#- Make Figure S5. Leaf short-term respiration response functions
source("R/plotRleafvsT.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Figure S6. The direct short-term temperature response curves.
#  This takes a little while to fit all the temperature response curves
#  This code needs adjustment to exclude the low PAR data.
source("R/photo_AvT.R")      # area-based
source("R/photo_AvT_mass.R") # mass-based 
#-------------------------------------------------------------------------------------















#-- other stuff.


#-------------------------------------------------------------------------------------
#- Check the estimation of total crown leaf area (48 plants were harvested on the day
#   following the leaf counts and average leaf size measurements)
checkLeafAreaEst()
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make a big plot showing the hysteresis in the growth interval for the supplemental.
# This takes a little while to fit all the temperature response curves
source("R/growth_analysis_interval.R")
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make three dimensional plot of A-Q curves.
source("R/plot_AQ_3d.R")

#- rotate the rgl plot to your liking, then run the following code.
#  Import the image into powerpoint and add axis labels etc.
rgl.snapshot(filename="output/Aq_3d_provenances.png",fmt="png")
#-------------------------------------------------------------------------------------

