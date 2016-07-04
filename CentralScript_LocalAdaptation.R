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
#- Make figure 2. Temperature response of final mass and absolute growth rate
source("R/plot_figure2.R")
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make figure 3. Growth analysis of the intensive interval
# Plots RGR, LAR, and NAR versus temperature
source("R/growth_analysis_interval_figure3.R")

#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make figure 4. LAR decomposition of the growth interval
source("R/plot_LAR_decomposition.R")
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make figures 5 and 6. The respiration across tissue components measured at the end.
source("R/R_components_harvest.R")
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make figure 5. The direct short-term temperature response curves.
#  This takes a little while to fit all the temperature response curves
source("R/photo_AvT.R")      # area-based
source("R/photo_AvT_mass.R") # mass-based (Figure S4)
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
plotAussie(export=F)
#-------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------
#- Make Figure S2. Allometry of d2h relative to total plant mass.
returnMassFromAllom(d2hdat=25,plotson=T,droughtdat=F)
#-------------------------------------------------------------------------------------




#-------------------------------------------------------------------------------------
#- Make Figure SX?. Leaf short-term respiration response functions
source("R/plotRleafvsT.R")
#-------------------------------------------------------------------------------------



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

