#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Download GREAT data from HIEv.
#    Assumes "R/loadLibraries.R" has been run, so the HIEv is all set up.
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#- download all files on HIEv for the main part of the "GREAT" experiment
downloadHIEv(searchHIEv("GHS39_GREAT_MAIN"),topath="data")
