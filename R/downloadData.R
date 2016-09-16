#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-- Download GREAT data from HIEv.
#    Assumes "R/loadLibraries.R" has been run, so the HIEv is all set up.
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#- download all files on HIEv for the main part of the "GREAT" experiment
downloadHIEv(searchHIEv("GHS39_GREAT_MAIN"),topath="data")

#- get a few relevant files of the S30 experiment
downloadHIEv(searchHIEv("GHS30_GREAT_MAIN_EUTE-ALA"),topath="data")
downloadHIEv(searchHIEv("GHS30_PCS_BIOG_harvest_130107-130111_L1"),topath="data")
downloadHIEv(searchHIEv("GHS30_PCS_BIOG_PROV-LOCATIONS"),topath="data")
