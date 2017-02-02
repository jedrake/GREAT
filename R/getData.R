
#- function to download the zipfile containing all of the data.
get_zipdata <- function(){
  
  #------------------------------------------------------------------------------------------------------------------
  #- check if the data and output directories exist. If they don't, create them.
  dir.create(file.path("Data"),showWarnings=F)
  dir.create(file.path("Output"),showWarnings=F)
  
  #- define the file name and the URL to the data
  zipfn <- "Glasshouse_DRAKE_EUTE_local-adaptation.zip"
  url <-   "http://research-data.westernsydney.edu.au/redbox/verNum1.8-SNAPSHOT/default/detail/baa6e7b38113a59b8e9d2e6b0fb4009a/Glasshouse_DRAKE_EUTE_THERMAL-NICHE.zip"
  
  
  #- download the data, if there is no local copy
  failureFlag <- 0
  if(!file.exists(zipfn)){
    failureFlag <- try(download.file(url, zipfn, mode="wb"))
  }
  
  #- unzip the data
  unzip(zipfn, exdir="Data", overwrite=TRUE)
  
  #- print an informative error message if downloading fails
  if(failureFlag !=0){
    message("Download failed. Perhaps try downloading the data manually by pointing a web-browser to http://doi.org/10.4225/35/57e4bf22dd3ec")
  }

}