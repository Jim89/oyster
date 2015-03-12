########################### Set up environment #################################
# load required packages
  library(lubridate)
  library(dplyr)
  library(magrittr)
  library(stringr)
  library(rvest)
  library(ggplot2)
  library(RgoogleMaps)

# run the scripts that get, clean and combine the data together

  ptm <- proc.time()
  print("Getting and cleaning data")
  source("./gettingAndCleaning/01_oysterData.R")
  source("./gettingAndCleaning/02_stationsData.R")
  source("./gettingAndCleaning/03_combined.R")
  print("Complete, took:")
  proc.time() - ptm

########################### Create some plots ##################################