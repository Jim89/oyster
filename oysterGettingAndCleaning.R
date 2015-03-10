############### set up working environment and load packages ###################
library(lubridate)
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

source("./functions/getStationsData.R")

########################## Get the raw data to be used #########################

# Raw Oyster Data (provided by TFL) --------------------------------------------
files <-
  list.files("./data",recursive = T) %>% as.list %>% paste0("./data/", .)

# read in the data
oyster <-
  lapply(files, function(x) read.csv(x, stringsAsFactors = F, skip = 1)) %>% rbind_all

# rail stations Data from Wikipedia --------------------------------------------
# get railway stations information
  railStations <- getRailStations()

# tube stations Data from Wikipedia --------------------------------------------
# starting from Baker St, find all links on the page
  links <- extractLinks()

# loop over the links and try to extract the coordinates from the page
  geos <- sapply(1:length(links), function(x) try(extractGeo(links[x]), silent = T))

# clean up the coordinates
  geosClean <- sapply(1:length(geos), function(x) geos[[x]][1])

# loop over the links and extract the station name from the page
  titles <- sapply(1:length(links), function(x) try(extractTitle(links[x]), silent = T))

# clean up the station names
  titleClean <- sapply(1:length(titles), function(x) titles[[x]][1])

# combine in to one data frame
  locations <- cbind(geosClean, titleClean) %>% data.frame()

# clean up
  rm(list = c("files", "links", "geos","geosClean", "titles", "titleClean"))
  gc()


############################# Clean the data ##################################
# set all names to lowercase because I'm lazy
  names(railStations) %<>% tolower
  names(oyster) %<>% tolower
  names(locations) %<>% tolower

# Oyster Data ------------------------------------------------------------------
badRecords <- "touch-in|Topped-up|touch-out|Season ticket|Bus journey|Topped up|Entered and exited"

oyster %<>%
  .[-grep(badRecords, .$journey.action),] %>%
  mutate(date.clean = dmy(date),
         start.time.clean = ifelse(start.time == "", "00:00", start.time) %>%
                                    paste0(":00"),
         end.time.clean = ifelse(end.time == "", "00:00", end.time) %>%
                                  paste0(":00"),
         start.datetime = paste(date, start.time.clean, sep = " ") %>% dmy_hms(),
         end.datetime = paste(date, end.time.clean, sep = " ") %>% dmy_hms(),
         journey.time = ((end.datetime - start.datetime)) %>% as.duration(),
         start.day = wday(start.datetime, label = T),
         end.day = wday(end.datetime, label = T))







# Rail stations data -----------------------------------------------------------
# extract lat/long
coordsRaw  <-  railStations$coordinates %>% str_split("/")

# clean up
coordsClean <- sapply(1:length(coordsRaw), function(x) coordsRaw[[x]][[3]]) %>%
  str_trim() %>%
  str_split(";")


lattitude <- sapply(1:length(coordsClean), function(x) coordsClean[[x]][1]) %>%
  str_trim() %>%
  as.numeric()

longitude <- sapply(1:length(coordsClean), function(x) coordsClean[[x]][2]) %>%
  str_trim() %>%
  str_extract("-[0-9].[0-9]*|[0-9].[0-9]*") %>%
  as.numeric()


# add latt and long back to data frame
railStations %<>% mutate(long = longitude,
                         lat = lattitude) %>%
  select(-coordinates)

# Tube stations data -----------------------------------------------------------
# find those pages that weren't really stations/where the link didn't work
  errors <- grep("[Tt]alk|[Gg]roup|[Cc]ategory|[Ee]rror|[Cc]hanges|[Pp]ages|[Ff]ile|[Ll]ist", locations$titleClean)

# cut out the broken links and take unique values
  locations <- locations[-errors,] %>% unique()

# split up the coordinates (currently lat and long in one field)
  geoSplit <- str_split(locations$geosClean, ";")

# pick up lat
  lat <- sapply(1:length(geoSplit), function(x) geoSplit[[x]][1]) %>% as.numeric

# pick up long
  long <- sapply(1:length(geoSplit), function(x) geoSplit[[x]][2]) %>% as.numeric

# combine in to one data set
  tubeStations <- data.frame(station = locations$titleClean, long, lat)

# combine all stations w/ coordinates in to one --------------------------------

stations <- railStations %>% select(station, long, lat) %>%
            rbind(tubeStations) %>% arrange(station)

stations$station %<>% gsub("[Ss]tation|[Tt]ube|\\[|\\]|[0-9]|", "", .) %>% str_trim
stations %<>% unique()


