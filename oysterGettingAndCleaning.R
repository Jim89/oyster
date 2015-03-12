############### set up working environment and load packages ###################
library(lubridate)
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

############### create helper functions for getting data #######################
# railway stations data from wikipedia -----------------------------------------
GetRailStations <-
  function(url = "http://en.wikipedia.org/wiki/List_of_London_railway_stations") {
    # function that loads a URL and extracts the first table from the page
    # by default it will operate on the Wikipedia list of london railway
    # stations page. The first table contains the location data for each station
  url %>%
  html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table %>%
  tbl_df()
}

# tube stations from wikipedia -------------------------------------------------
# starting with Baker street, get the links to the other stations
ExtractLinks <-
function(url = "http://en.wikipedia.org/wiki/Baker_Street_tube_station") {
  # takes a URL and extracts all links from it
  url %>%
    html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("wiki", ., value = T) %>%
    grep("station", ., value = T) %>%
    paste0("http://en.wikipedia.org",.)
}

# function to extract coordinates from a station page
ExtractGeo <- function(url) {
  # takes a URL and uses the CSS selector provided by SelectorGadget widget to
  # extract the dirty coordinates data from it
  url %>%
    html() %>%
    html_nodes("span span span span span span.geo") %>%
    html_text()
}

# function to extract station name
ExtractTitle <- function(url) {
  # takes a URL and uses the CSS selector provided by SelectorGadget widget to
  # extract the first header, which in this case is the wikipedia page title
  url %>%
    html() %>%
    html_nodes("div h1") %>%
    html_text()
}

########################## Get the raw data to be used #########################

# Raw Oyster Data (provided by TFL) --------------------------------------------
files <-
  list.files("./data",recursive = T) %>% as.list %>% paste0("./data/", .)

# read in the data
oyster <-
  lapply(files, function(x) read.csv(x, stringsAsFactors = F, skip = 1)) %>% rbind_all

# rail stations Data from Wikipedia --------------------------------------------
# get railway stations information
  railStations <- GetRailStations()

# tube stations Data from Wikipedia --------------------------------------------
# starting from Baker St, find all links on the page
  links <- ExtractLinks()

# loop over the links and try to extract the coordinates from the page
  geos <- sapply(1:length(links), function(x) try(ExtractGeo(links[x]), silent = T))

# clean up the coordinates
  geosClean <- sapply(1:length(geos), function(x) geos[[x]][1])

# loop over the links and extract the station name from the page
  titles <- sapply(1:length(links), function(x) try(ExtractTitle(links[x]), silent = T))

# clean up the station names
  titleClean <- sapply(1:length(titles), function(x) titles[[x]][1])

# combine in to one data frame
  locations <- cbind(geosClean, titleClean) %>% data.frame()

# clean up
  rm(list = c("files", "links", "geos","geosClean", "titles", "titleClean"))
  gc()

############### create helper functions for rounding times #####################
# CelingTime- Round at time UP with arbitrary precision
# based on FloorTime sourced from here: http://stackoverflow.com/questions/16803867/round-a-date-in-r-to-an-arbitrary-level-of-precision
CeilingTime <- function(x, k = 1, unit = c("second", "minute", "hour", "day",
                                            "week", "month", "year")) {
# Function requires lubridate - load it
  library(lubridate, quietly=T)

  nmax <- NULL

  switch(unit, second = {nmax <- 60},
               minute = {nmax <- 60},
               hour   = {nmax <- 24})

  cuts <- seq(from = 0, to = nmax - 1, by = k)

# Rounds times down to the nearest kth unit interval
  rounded <-switch(unit,
            second = update(x, seconds = cuts[findInterval(second(x), cuts)]),
            minute = update(x, minutes = cuts[findInterval(minute(x), cuts)],
                               seconds = 0),
            hour   = update(x, hours = cuts[findInterval(hour(x), cuts)],
                               minutes = 0, seconds = 0),
            day    = update(x, hours = 0, minutes = 0, seconds = 0),
            week   = update(x, wdays = 1, hours = 0, minutes = 0, seconds = 0),
            month  = update(x, mdays = 1, hours = 0, minutes = 0, seconds = 0),
            year   = update(x, ydays = 1, hours = 0, minutes = 0, seconds = 0))

# Round up to the next kth unit interval
  if (unit=="second") {
    rounded <- rounded + seconds(k)
  } else if (unit == "minute") {
      rounded <- rounded + minutes(k)
  } else if (unit == "hour") {
      rounded <- rounded + hours(k)
  } else if (unit == "day") {
      rounded <- rounded + days(k)
  } else if (unit == "week") {
      rounded <- rounded + weeks(k)
  } else if (unit == "month") {
      rounded <- rounded + months(k)
  } else if(unit == "year") {
      rounded <- rounded + years(k)
    }
  return(rounded)
}


############################# Clean the data ##################################

  names(railStations) %<>% tolower
  names(oyster) %<>% tolower
  names(locations) %<>% tolower

# Oyster Data ------------------------------------------------------------------
# find records where something went awry in the journey
  badRecords <- "touch-in|Topped-up|touch-out|Season ticket|Bus journey|Topped up|Entered and exited|Unspecified location"

# create clearner times, dates and datetimes
  oyster %<>%
  .[-grep(badRecords, .$journey.action),] %>%
  mutate(start.time.clean = start.time %>% paste0(":00"),
         end.time.clean = end.time %>% paste0(":00"),
         date.clean = dmy(date),
         start.datetime = paste(date, start.time.clean, sep = " ") %>% dmy_hms(),
         end.datetime = paste(date, end.time.clean, sep = " ") %>% dmy_hms())
         )

# find records where I touched out after mighnight
  afterMidnight <- substring(oyster$end.time,1,2) %in% c("00","01")

# set the end datetimes to be the next day (i.e. after midnight) where needed
  oyster[afterMidnight, 13]  <-  oyster[afterMidnight, 13] + days(1)


# create journey times and days of the week
  oyster %<>%
  mutate(start.datetime.rounded = CeilingTime(start.datetime, 15, "minute"),
         journey.time = difftime(end.datetime, start.datetime, units = "mins"),
         start.day = wday(start.datetime, label = T)
        )

# split up the journey in to "to" and "from"
  toFrom <- str_split(oyster$journey.action, " to")

# get the "from" station
  from <- sapply(1:length(toFrom), function(x) toFrom[[x]][1]) %>%
          gsub("\\[.*\\]|\\(.*\\)","", .) %>% str_trim()

# get the "to" station
  to <- sapply(1:length(toFrom), function(x) toFrom[[x]][2]) %>%
          gsub("\\[.*\\]|\\(.*\\)","", .) %>% str_trim()

# put back in to data
  oyster %<>%
    mutate(from = from,
           to = to
           )

# clear out the junk
  rm(list = c("afterMidnight", "badRecords", "from", "to", "toFrom"))
  gc()

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


