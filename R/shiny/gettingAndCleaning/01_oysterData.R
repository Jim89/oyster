########################## Get the raw data to be used #########################
# Raw Oyster Data (provided by TFL) --------------------------------------------
files <-
  list.files("./data",recursive = T) %>% as.list %>% paste0("./data/", .)

# remove the stations data (otherwise it'll throw an error) --------------------
files <- files[-grep("stations", files)]

# read in the data -------------------------------------------------------------
oyster <-
  lapply(files, function(x) read.csv(x, stringsAsFactors = F, skip = 1)) %>% rbind_all


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
# set names to lowercase because I'm lazy
  names(oyster) %<>% tolower

# strip out guff and clean times -----------------------------------------------
# find records where something went awry in the journey
  badRecords <- "touch-in|Topped-up|touch-out|Season ticket|Bus journey|Topped up|Entered and exited|Unspecified location"

# create clearner times, dates and datetimes
  oyster %<>%
  .[-grep(badRecords, .$journey.action),] %>%
  mutate(start.time.clean = start.time %>% paste0(":00"),
         end.time.clean = end.time %>% paste0(":00"),
         date.clean = dmy(date),
         start.datetime = paste(date, start.time.clean, sep = " ") %>% dmy_hms(),
         end.datetime = paste(date, end.time.clean, sep = " ") %>% dmy_hms()
         )

# find records where I touched out after mighnight
  afterMidnight <- substring(oyster$end.time,1,2) %in% c("00","01")

# set the end datetimes to be the next day (i.e. after midnight) where needed
  oyster[afterMidnight, 13]  <-  oyster[afterMidnight, 13] + days(1)


# create journey times and days of the week
  oyster %<>%
  mutate(start.datetime.rounded = CeilingTime(start.datetime, 15, "minute"),
         journey.time = difftime(end.datetime, start.datetime, units = "mins"),
         start.day = wday(start.datetime, label = T),
         weekend = ifelse(start.day %in% c("Sat", "Sun"), "Weekend", "Weekday")
         )
  
  
# get journey station information ----------------------------------------------
# split up the journey in to "to" and "from"
  toFrom <- str_split(oyster$journey.action, " to")

# get the "from" station
  from <- sapply(1:length(toFrom), function(x) toFrom[[x]][1]) %>%
          gsub("\\[.*\\]|\\(.*\\)| [Dd][Ll][Rr]","", .) %>% str_trim()

# get the "to" station
  to <- sapply(1:length(toFrom), function(x) toFrom[[x]][2]) %>%
          gsub("\\[.*\\]|\\(.*\\)| [Dd][Ll][Rr]","", .) %>% str_trim()

# put back in to data
  oyster %<>%
    mutate(from = from,
           to = to
           )

# clear out the junk
  rm(list = c("afterMidnight", "badRecords", "from", "to", "toFrom", "files"))
  gc()
