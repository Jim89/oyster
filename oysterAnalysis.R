########################### Set up environment #################################
# load required packages
  library(lubridate)
  library(dplyr)
  library(magrittr)
  library(stringr)
  library(rvest)
  library(ggplot2)
  library(RgoogleMaps)
  library(grid)
  library(scales)

# run the scripts that get, clean and combine the data together

  ptm <- proc.time()
  print("Getting and cleaning data")
  source("./gettingAndCleaning/01_oysterData.R")
  source("./gettingAndCleaning/02_stationsData.R")
  source("./gettingAndCleaning/03_combined.R")
  print("Complete, took:")
  proc.time() - ptm

# set up some colours to use
  districtLine <- col2rgb("#007229")
  jubileeLine <- col2rgb("#868f98")
  kpmgDarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)

########################### Create some plots ##################################
# histogram of journey times split by weekend ----------------------------------
journeyTimeHist <-
combined %>%
  ggplot(aes(x = journey.time %>% as.numeric)) +
  geom_histogram(binwidth = 5, aes(fill = weekend), alpha = 0.8, 
                 colour = "lightgrey") +
  facet_grid(weekend ~ ., scales = "fixed") +
  scale_x_continuous(breaks = seq(from = 0,
                                  to = max(combined$journey.time) %>% 
                                       as.numeric + 5,
                                  by = 5)) +
  scale_fill_manual(values = rep(kpmgDarkBlue, 2), guide = F) +
  xlab("Journey time / minutes") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        text = element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "lightgrey", 
                                          linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.margin.y = unit(0.1, units = "in"),
        panel.background = element_rect(fill = "white",colour = "lightgrey"))



# line chart of journey times for my commute -----------------------------------
# create function to plot the data (as I'll do it for both morning and evening)
CommutePlot <- function(data, start, end) {
  data %>%
    filter(weekend != "Weekend") %>%
    mutate(start.time.clean = start.time.clean %>% as.character %>%
             str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]") %>%
             strptime(format = "%T") %>%
             as.POSIXct %>%
             CeilingTime(2, "minute")) %>%
    filter(start.time.clean %>% between(start, end)) %>%
    group_by(start.time.clean) %>%
    summarise(journeys = n(),
              journey.time = journey.time %>% as.numeric %>% mean) %>% # View
    mutate(start.time.clean = start.time.clean %>% as.character %>% 
             str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) %>%
    ggplot(aes(x = start.time.clean, y = journey.time, group = 1)) +
    geom_line() +
    geom_point(aes(size = journeys), colour = kpmgDarkBlue, alpha = 0.8) +
    scale_size(name = "Number of\nJourneys",
               range = c(0, 10)) +
    xlab("Departure Time") +
    ylab("Journey Time / minutes") +
    #  geom_smooth(method = "lm", alpha = 0.075) +
    geom_smooth(method = "loess", size = 0.5, 
                colour = kpmgDarkBlue, alpha = 0.075) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = -90),
          axis.text.y = element_text(size = 12),
          #axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = "lightgrey", 
                                            linetype = "dotted"),
          panel.grid.major.y = element_line(colour = "lightgrey", 
                                            linetype = "dotted"),
          panel.grid.minor.y = element_blank(),
          panel.margin.y = unit(0.1, units = "in"),
          panel.background = element_rect(fill = "white",colour = "lightgrey"),
          legend.background = element_rect(fill = "white")) 
}

# set up start and end time windows
  startMorning <- "06:30:00" %>% strptime(format = "%T") %>% as.POSIXct
  endMorning <- "08:00:00" %>% strptime(format = "%T") %>% as.POSIXct

  startEvening <- "17:15:00" %>% strptime(format = "%T") %>% as.POSIXct
  endEvening <- "19:00:00" %>% strptime(format = "%T") %>% as.POSIXct

# create the plots
  morningCommute <- CommutePlot(combined, startMorning, endMorning)
  eveningCommute <- CommutePlot(combined, startEvening, endEvening)