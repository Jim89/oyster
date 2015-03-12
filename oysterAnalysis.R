########################### Set up environment #################################
# load required packages
  library(lubridate)
  library(chron)
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
# histogram of journey times split by weekend
journeyTimeHist <-
combined %>%
  mutate(weekend = ifelse(start.day %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>%
  ggplot(aes(x = journey.time %>% as.numeric)) +
  geom_histogram(binwidth = 5, aes(fill = weekend), alpha = 0.8, colour = "lightgrey") +
  facet_grid(weekend ~ ., scales = "fixed") +
  scale_x_continuous(breaks = seq(from = 0,
                                  to = max(combined$journey.time)%>% as.numeric + 5,
                                  by = 5)) +
  scale_fill_manual(values = rep(kpmgDarkBlue, 2),
                    guide = F) +
  xlab("Journey time / minutes") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "lightgrey", linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.margin.y = unit(0.1, units = "in"),
        panel.background = element_rect(fill = "white",colour = "lightgrey"))


combined %>%
  mutate(start.time.rounded = start.datetime.rounded %>% as.character %>%
           str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) %>%
  group_by(start.time.rounded) %>%
  summarise(journey.time = journey.time %>% as.numeric %>% mean) %>%
  mutate(start.time.rounded %>% strptime(format = "%T")) %>%
  ggplot(aes(x = start.time.rounded, y = journey.time, group = 1 )) +
  geom_line() +
  theme(axis.text.x = element_text(angle = -90))


