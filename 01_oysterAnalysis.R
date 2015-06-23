########################### Set up environment #################################
# load required packages
library(lubridate)
library(dplyr)
library(magrittr)
library(stringr)
library(rvest)
library(ggplot2)
library(grid)
library(scales)
library(leaflet)

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
DarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
Purple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
lightGrey <- rgb(red = 186, green = 187, blue = 188, maxColorValue = 255)

########################### Create some plots ##################################
# histogram of journey times split by weekend ----------------------------------
journeyTimeHist <-
combined %>%
ggplot(aes(x = journey.time %>% as.numeric)) +
geom_histogram(binwidth = 5, aes(fill = weekend), alpha = 0.8,
               colour = "white") +
facet_grid(weekend ~ ., scales = "fixed") +
scale_x_continuous(breaks = seq(from = 0,
                                to = combined$journey.time %>%
                                     as.numeric() %>%
                                     max(na.rm = T) + 5,
                                by = 5)) +
scale_fill_manual(values = rep(DarkBlue, 2), guide = F) +
xlab("Journey time / minutes") +
theme(axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      #axis.text.y = element_blank(),
      text = element_text(size = 14),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
#         element_line(colour = "lightgrey",
#                                         linetype = "dotted"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.margin.y = unit(0.1, units = "in"),
      panel.background = element_rect(fill = "white", colour = "lightgrey"))



# line chart of journey times for my commute -----------------------------------
# create function to plot the data (as I'll do it for both morning and evening)
CommutePlot <- function (data, start, end, interval) {
# take the data and perform some manipulations
data %>%
  filter(weekend != "Weekend") %>%
  mutate(start.time.clean = start.time.clean %>% as.character %>%
           str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]") %>%
           strptime(format = "%T") %>%
           as.POSIXct %>%
           CeilingTime(interval, "minute")) %>%
  filter(start.time.clean %>% between(start, end)) %>%
  group_by(start.time.clean) %>%
  summarise(journeys = n(),
            journey.time = journey.time %>% as.numeric %>% mean) %>% # View
  mutate(start.time.clean = start.time.clean %>% as.character %>%
           str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) %>%
# make the plot itself
  ggplot(aes(x = start.time.clean, y = journey.time, group = 1)) +
#  geom_line(colour = DarkBlue) +
  geom_point(aes(size = journeys), colour = DarkBlue, alpha = 0.8) +
  scale_size(name = "Number of\nJourneys", range = c(0, 10)) +
  xlab("Departure Time") +
  ylab("Average Journey Time / minutes") +
  #  geom_smooth(method = "lm", alpha = 0.075) +
  geom_smooth(method = "loess", size = 0.5, colour = Purple, alpha = 0.25) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -90),
        axis.text.y = element_text(size = 12),
        #axis.title.y = element_blank(),
        #axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
#           element_line(colour = "lightgrey",s
#                                           linetype = "dotted"),
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
  morningCommute <- CommutePlot(combined, startMorning, endMorning, 2)
  eveningCommute <- CommutePlot(combined, startEvening, endEvening, 2)

# journeys over the day --------------------------------------------------------
  start <- "05:00:00" %>% strptime(format = "%T") %>% as.POSIXct
  end <- "22:30:00" %>% strptime(format = "%T") %>% as.POSIXct

dailyActivity <-
combined %>%
# filter(weekend != "Weekend") %>%
 mutate(start.time.clean = start.time.clean %>% as.character %>%
          str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]") %>%
          strptime(format = "%T") %>%
          as.POSIXct %>%
          CeilingTime(30, "minute")) %>% # View
  filter(start.time.clean %>% between(start, end)) %>%
  group_by(start.time.clean) %>%
  summarise(journeys = n()) %>% # View
  mutate(start.time.clean = start.time.clean %>% as.character %>%
           str_extract("[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) %>%
  # make the plot itself
  ggplot(aes(x = start.time.clean, y = journeys, group = 1)) +
  geom_line(colour = DarkBlue) +
#  geom_point(aes(size = journeys), alpha = 0.8) +
#  scale_size(name = "Number of\nJourneys", range = c(0, 10)) +
  xlab("Departure Time") +
  ylab("Journeys") +
#  geom_smooth(method = "lm", alpha = 0.075) +
#  geom_smooth(method = "loess", size = 0.5, colour = Purple, alpha = 0.25) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -90),
        axis.text.y = element_text(size = 12),
        #axis.title.y = element_blank(),
        #axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
#           element_line(colour = "lightgrey",
#                                           linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "lightgrey",
                                          linetype = "dotted"),
        panel.grid.minor.y = element_blank(),
        panel.margin.y = unit(0.1, units = "in"),
        panel.background = element_rect(fill = "white",colour = "lightgrey"),
        legend.background = element_rect(fill = "white"))

# save some plots ----- --------------------------------------------------------
if (!dir.exists("./plots")) {
  dir.create("./plots")
}

# journey time histogram
  ggsave("./plots/journeyTimeHist.png", journeyTimeHist, width = 6, height = 4, units = "in")

# journeys over the day
  ggsave("./plots/dailyActivity.png", dailyActivity, width = 6, height = 4, units = "in")

# commute time
  ggsave("./plots/morningCommut.png", morningCommute, width = 6, height = 4, units = "in")
  ggsave("./plots/eveningCommute.png", eveningCommute, width = 6, height = 4, units = "in")


# set up a map widget ----------------------------------------------------------
# reshape the data
  vistited <- combined %>%
              select(from, from.long, from.lat) %>%
              setNames(c("station", "long", "lat")) %>%
              rbind(combined %>%
                      select(to, to.long, to.lat) %>%
                      setNames(c("station", "long", "lat"))) %>%
              group_by(station, long, lat) %>%
              summarise(visits = n()) %>%
              filter(!is.na((long)))

# create pop-up text
  popup <- paste0("<strong> Station: </strong>",
                  vistited$station,
                  "<br><strong>Visits: </strong>",
                  vistited$visits)

# create the widget
  vistited %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng = -0.1275, lat = 51.5072, zoom = 11) %>%
  addCircles(radius = ~3*visits, popup = popup, stroke = T,
             fillColor = DarkBlue,
             fillOpacity = 0.75)