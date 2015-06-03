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
  library(shiny)
  library(DT)

# run the scripts that get, clean and combine the data together
  suppressWarnings(source("./gettingAndCleaning/01_oysterData.R"))
  suppressWarnings(source("./gettingAndCleaning/02_stationsData.R"))
  suppressWarnings(source("./gettingAndCleaning/03_combined.R"))

# set up some colours to use
  districtLine <- col2rgb("#007229")
  jubileeLine <- col2rgb("#868f98")
  kpmgDarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
  kpmgPurple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
  lightGrey <- rgb(red = 186, green = 187, blue = 188, maxColorValue = 255)

# create function for plotting the histogram
  journeyTimeHist.f <- function (data, days = c("Weekend", "Weekday")) {
  data %>%
  filter(weekend %in% days) %>%
  ggplot(aes(x = journey.time %>% as.numeric)) +
  geom_histogram(binwidth = 5, aes(fill = weekend), alpha = 0.8,
                 colour = "white") +
  facet_grid(weekend ~ ., scales = "fixed") +
  scale_x_continuous(breaks = seq(from = 0,
                                  to = data$journey.time %>%
                                       as.numeric() %>%
                                       max(na.rm = T) + 5,
                                  by = 5)) +
  scale_fill_manual(values = rep(kpmgDarkBlue, 2), guide = F) +
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
  }


# set up data for the map widget -----------------------------------------------
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

shinyServer( function(input, output) {
  # create the journey time histogram
  output$hist <- renderPlot({
                 if ( input$day == "Both" ) {
                   journeyTimeHist.f(combined)
                 } else if ( input$day == "Weekend") {
                   journeyTimeHist.f(combined, "Weekend")
                 } else if ( input$day == "Weekday") {
                   journeyTimeHist.f(combined, "Weekday")
                 }
                 })
  # create the leaftlet map
  output$map <- renderLeaflet({
                vistited %>%
                leaflet() %>%
                addProviderTiles("Stamen.TonerLite",
                                  options = providerTileOptions(noWrap = TRUE)) %>%
                setView(lng = -0.1275, lat = 51.5072, zoom = 13) %>%
                addCircles(radius = ~2.2*visits, popup = popup, stroke = T,
                           fillColor = kpmgDarkBlue,
                           fillOpacity = 0.75)  })
  # create the data to provide
  output$data <- renderDataTable({combined[ ,c(1:5, 8)] %>%
                                  mutate(Note = ifelse(charge == 0,
                                                       "No charge - travel within travel card zone(s)",
                                                       note)) %>%
                                  select(-note) %>%
                                  setNames(c("Date", "Touched in", "Touched Out",
                                             "Journey", "Charge (£)", "Note"))})

})