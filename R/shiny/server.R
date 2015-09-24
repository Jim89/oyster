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
  library(circlize)

# set up some colours to use
  DarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
  Purple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
  lightGrey <- rgb(red = 186, green = 187, blue = 188, maxColorValue = 255)


# run the scripts that get, clean and combine the data together
  suppressWarnings(source("./gettingAndCleaning/01_oysterData.R"))
  suppressWarnings(source("./gettingAndCleaning/02_stationsData.R"))
  suppressWarnings(source("./gettingAndCleaning/03_combined.R"))
  suppressWarnings(source("./gettingAndCleaning/04_visited.R"))

# pull in helper functions
  source("./functions/journeyTimeHist.R")
  source("./functions/commutePlot.R")
  source("./functions/createChordDiagram.R")


# establish shiny server -------------------------------------------------------
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
                           fillColor = DarkBlue,
                           fillOpacity = 0.75)  })
  
  # create the data table to provide
  output$data <- renderDataTable({combined[ ,c(1:5, 8)] %>%
                                  mutate(Note = ifelse(charge == 0,
                                                       "No charge - travel within travel card zone(s)",
                                                       note)) %>%
                                  select(-note) %>%
                                  setNames(c("Date", "Touched in", "Touched Out",
                                             "Journey", "Charge", "Note"))})

  # create the commute plot
    output$commute <- renderPlot({
      if ( input$commute == "Morning"){
        # set up start and end time windows
          start <- "06:30:00" %>% strptime(format = "%T") %>% as.POSIXct
          end <- "08:00:00" %>% strptime(format = "%T") %>% as.POSIXct
          if (input$smooth == TRUE) {
            CommutePlot(combined, start, end, 2) +
            geom_smooth(method = "loess",
                        size = 0.5,
                        colour = Purple,
                        alpha = 0.25)
          } else {
            CommutePlot(combined, start, end, 2)
          }
      } else if ( input$commute == "Evening") {
        # set up start and end windows
          start <- "17:00:00" %>% strptime(format = "%T") %>% as.POSIXct
          end <- "19:00:00" %>% strptime(format = "%T") %>% as.POSIXct
          if (input$smooth == TRUE) {
            CommutePlot(combined, start, end, 2) +
            geom_smooth(method = "loess",
                        size = 0.5,
                        colour = Purple,
                        alpha = 0.25)
          } else {
            CommutePlot(combined, start, end, 2)
          }
      }
   })

  # create the circos plot
    output$circos <- renderPlot( { createChordPlot(combined, as.numeric(input$journeys)) } ) 
})