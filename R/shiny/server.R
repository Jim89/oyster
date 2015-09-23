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

# run the scripts that get, clean and combine the data together
  suppressWarnings(source("./gettingAndCleaning/01_oysterData.R"))
  suppressWarnings(source("./gettingAndCleaning/02_stationsData.R"))
  suppressWarnings(source("./gettingAndCleaning/03_combined.R"))

# set up some colours to use
  districtLine <- col2rgb("#007229")
  jubileeLine <- col2rgb("#868f98")
  DarkBlue <- rgb(red = 0, green = 51, blue = 141, maxColorValue = 255)
  Purple <- rgb(red = 142, green = 37, blue = 141, maxColorValue = 255)
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
  }

# create function for plotting commute
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
#   geom_smooth(method = "loess", size = 0.5, colour = Purple, alpha = 0.25) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90),
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


# set up data for the map widget -----------------------------------------------
# reshape the data
  vistited <- combined %>%
              select(from, from.long, from.lat) %>%
              setNames(c("station", "longitude", "latitude")) %>%
              rbind(combined %>%
                      select(to, to.long, to.lat) %>%
                      setNames(c("station", "longitude", "latitude"))) %>%
              group_by(station, longitude, latitude) %>%
              summarise(visits = n()) %>%
              filter(!is.na((longitude)))

# create pop-up text
  popup <- paste0("<strong> Station: </strong>",
                  vistited$station,
                  "<br><strong>Visits: </strong>",
                  vistited$visits)


# set up data for the circos plot ----------------------------------------------


# set up colours
  bakerloo <- rgb(red = 137, green = 78, blue = 36, maxColorValue = 255)
  central <- rgb(red = 220, green = 36, blue = 31, maxColorValue = 255)
  circle <- rgb(red = 255, green = 206, blue = 0, maxColorValue = 255)
  district <- rgb(red = 0, green = 114, blue = 41, maxColorValue = 255)
  hc <- rgb(red = 215, green = 153, blue = 175, maxColorValue = 255)
  jubilee <- rgb(red = 134, green = 143, blue = 152, maxColorValue = 255)
  metropolitan <- rgb(red = 117, green = 16, blue = 86, maxColorValue = 255)
  northern <- rgb(red = 0, green = 0, blue = 0, maxColorValue = 255)
  picadilly <- rgb(red = 0, green = 25, blue = 168, maxColorValue = 255)
  victoria <- rgb(red = 0, green = 160, blue = 226, maxColorValue = 255)
  wc <- rgb(red = 118, green = 208, blue = 189, maxColorValue = 255)
  dlr <- rgb(red = 0, green = 175, blue = 173, maxColorValue = 255)
  overground <- rgb(red = 232, green = 106, blue = 16, maxColorValue = 255)

createChordPlot <- function(oyster_data, top_journeys){
  journeys <- oyster_data %>%
              group_by(from, to) %>%
              summarise(journeys = n()) %>%
              ungroup() %>%
              arrange(desc(journeys)) %>%
              mutate(rank = row_number(-journeys))

  dat <- oyster_data %>%
         inner_join(journeys %>% filter(rank <= top_journeys),
                    by = c("from" = "from",
                           "to" = "to"))

  mat <- table(dat$from, dat$to) %>%
         as.data.frame.matrix() %>%
         as.matrix()
  colnames(mat) %<>% str_wrap(width = 10)
  rownames(mat) %<>% str_wrap(width = 10)


  grid.col = NULL # just create the variable
  grid.col[rownames(mat)] = "steelblue"
  grid.col[colnames(mat)] = "steelblue"


# clear the plot
  circos.clear()


  # set the basic basic circos graphic parameters
  circos.par(cell.padding=c(0,0,0,0),
             track.margin=c(0,0),
             start.degree = 0,
             gap.degree = 2.5,
             points.overflow.warning=FALSE)

# make the basic plot
  chordDiagram(mat,
               annotationTrack = NA,
               preAllocateTracks = list(track.height = 0.1),
               grid.col = picadilly,
               link.border = "lightgrey")

# add the labels
  circos.trackPlotRegion(track.index = 1,
                         panel.fun = function(x, y) {
                          xlim = get.cell.meta.data("xlim")
                          xplot = get.cell.meta.data("xplot")
                          ylim = get.cell.meta.data("ylim")
                          sector.name = get.cell.meta.data("sector.index")
                          if ( abs(xplot[2] - xplot[1]) < 20) {
                               circos.text(mean(xlim),
                                           ylim[1],
                                           sector.name,
                                           facing = "clockwise",
                                           niceFacing = TRUE,
                                           adj = c(0, 0.5)
                                          )
                          } else {
                          circos.text(mean(xlim),
                                      ylim[1],
                                      sector.name,
                                      facing = "inside",
                                      niceFacing = TRUE,
                                      adj = c(0.5, 0)
                                      )
                            }
                          },
                         bg.border = central,
                         bg.col = central,
                         track.height = 5)

}




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
  # create the data to provide
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
    output$circos <- renderPlot({reactive(createChordPlot(combined, input$journeys))})

})