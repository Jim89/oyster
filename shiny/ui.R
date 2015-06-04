library(shiny)
library(leaflet)
library(DT)

# Set up the application
shinyUI(fluidPage(
  # Create the title
    titlePanel("TfL Oyster Card Data Explorations"),

  # set up the sidebar
    #sidebarLayout(position = "left",
      sidebarPanel(
        h4("Weekends or weekdays?"),
        helpText("Chose whether you would like to see a histogram of journey time for weekdays, weekends, or both."),
        radioButtons("day", NULL,
                     c("Weekday" = "Weekday",
                       "Weekend" = "Weekend",
                       "Both" = "Both")),
        br(),
        h4("Morning or evening commute?"),
        helpText("Find the optimum commute time - chose whether you would like to see a plot of commute duration vs. touch in time for mornings or evenings (smoother optional)"),
        radioButtons("commute", NULL,
                     c("Morning" = "Morning",
                       "Evening" = "Evening")),
        checkboxInput('smooth', 'Smooth', FALSE)
      ),

  # add the main panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Journey Time Histogram", plotOutput("hist")),
                  tabPanel("Commute time", plotOutput("commute")),
                  tabPanel("The Data", dataTableOutput("data"))
                  )
    )
    #)
))


