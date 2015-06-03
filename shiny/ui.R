library(shiny)


# Set up the application
shinyUI(fluidPage(
  # Create the title
    titlePanel("TfL Oyster Card Data Explorations"),

  # set up the sidebar
    sidebarLayout(
      sidebarPanel(
        radioButtons("day", "Weekends or Weekdays?\n (affects histogram only)",
                     c("Both" = "Both",
                       "Weekday" = "Weekday",
                       "Weekend" = "Weekend"))
      ),

  # add the main panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("map")),
                  tabPanel("Journey Time Histogram", plotOutput("hist")),
                  tabPanel("The Data", dataTableOutput("data"))
                  )
    )
    )
))