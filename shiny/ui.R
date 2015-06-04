# TfL Data Exploration Shiny Application ---------------------------------------


# set up -----------------------------------------------------------------------
# load packages that will be used for the application
  library(shiny)
  library(leaflet)
  library(DT)
  library(markdown)

# Set up the application ui
  shinyUI(navbarPage("Oyster Explorer",

# define the tabs to be used in the app ----------------------------------------
# introduction splash
  tabPanel("Intro",
           includeMarkdown("./md/intro.md")),

# visualisation of visits mapped on to interactive map
  tabPanel("Map", hr(), leafletOutput("map"), hr()),

# journey time histogram(s)
  tabPanel("Journey Time Histogram",
           sidebarPanel(
           h4("Weekends or weekdays?"),
           helpText("Chose whether you would like to see a histogram of journey time for weekdays, weekends, or both."),
           radioButtons("day", NULL,
                      c("Weekday" = "Weekday",
                        "Weekend" = "Weekend",
                        "Both" = "Both"))),
          mainPanel(plotOutput("hist"))),

# commute journey duration vs. touch in time
  tabPanel("Commute time",
           sidebarPanel(
           h4("Morning or evening commute?"),
           helpText("Find the optimum commute time - chose whether you would like to see a plot of commute duration vs. touch in time for mornings or evenings (smoother optional)"),
           radioButtons("commute", NULL,
                      c("Morning" = "Morning",
                        "Evening" = "Evening")),
          checkboxInput('smooth', 'Smooth', FALSE)
           ),
           mainPanel(plotOutput("commute"))),

# simple data table output
  tabPanel("The Data", dataTableOutput("data"))

# close the UI definition
))


