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
           includeMarkdown("./md/intro.md"),
           hr()),

# visualisation of visits mapped on to interactive map
  tabPanel("Map",
          # set up styling for map
          div(class="outer",
          tags$head(
            includeCSS("./css/styles.css"))),
          # plot the map
          leafletOutput("map", height = 750),
          # add the overlay panel
          absolutePanel(id = "description",
                        class = "panel panel-default",
                        fixed = T,
                        draggable = T,
                        top = 90,
                        left = "auto",
                        right = 20,
                        bottom = "auto",
                        width = "25%",
                        height = "auto",
          # set content of panel
          h1("Station Visits Explorer"),
          p("This map shows all stations visited on a map of London.",
            span(strong("Station points are sized by the number of visits."))),
          h4("Instructions:"),
          tags$ul(
            tags$li("Zoom in/out and navigate around the map with your mouse."),
            tags$li("Select any marker to see the station name and number of visits"),
            tags$li(p("Try to find the station where the creator of this app lives - send answers via",
                    span(tags$a(href="https://twitter.com/leach_jim", "twitter."))))))
  ),

# journey time histogram(s)
  tabPanel("Journey times",
           fluidRow(column(12,
                           h1("Journey Times"),
                           p("Many people spend a lot of time on public transport, especially in a big city like London. But how long do journeys really take? This histogram shows the distribution of journey times for weekends, weekdays, or both - helping to answer that question."),
                           br(),
                           h4("Instructions"),
                           p("Use the radio buttons on the left to chose weekends, weekdays, or a faceted plot of both."))),
           hr(),
           fluidRow(sidebarPanel(width = 3,
                    h4("Weekends or weekdays?"),
                    helpText("Chose whether you would like to see a histogram of journey time for weekdays, weekends, or both."),
                    radioButtons("day", NULL,
                                  c("Weekday" = "Weekday",
                                    "Weekend" = "Weekend",
                                    "Both" = "Both"))),
                    mainPanel(plotOutput("hist", height = 500)))
           ),

# commute journey duration vs. touch in time
  tabPanel("Commute time",
           fluidRow(column(8,
                           h1("Commute Duration"),
                           p("A lot us spend most of our time on public transport during our commute.
                             Sometimes our commute seems longer than usual. This tab helps invesitage the relationship between the time the journey is started and how long it takes. This has been done for:"),
                           tags$ul(
                             tags$li("Morning commute - 6:30 am until 8:00 am"),
                             tags$li("Evening commute - 5:00 pm until 7:00 pm")
                           ),
                           p("This plot shows how long the average journey is against the time the journey was started - helping to find the optimum time to set off."),
                           h4("Instructions"),
                           p("Use the radio buttons on the right to select the morning or evening commute. Add an optional smoother with the checkbox.")),
           column(4,
           h4("Morning or evening commute?"),
           helpText("Find the optimum commute time - chose whether you would like to see a plot of commute duration vs. touch in time for mornings or evenings (smoother optional)"),
           radioButtons("commute", NULL,
                          c("Morning" = "Morning",
                            "Evening" = "Evening")),
           checkboxInput('smooth', 'Smooth', FALSE))),

          fluidRow(plotOutput("commute", height = 500))),

# simple data table output
  tabPanel("The data",
           column(2,
                  h1("The raw data"),
                  p("This tabs shows the raw data as received from TfL.
                    Filter, sort and search. If you'd like to know more, get in
                    touch with the app creator via",
                    span(tags$a(href="https://twitter.com/leach_jim", "twitter.")))),
           column(10, dataTableOutput("data", height = "100%"))),

# circos plot
  tabPanel("Circos Plot",
           sidebarPanel(width = 3,
                    h4("Most Popular Journeys"),
                    helpText("Choose how many of the most popular journeys you'd like to see the diagram for"),
                    radioButtons("journeys", NULL,
                                  c("5" = "5",
                                    "10" = "10",
                                    "15" = "15",
                                    "20" = "20",
                                    "25" = "25",
                                    "30" = "30"))),
           mainPanel(plotOutput("circos", height = 750, width = 750)))

# close the UI definition
))


