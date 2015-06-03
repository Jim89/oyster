library(shiny)


# Set up the application
shinyUI(pageWithSidebar(
  # Create the title
    headerPanel("Oyster Data Explorations"),

    # Add a sidebar to switch between morning and evening commutes
      sidebarPanel(
        checkboxGroupInput("box", "Select a group:",
                         c("Weekday" = "Weekday",
                           "Weekend" = "Weekend"
                           ))),
  # add the main panel
    mainPanel(verbatimTextOutput("oid"),
              plotOutput("hist"))

))