#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Specify the application port
#options(shiny.host = "0.0.0.0")
#options(shiny.port = 3838)


source("global.R")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Box 1", status = "primary", solidHeader = TRUE, width = 6,
                    "Content of Box 1"),
                box(title = "Box 2", status = "warning", solidHeader = TRUE, width = 6,
                    "Content of Box 2")
              ),
              fluidRow(
                box(title = "Plot", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("plot"))
              )
      ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(rnorm(500), main = "Histogram of Random Normal Values")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

