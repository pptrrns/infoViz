install.packages("shinydashboard")
library(shiny)


ui <- dashboardPage(dashboardHeader(title = "Basic dashboard"),
                    dashboardSidebar(),
                    dashboardBody())
server <- function(input, output) { }


shinyApp(ui, server)