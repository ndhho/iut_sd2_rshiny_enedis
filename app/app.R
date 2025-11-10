#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("shiny")
#install.packages("shinydashboard")
library(shinydashboard)
library(shiny)

#file_path_59 = "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/refs/heads/main/data/clean_csv/logements_59.csv" 
#file_path_69 = "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/refs/heads/main/data/clean_csv/logements_69.csv" 
#logements_59 = read.csv(file_path_59, sep = ';', dec = ',')
#logements_69 = read.csv(file_path_69, sep = ';', dec = ',')


# Define UI for application that draws a histogram

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview",
            h2("Overview")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  ),
  fluidRow(
    column(width = 6,
           
      # A static valueBox
      valueBox(5 * 2, "5*2 = ValueIN", icon = icon("credit-card")),
      valueBox(1 * 2, "1x2", icon = icon("credit-card")),
      valueBox(10 * 2, "width = 4", icon = icon("credit-card"), width = 4),
      valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
      valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
      
    ),
    # Dynamic valueBoxes
    valueBoxOutput("progressBox"),
    
    valueBoxOutput("approvalBox")
  )

)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  sidebar,
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
