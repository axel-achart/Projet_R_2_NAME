#  Installer les packages nécessaires (à exécuter une seule fois)
install.packages("shiny")
install.packages("shinydashboard")

#  Charger les packages
library(shiny)
library(shinydashboard)

#  Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Mon Dashboard Shiny"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      sliderInput("n", "Nombre de points :", min = 10, max = 1000, value = 500)
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Histogramme dynamique", status = "primary", solidHeader = TRUE,
                    plotOutput("histPlot"), width = 12)
              )
      )
    )
  )
)

#  Logique serveur
server <- function(input, output) {
  output$histPlot <- renderPlot({
    hist(rnorm(input$n), col = "skyblue", border = "white",
         main = paste("Histogramme de", input$n, "valeurs"))
  })
}

#  Lancer l'application
shinyApp(ui = ui, server = server)