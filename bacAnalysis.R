# bacAnalyse.R

# 📦 Charger les packages nécessaires
library(shiny)
library(tidyverse)

# 📂 Charger le fichier RData contenant df_working
load("df_working.RData")

# 🔧 Créer la colonne du taux de réussite si elle n'existe pas déjà
df_working <- df_working %>%
  mutate(Success_Rate = (Total_Number_Admitted / Number_of_Attendees) * 100)

# 🖼️ Interface utilisateur
ui <- fluidPage(
  titlePanel("Dashboard : Taux de réussite au baccalauréat par académie"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("session", "Sélectionnez une session :", choices = sort(unique(df_working$Session))),
      selectInput("academy", "Sélectionnez une académie :", choices = sort(unique(df_working$Academy)))
    ),
    
    mainPanel(
      plotOutput("barplot_rate"),
      tableOutput("summary_table")
    )
  )
)

# ⚙️ Partie serveur
server <- function(input, output, session) {
  
  # Données filtrées en fonction des entrées utilisateur
  filtered_data <- reactive({
    df_working %>%
      filter(Session == input$session, Academy == input$academy)
  })
  
  # 📊 Barplot des taux de réussite
  output$barplot_rate <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = reorder(Diploma_Specialty, Success_Rate), y = Success_Rate, fill = Diploma_Specialty)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Taux de réussite par spécialité -", input$academy, "-", input$session),
        x = "Spécialité",
        y = "Taux de réussite (%)"
      ) +
      theme_minimal()
  })
  
  # 📋 Tableau récapitulatif
  output$summary_table <- renderTable({
    filtered_data() %>%
      group_by(Diploma_Specialty) %>%
      summarise(
        Effectif = sum(Number_of_Attendees, na.rm = TRUE),
        Admis = sum(Total_Number_Admitted, na.rm = TRUE),
        Taux = round(mean(Success_Rate, na.rm = TRUE), 1)
      )
  })
}

# 🚀 Lancer l’application
shinyApp(ui = ui, server = server)
