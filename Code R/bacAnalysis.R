library(shiny)
library(dplyr)
library(ggplot2)

# Lecture des données
df_working <- read.csv("../data/raw/baccalaureate_by_academy_france.csv",sep = ";", stringsAsFactors = FALSE)
print(names(df_working))  # Afficher les noms des colonnes pour vérification
# Interface utilisateur
ui <- fluidPage(
  titlePanel("Dashboard Bac par Académie (France)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Année :", choices = c("Toutes", unique(df_working$Session)), multiple = TRUE),
      selectInput("academy", "Académie :", choices = c("Toutes", unique(df_working$Academy)), multiple = TRUE),
      selectInput("specialty", "Spécialité :", choices = c("Toutes", unique(df_working$Diploma_Specialty)), multiple = TRUE)
      ),
    
    mainPanel(
      h3("Pourcentage de Réussite"),
      textOutput("pourcentage_reussite"),
      
      h3("Répartition des Filières"),
      plotOutput("camembert_filiere"),
      
      h3("Répartition des Mentions (Score simulé)"),
      plotOutput("courbe_mentions")
    )
  )
)
print(names(df_working))
# Serveur
server <- function(input, output, session) {
  
  data_filtered <- reactive({
    data <- df_working
    # Filtre année
    if (!("Toutes" %in% input$year)) {
      data <- data[data$Session %in% input$year, ]
    }
    # Filtre académie
    if (!("Toutes" %in% input$academy)) {
      data <- data[data$Academy %in% input$academy, ]
    }
    # Filtre spécialité
    if (!("Toutes" %in% input$specialty)) {
      data <- data[data$Diploma_Specialty %in% input$specialty, ]
    }
    data
  })
  observe({
    if ("Toutes" %in% input$year) {
      updateSelectInput(session, "year", selected = unique(df_working$Session))
    }
    if ("Toutes" %in% input$academy) {
      updateSelectInput(session, "academy", selected = unique(df_working$Academy))
    }
    if ("Toutes" %in% input$specialty) {
      updateSelectInput(session, "specialty", selected = unique(df_working$Diploma_Specialty))
    }
  })
  
  
  output$pourcentage_reussite <- renderText({
    data <- data_filtered()
    if (nrow(data) == 0) return("Pas de données")
    pourcentage <- sum(data$Total_Number_Admitted) / sum(data$Number_of_Attendees) * 100
    paste0(round(pourcentage, 2), "% de réussite")
  })
  
  output$camembert_filiere <- renderPlot({
    data <- data_filtered()
    if (nrow(data) == 0) return(NULL)
    df_working_pie <- data %>%
      group_by(Path) %>%
      summarise(nb = sum(Number_of_Attendees))
    
    ggplot(df_working_pie, aes(x = "", y = nb, fill = Path)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Répartition des Filières (Path)")
  })
  
  output$courbe_mentions <- renderPlot({
    data <- data_filtered()
    if (nrow(data) == 0) return(NULL)
    
    # Construction du tableau avec les mentions + scores
    mentions <- data.frame(
      Mention = c("Très Bien + Félicitations", "Très Bien", "Bien", "Assez Bien", "Passable","Refusé"),
      Score = c(18, 16, 14, 12, 10, 8),
      Nombre = c(
        sum(data$Number_Admitted_With_Highest_Honors_With_Jury_Congratulations),
        sum(data$Number_Admitted_With_Highest_Honors_Without_Jury_Congratulations),
        sum(data$Number_Admitted_With_High_Honors),
        sum(data$Number_Admitted_With_Honors),
        sum(data$Number_Admitted_Without_Honors),
        sum(data$Total_Number_Rejected)
      )
    )
    
    ggplot(mentions, aes(x = Score, y = Nombre, fill = Mention)) +
      geom_col(width = 0.9, alpha = 0.8) +
      labs(title = "Répartition simulée des mentions", x = "Score estimé", y = "Nombre de candidats") +
      theme_minimal()
  })
  
}

# Lancement
shinyApp(ui = ui, server = server)

