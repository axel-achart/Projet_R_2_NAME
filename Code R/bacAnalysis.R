library(shiny)
library(dplyr)
library(ggplot2)

# Lecture des données
df_working <- read.csv("../data/raw/baccalaureate_by_academy_france.csv")
print(names(df_working))  # Afficher les noms des colonnes pour vérification
# Interface utilisateur
ui <- fluidPage(
  titlePanel("Dashboard Bac par Académie (France)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Année :", choices = unique(df_working$Session)),
      selectInput("academy", "Académie :", choices = unique(df_working$Academy)),
      selectInput("specialty", "Spécialité :", choices = unique(df_working$Diploma_Specialty))
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

# Serveur
server <- function(input, output, session) {
  
  data_filtered <- reactive({
    df_working %>%
      filter(
        Session == input$year,
        Academy == input$academy,
        Diploma_Specialty == input$specialty
      )
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
      Mention = c("Très Bien + Félicitations", "Très Bien", "Bien", "Assez Bien", "Passable"),
      Score = c(18, 16, 14, 12, 10),
      Nombre = c(
        sum(data$Number_Admitted_With_Highest_Honors_With_Jury_Congratulations),
        sum(data$Number_Admitted_With_Highest_Honors_Without_Jury_Congratulations),
        sum(data$Number_Admitted_With_High_Honors),
        sum(data$Number_Admitted_With_Honors),
        sum(data$Number_Admitted_Without_Honors)
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

