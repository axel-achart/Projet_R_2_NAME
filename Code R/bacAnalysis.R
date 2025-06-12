# Code R for French Bac Analysis

# --> Set Working Directory
read.csv("baccalaureate_by_academy_france.csv")
data <- read.csv("baccalaureate_by_academy_france.csv") # Set variable  of dataset

# Vérifier si valeurs manquantes
sum(is.na(baccalaureate_by_academy_france))

# UI by Shiny
library(shiny)
library(dplyr)
library(ggplot2)

# Lecture des données
df_working <- read.csv("baccalaureate_by_academy_france.csv")

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
    
    # Simulation d'un "score" : poids fictif par niveau de mention
    data_long <- data %>%
      mutate(
        Mention = case_when(
          Number_Admitted_With_Highest_Honors_With_Jury_Congratulations > 0 ~ "Très Bien + Félicitations",
          Number_Admitted_With_Highest_Honors_Without_Jury_Congratulations > 0 ~ "Très Bien",
          Number_Admitted_With_High_Honors > 0 ~ "Bien",
          Number_Admitted_With_Honors > 0 ~ "Assez Bien",
          Number_Admitted_Without_Honors > 0 ~ "Passable",
          TRUE ~ "Non admis"
        ),
        score = case_when(
          Mention == "Très Bien + Félicitations" ~ 18,
          Mention == "Très Bien" ~ 16,
          Mention == "Bien" ~ 14,
          Mention == "Assez Bien" ~ 12,
          Mention == "Passable" ~ 10,
          TRUE ~ 8
        )
      )
    
    ggplot(data_long, aes(x = score, fill = Mention)) +
      geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
      labs(title = "Répartition simulée des mentions", x = "Score estimé", y = "Nombre de candidats") +
      theme_minimal()
  })
}

# Lancement
shinyApp(ui = ui, server = server)