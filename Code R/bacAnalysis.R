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

    # Construction du tableau empilé
    mentions_stacked <- data %>%
      group_by(Path) %>%
      summarise(
        `Très Bien + Félicitations` = sum(Number_Admitted_With_Highest_Honors_With_Jury_Congratulations),
        `Très Bien` = sum(Number_Admitted_With_Highest_Honors_Without_Jury_Congratulations),
        Bien = sum(Number_Admitted_With_High_Honors),
        `Assez Bien` = sum(Number_Admitted_With_Honors),
        Passable = sum(Number_Admitted_Without_Honors),
        Refusé = sum(Total_Number_Rejected)
      ) %>%
      tidyr::pivot_longer(
        cols = -Path,
        names_to = "Mention",
        values_to = "Nombre"
      )

    # Affichage en histogramme empilé
    ggplot(mentions_stacked, aes(x = Mention, y = Nombre, fill = Path)) +
      geom_col(position = "stack") +
      labs(title = "Répartition des Mentions par Type de Bac", x = "Mention", y = "Nombre de candidats") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  
}

# Lancement
shinyApp(ui = ui, server = server)

