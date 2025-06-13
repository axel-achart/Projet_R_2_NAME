library(shiny)
library(dplyr)
library(ggplot2)

# Lecture des données
df_working <- read.csv("../data/raw/baccalaureate_by_academy_france.csv",sep = ";", stringsAsFactors = FALSE)
print(names(df_working))  # Afficher les noms des colonnes pour vérification
# Interface utilisateur
ui <- fluidPage(
  tags$div(
    tags$h1("Dashboard Bac par Académie (France)", style = "text-align: center;")
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Année :", choices = c("Toutes", unique(df_working$Session)), multiple = TRUE),
      selectInput("academy", "Académie :", choices = c("Toutes", unique(df_working$Academy)), multiple = TRUE),
      selectInput("specialty", "Spécialité :", choices = c("Toutes", unique(df_working$Diploma_Specialty)), multiple = TRUE)
      ),
    
    mainPanel(
      h3("Pourcentage de Réussite"),
      textOutput("pourcentage_reussite"),

      hr(),
      
      h3("Répartition des Filières"),
      plotOutput("camembert_filiere"),
      
      hr(),

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
  
  
  # In your server function, inside renderPlot for camembert_filiere:
output$camembert_filiere <- renderPlot({
  data <- data_filtered()
  if (nrow(data) == 0) return(NULL)
  
  # Group by Path and calculate admitted and attendees
  df_success <- data %>%
    group_by(Path) %>%
    summarise(
      Admitted = sum(Total_Number_Admitted, na.rm = TRUE),
      Attendees = sum(Number_of_Attendees, na.rm = TRUE)
    ) %>%
    mutate(
      SuccessRate = ifelse(Attendees > 0, Admitted / Attendees * 100, 0)
    )
  
  # For pie chart, you need the proportion of each Path's success rate
  df_success <- df_success %>%
    mutate(
      label = paste0(Path, "\n", round(SuccessRate, 1), "%")
    )
  
  # Plot as a pie chart (camembert)
  ggplot(df_success, aes(x = "", y = SuccessRate, fill = Path)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), size = 3) +
    labs(title = "Taux de réussite par filière (Path)")
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
    geom_col(position = "stack", color = "white", width = 0.9) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Répartition des Mentions par Type de Bac", x = "Mention", y = "Nombre de candidats", fill = "Filière") +
    theme_minimal(base_family = "Arial", base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 13),
      axis.title.y = element_text(face = "bold", size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11)
    )
  })
}

# Lancement
shinyApp(ui = ui, server = server)

