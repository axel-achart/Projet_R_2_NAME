library(shiny)
library(dplyr)
library(ggplot2)

# Lecture des données
df_working <- read.csv("../data/raw/baccalaureate_by_academy_france.csv", sep = ";", stringsAsFactors = FALSE)

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
      h3("Répartition des Mentions"),
      plotOutput("courbe_mentions", height = "500px"),
      
      hr(),
      h3("Comparaison des Académies (Taux de Réussite)"),
      plotOutput("comparaison_academies", height = "500px")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  data_filtered <- reactive({
    data <- df_working
    if (!("Toutes" %in% input$year)) {
      data <- data[data$Session %in% input$year, ]
    }
    if (!("Toutes" %in% input$academy)) {
      data <- data[data$Academy %in% input$academy, ]
    }
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
    pourcentage <- sum(data$Total_Number_Admitted, na.rm = TRUE) / sum(data$Number_of_Attendees, na.rm = TRUE) * 100
    paste0(round(pourcentage, 2), "% de réussite")
  })
  
  output$camembert_filiere <- renderPlot({
    data <- data_filtered()
    if (nrow(data) == 0) return(NULL)
    
    df_success <- data %>%
      group_by(Path) %>%
      summarise(
        Admitted = sum(Total_Number_Admitted, na.rm = TRUE),
        Attendees = sum(Number_of_Attendees, na.rm = TRUE)
      ) %>%
      mutate(
        SuccessRate = ifelse(Attendees > 0, Admitted / Attendees * 100, 0),
        label = paste0(Path, "\n", round(SuccessRate, 1), "%")
      )
    
    ggplot(df_success, aes(x = "", y = SuccessRate, fill = Path)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5), size = 3) +
      labs(title = "Taux de réussite par filière")
  })
  
  output$courbe_mentions <- renderPlot({
    data <- data_filtered()
    if (nrow(data) == 0) return(NULL)
    
    mentions_stacked <- data %>%
      group_by(Path) %>%
      summarise(
        '6 Très Bien + Félicitations' = sum(Number_Admitted_With_Highest_Honors_With_Jury_Congratulations, na.rm = TRUE),
        '5 Très Bien' = sum(Number_Admitted_With_Highest_Honors_Without_Jury_Congratulations, na.rm = TRUE),
        '4 Bien' = sum(Number_Admitted_With_High_Honors, na.rm = TRUE),
        '3 Assez Bien' = sum(Number_Admitted_With_Honors, na.rm = TRUE),
        '2 Passable' = sum(Number_Admitted_Without_Honors, na.rm = TRUE),
        '1 Refusé' = sum(Total_Number_Rejected, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(
        cols = -Path,
        names_to = "Mention",
        values_to = "Nombre"
      )
    
    ggplot(mentions_stacked, aes(x = Mention, y = Nombre, fill = Path)) +
      geom_col(position = "stack", color = "white", width = 0.9) +
      geom_text(
        aes(label = ifelse(Nombre >= 5, Nombre, "")),
        position = position_stack(vjust = 0.5),
        size = 3.5,
        color = "black"
      ) +
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
  
  
  
output$comparaison_academies <- renderPlot({
  data <- data_filtered()
  if (nrow(data) == 0) return(NULL)
  
  df_compare <- data %>%
    group_by(Academy) %>%
    summarise(
      Nombre_Presents = sum(Number_of_Attendees, na.rm = TRUE),
      Nombre_Admis = sum(Total_Number_Admitted, na.rm = TRUE)
    ) %>%
    mutate(Taux_Reussite = 100 * Nombre_Admis / Nombre_Presents)
  
  ggplot(df_compare, aes(x = reorder(Academy, Taux_Reussite), y = Taux_Reussite, fill = Academy)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(round(Taux_Reussite, 1), "%")), hjust = -0.1, size = 4) + 
    coord_flip() +
    labs(
      title = "Comparaison des taux de réussite par académie",
      x = "Académie",
      y = "Taux de réussite (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
    ) +
    ylim(0, max(df_compare$Taux_Reussite) + 10)  
})
}

# Lancement de l'application
shinyApp(ui = ui, server = server)