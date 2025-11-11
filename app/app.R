# --- 1. CHARGEMENT DES LIBRAIRIES ---
# Assurez-vous d'avoir installé ces packages (ex: install.packages("leaflet"))
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(scales)  # Pour les pourcentages
library(leaflet) # Pour la carte interactive
library(DT)      # Pour afficher des tables (bonus)
library(sf)

# --- 2. PRÉPARATION DES DONNÉES (GLOBAL) ---
print("Chargement des librairies... OK")

# Importer les deux jeux de données
url_lyon <- "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/main/data/clean_csv/logements_69.csv"
url_lille <- "https://raw.githubusercontent.com/ndhho/iut_sd2_rshiny_enedis/main/data/clean_csv/logements_59.csv"
data_lyon <- read.csv2(url_lyon, header = TRUE)
data_lille <- read.csv2(url_lille, header = TRUE)
print("Données brutes chargées.")

# Combiner les données brutes
data_combined_raw <- rbind(data_lyon, data_lille)

# Conversion des coordonnées (longue opération, faite une seule fois)
print("Démarrage de la conversion des coordonnées (peut prendre 1 min)...")
data_with_coords <- data_combined_raw %>%
  # S'assurer que les coordonnées sont numériques
  mutate(
    coordonnee_cartographique_x_ban = as.numeric(coordonnee_cartographique_x_ban),
    coordonnee_cartographique_y_ban = as.numeric(coordonnee_cartographique_y_ban)
  ) %>%
  filter(
    !is.na(coordonnee_cartographique_x_ban), 
    !is.na(coordonnee_cartographique_y_ban)
  ) %>%
  # 1. Convertir en objet spatial 'sf' (CRS source = Lambert-93 = 2154)
  st_as_sf(
    coords = c("coordonnee_cartographique_x_ban", "coordonnee_cartographique_y_ban"),
    crs = 2154, 
    remove = FALSE 
  ) %>%
  # 2. Transformer vers le CRS de Leaflet (WGS84 = 4326)
  st_transform(crs = 4326) %>%
  # 3. Extraire la latitude et la longitude
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  # 4. Revenir à un data.frame normal
  st_drop_geometry()

print("Conversion des coordonnées terminée.")

# Préparation finale des données
data_combined <- data_with_coords %>%
  mutate(
    Ville = case_when(
      code_departement_ban == 69 ~ "Lyon",
      code_departement_ban == 59 ~ "Lille"
    ),
    conso_m2_total = cout_total_5_usages / surface_habitable_logement,
    conso_m2_chauffage = cout_chauffage / surface_habitable_logement,
    etiquette_dpe = factor(etiquette_dpe, levels = c("A", "B", "C", "D", "E", "F", "G"))
  ) %>%
  # Filtrer les outliers et les NA (important !)
  filter(
    !is.na(Ville) & !is.na(etiquette_dpe) & !is.na(flag) &
      surface_habitable_logement > 5 & surface_habitable_logement < 500 &
      !is.na(cout_total_5_usages) & is.finite(cout_total_5_usages) & cout_total_5_usages > 0 &
      !is.na(lat) & !is.na(lon)
  )
print("Préparation des données terminée. Lancement de l'application.")


# --- 3. DÉFINITION DE L'INTERFACE UTILISATEUR (UI) ---
ui <- dashboardPage(
  
  dashboardHeader(title = "Analyse DPE 59/69"),
  
  # --- BARRE LATÉRALE (SIDEBAR) ---
  dashboardSidebar(
    sidebarMenu(
      # Onglets de navigation
      menuItem("Contexte", tabName = "contexte", icon = icon("info-circle")),
      menuItem("Overview (Carte & KPIs)", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analyse DPE", tabName = "analyse_dpe", icon = icon("chart-bar")),
      menuItem("Analyse des Coûts", tabName = "analyse_couts", icon = icon("euro-sign"))
    ),
    
    # Widgets de filtrage (pour répondre au cahier des charges)
    hr(), # Ligne de séparation
    
    selectInput("ville_filter", "Choisir la/les ville(s):",
                choices = c("Lyon", "Lille"),
                selected = c("Lyon", "Lille"),
                multiple = TRUE),
    
    checkboxGroupInput("flag_filter", "Type de logement:",
                       choices = c("neuf", "existant"),
                       selected = c("neuf", "existant")),
    
    sliderInput("surface_filter", "Filtrer par surface (m²):",
                min = 10, max = 500,
                value = c(10, 500)),
    numericInput("sample_size", "Nb. de points sur la carte (max 5000):",
                 value = 1000, min = 100, max = 5000, step = 100)
  ),
  
  # --- CORPS DE L'APPLICATION (BODY) ---
  dashboardBody(
    tabItems(
      
      # --- Onglet 1 : Contexte ---
      tabItem(tabName = "contexte",
              h2("Contexte du projet et des données"),
              fluidRow(
                box(title = "Objectif de l'application", width = 12, status = "primary", solidHeader = TRUE,
                    "Cette application a été développée pour analyser et comparer la performance énergétique (DPE) des appartements des métropoles de Lyon (69) et Lille (59).",
                    br(),
                    "Elle permet de visualiser la répartition des DPE, la part des sources d'énergie (notamment l'électricité) et l'impact sur les coûts énergétiques au m²."
                ),
                box(title = "Données", width = 12, status = "info",
                    "Les données proviennent de l'ADEME et concernent les DPE des logements neufs et existants. Elles ont été pré-nettoyées et filtrées pour ne conserver que les appartements."
                ),
                box(title = "Aperçu des données (échantillon)", width = 12, status = "info", solidHeader = TRUE,
                    h4("Un échantillon aléatoire de 100 lignes est affiché ci-dessous."),
                    # 'DT' est la version 'Shiny' de 'datatable'
                    DTOutput("table_echantillon")
                ),
                box(title = "Partenaires et Remerciements", width = 12, status = "info",
                    
                    # Nous utilisons du CSS simple pour centrer et espacer les logos
                    tags$div(
                      style = "display: flex; justify-content: space-around; align-items: center; padding: 20px;",
                      
                      # Logo 1 : ENEDIS
                      tags$img(src = "logo_enedis.jpg",
                               title = "ENEDIS",
                               height = "70px"),  # Ajustez la hauteur
                      
                      # Logo 2 : ADEME
                      tags$img(src = "logo_ademe.png",
                               title = "ADEME",
                               height = "70px"),
                      
                      # Logo 3 : IUT
                      tags$img(src = "logo_iut.png", 
                               title = "Logo École",
                               height = "70px"),
                      
                    )
                )
              )
      ),
      
      # --- Onglet 2 : Overview (Carte & KPIs) ---
      tabItem(tabName = "overview",
              h2("Overview (Carte & KPIs)"), # Titre mis à jour
              
              # LIGNE 1 : Les KPIs (que nous remettons)
              fluidRow(
                valueBoxOutput("kpi_logements"),
                valueBoxOutput("kpi_passoires"),
                valueBoxOutput("kpi_cout_m2_total")
              ),
              
              # LIGNE 2 : La carte (qui fonctionne)
              fluidRow(
                box(title = "Carte interactive des DPE", width = 12, status = "primary", solidHeader = TRUE,
                    # On utilise bien 'map_points' et une bonne hauteur
                    leafletOutput("map_points", height = "70vh") 
                )
              )
      ),
      
      # --- Onglet 3 : Analyse DPE (Graphiques Rmd 1 & 2) ---
      tabItem(tabName = "analyse_dpe",
              h2("Performance DPE et Sources d'Énergie"),
              fluidRow(
                box(title = "Répartition des étiquettes DPE", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("plot_dpe_repartition") # Sortie pour le graphique Rmd 1
                )
              ),
              fluidRow(
                box(title = "Focus sur la part de l'électricité", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("plot_pie_chart") # Sortie pour le graphique Rmd 2
                )
              )
      ),
      
      # --- Onglet 4 : Analyse des Coûts (Graphiques Rmd 3 & 4) ---
      tabItem(tabName = "analyse_couts",
              h2("Analyse des Coûts au m² (Logements Existants)"),
              fluidRow(
                box(title = "Coût Énergétique Total Moyen par m²", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("plot_cost_total_m2") # Sortie pour le graphique Rmd 3
                )
              ),
              fluidRow(
                box(title = "Coût du Chauffage ÉLECTRIQUE par m²", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("plot_cost_elec_m2") # Sortie pour le graphique Rmd 4
                )
              )
      )
    )
  )
)


# --- 4. DÉFINITION DU SERVEUR (LOGIQUE) ---
server <- function(input, output, session) {
  
  # --- A. Données Réactives ---
  # Créer un dataframe réactif qui se met à jour en fonction des filtres
  data_filtered <- reactive({
    data_combined %>%
      filter(
        Ville %in% input$ville_filter,
        flag %in% input$flag_filter,
        surface_habitable_logement >= input$surface_filter[1],
        surface_habitable_logement <= input$surface_filter[2]
      )
  })
  
  
  # --- B. Logique pour l'onglet 'Overview' ---
  
  # --- Définitions globales pour la carte (exécutées une fois) ---
  
  # 1. Palette de couleurs (Vert -> Rouge)
  dpe_colors <- c("#28a745", "#a0c22c", "#fff33b", "#f8d432", "#f2a822", "#f08024", "#d7191c")
  pal <- colorFactor(palette = dpe_colors, domain = levels(data_combined$etiquette_dpe))
  
  # 2. Plage Logarithmique (basée sur TOUTES les données)
  log_range <- range(log10(data_combined$cout_total_5_usages + 1), na.rm = TRUE)
  # Gérer le cas où data_combined est vide et log_range est invalide
  if (!all(is.finite(log_range))) {
    log_range <- c(log10(100+1), log10(5000+1)) # Plage par défaut
  }
  
  # 3. Fonction pour calculer le rayon
  #    (le log(cost) est MAINTENANT à l'intérieur de la fonction)
  scale_radius <- function(cost) {
    scales::rescale(log10(cost + 1), to = c(4, 20), from = log_range)
  }
  
  
  # --- Fin des définitions globales ---
  
  
  # --- Rendu des KPIs ---
  
  output$kpi_logements <- renderValueBox({
    nb_logements <- nrow(data_filtered())
    valueBox(
      value = prettyNum(nb_logements, big.mark = " "),
      subtitle = "Logements dans la sélection",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$kpi_passoires <- renderValueBox({
    passoires <- data_filtered() %>%
      filter(etiquette_dpe %in% c("F", "G")) %>%
      nrow()
    total <- nrow(data_filtered())
    taux <- if(total > 0) (passoires / total) else 0
    
    valueBox(
      value = percent(taux, accuracy = 0.1),
      subtitle = "Taux de passoires (F+G)",
      icon = icon("thermometer-empty"),
      color = "red"
    )
  })
  
  output$kpi_cout_m2_total <- renderValueBox({
    cout_moyen <- data_filtered() %>%
      summarise(moy = mean(conso_m2_total, na.rm = TRUE)) %>%
      pull(moy)
    
    valueBox(
      value = paste(round(cout_moyen, 2), "€/m²"),
      subtitle = "Coût total moyen / m²",
      icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  # --- Données réactives pour la CARTE ---
  data_map <- reactive({
    data_to_sample <- data_filtered()
    n_total <- nrow(data_to_sample)
    sample_size <- min(c(input$sample_size, 5000, n_total))
    
    if (n_total == 0) return(data.frame()) 
    
    data_to_sample %>%
      sample_n(size = sample_size) %>%
      mutate(
        # On utilise la fonction 'scale_radius' définie au-dessus
        radius = scale_radius(cout_total_5_usages) 
      )
  })
  
  # --- Rendu de la CARTE ---
  output$map_points <- renderLeaflet({
    
    map_data <- data_map()
    
    # Créer la carte de base
    map_leaflet <- leaflet(data = map_data) %>%
      addTiles() %>%
      setView(lng = 3.9, lat = 48.0, zoom = 5.5) %>% 
      # Légende : Couleurs DPE uniquement
      addLegend(
        pal = pal, 
        values = levels(data_combined$etiquette_dpe), 
        title = "Étiquette DPE",
        position = "bottomright"
      )
    
    # Ajouter les points (si les données existent)
    if(nrow(map_data) > 0) {
      map_leaflet %>%
        addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          fillColor = ~pal(etiquette_dpe),
          fillOpacity = 0.8,              
          color = "black",                
          weight = 1,
          opacity = 1,
          radius = ~radius,
          popup = ~paste(
            "<b>DPE:", etiquette_dpe, "</b><br/>",
            "Ville:", Ville, "<br/>",
            "Coût 5 usages:", cout_total_5_usages, "€<br/>",
            "Surface:", surface_habitable_logement, "m²"
          )
        )
    } else {
      map_leaflet
    }
  })
  
  
  # --- C. Logique pour l'onglet 'Analyse DPE' ---
  
  # Graphique 1 : Répartition DPE (basé sur le Rmd)
  output$plot_dpe_repartition <- renderPlot({
    
    data_plot <- data_filtered() %>%
      group_by(Ville) %>%
      mutate(Total_Ville = n()) %>%
      group_by(Ville, etiquette_dpe, Total_Ville) %>%
      summarise(Nombre = n(), 
                Proportion = Nombre / first(Total_Ville),
                .groups = 'drop')
    
    ggplot(data_plot, aes(x = etiquette_dpe, y = Proportion, fill = Ville)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = scales::percent(Proportion, accuracy = 0.1)), 
        position = position_dodge(width = 0.9),
        vjust = -0.5, size = 3
      ) +
      labs(title = "Répartition des étiquettes DPE",
           x = "Classe DPE", y = "Proportion des logements (%)") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Lyon" = "#0047AB", "Lille" = "#E69F00")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Graphique 2 : Camemberts (basé sur le Rmd)
  output$plot_pie_chart <- renderPlot({
    
    data_pie_reactive <- data_filtered() %>%
      mutate(
        energie_focus = if_else(
          type_energie_principale_chauffage == "Électricité", 
          "Électricité", 
          "Autres Energies"
        )
      ) %>%
      group_by(Ville, flag, energie_focus) %>%
      summarise(n = n(), .groups = 'drop') %>%
      group_by(Ville, flag) %>%
      mutate(total_groupe = sum(n),
             proportion = n / total_groupe) %>%
      arrange(Ville, flag, desc(energie_focus))
    
    ggplot(data_pie_reactive, aes(x = "", y = proportion, fill = energie_focus)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      facet_grid(Ville ~ flag) +
      geom_text(
        aes(label = percent(proportion, accuracy = 1)), 
        position = position_stack(vjust = 0.5),
        color = "white", size = 4
      ) +
      scale_fill_manual(values = c("Électricité" = "#FFA500", "Autres Energies" = "#AAAAAA"),
                        name = "Source d'énergie") +
      theme_void() +
      labs(title = "Focus sur la part de l'électricité dans le chauffage principal") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom",
            strip.text = element_text(face = "bold", size = 10))
  })
  
  
  # --- D. Logique pour l'onglet 'Analyse des Coûts' ---
  
  # Graphique 3 : Coût total m² (basé sur le Rmd)
  output$plot_cost_total_m2 <- renderPlot({
    
    data_plot <- data_filtered() %>%
      filter(flag == "existant") %>%
      group_by(Ville, etiquette_dpe) %>%
      summarise(conso_m2_moy = mean(conso_m2_total, na.rm = TRUE),
                .groups = 'drop') %>%
      filter(is.finite(conso_m2_moy) & conso_m2_moy > 0)
    
    ggplot(data_plot, aes(x = etiquette_dpe, y = conso_m2_moy)) +
      geom_bar(stat = "identity", fill = "#D55E00") + 
      geom_text(aes(label = round(conso_m2_moy, 2)), 
                vjust = -0.5, size = 3, color = "black") +
      facet_wrap(~ Ville) + 
      scale_y_continuous(limits = c(0, max(data_plot$conso_m2_moy, na.rm = TRUE) * 1.1)) +
      labs(title = "Coût Énergétique Total Moyen par m² (Logements Existants)",
           x = "Classe DPE", y = "Coût Moyen par m² (€/m²)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Graphique 4 : Coût chauffage ÉLECTRIQUE m² (basé sur le Rmd)
  output$plot_cost_elec_m2 <- renderPlot({
    
    data_plot <- data_filtered() %>%
      filter(
        flag == "existant", 
        type_energie_principale_chauffage == "Électricité"
      ) %>%
      group_by(Ville, etiquette_dpe) %>%
      summarise(
        conso_m2_chauffage_moy = mean(conso_m2_chauffage, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(is.finite(conso_m2_chauffage_moy) & conso_m2_chauffage_moy > 0)
    
    # Gérer le cas où il n'y a pas de données
    if (nrow(data_plot) == 0) {
      return(ggplot() + labs(title = "Pas de données pour cette sélection"))
    }
    
    ggplot(data_plot, aes(x = etiquette_dpe, y = conso_m2_chauffage_moy)) +
      geom_bar(stat = "identity", fill = "#0072B2") + 
      geom_text(
        aes(label = round(conso_m2_chauffage_moy, 2)), 
        vjust = -0.5, size = 3, color = "black"
      ) +
      facet_wrap(~ Ville) + 
      scale_y_continuous(limits = c(0, max(data_plot$conso_m2_chauffage_moy, na.rm = TRUE) * 1.1)) +
      labs(
        title = "Coût du Chauffage ÉLECTRIQUE par m² (Logements Existants)",
        subtitle = "Uniquement pour les logements où l'électricité est le chauffage principal",
        x = "Classe DPE", y = "Coût Chauffage Électrique Moyen (€/m²)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })
  

# --- E. Logique pour l'onglet 'Contexte' ---

  output$table_echantillon <- renderDT({
    
    # Prendre 100 lignes au hasard dans la BDD complète (avant filtres)
    echantillon <- data_combined %>%
      sample_n(100) %>%
      # Sélectionner les colonnes les plus pertinentes à montrer
      select(
        Ville, 
        flag, 
        etiquette_dpe, 
        etiquette_ges, 
        surface_habitable_logement,
        cout_total_5_usages,
        type_energie_principale_chauffage
      )
    
    # Afficher le datatable
    datatable(
      echantillon,
      rownames = FALSE,
      options = list(
        pageLength = 5, # N'afficher que 5 lignes à la fois
        scrollX = TRUE, # Permettre le scroll horizontal si trop de colonnes
        dom = "tp"      # N'afficher que la table et la pagination ('t' et 'p')
      )
    )
  })
}
# --- 5. LANCEMENT DE L'APPLICATION ---
shinyApp(ui = ui, server = server)