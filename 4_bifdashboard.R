library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(factoextra)
library(cluster)
library(corrplot)
library(tidyr)
library(DT)
library(httr)
library(ggimage)
library(rsconnect)
library(readr)

#setwd("/Users/lukasbachcouzy/Documents/DAL-Projects/2.semester/EksamenF25/cphBIF")
#saveRDS(allpass,"All_Passes_player_stat_clusters.rds")
#allpass <- allpasses[,1:32]
#allpass <- left_join(allpass,pstats, by= "SHORTNAME")

allpasses <- readRDS("All_Passes_player_stat_clusters.rds")
kmod <- readRDS("Kmod.RDS")
pos <- read_rds("POS.rds")
passCluster <- read_rds("passesCLUSTER.rds")
allpasses <- left_join(allpasses,pos,by ="EVENT_WYID")
allpasses <- left_join(allpasses,passCluster, by="EVENT_WYID")

allpasses$SEASON_WYID.x <- ifelse(allpasses$SEASON_WYID.x == "189918", 
                                  gsub("189918", "24/25", allpasses$SEASON_WYID.x), 
                                  "23/24")
allpasses <- allpasses %>%
  filter(!is.na(main_cluster)) %>%
  mutate(
    main_cluster = as.numeric(as.character(main_cluster))
  ) %>%
  filter(SHORTNAME != "Y. Badji")

team_summary <- allpasses %>%
  group_by(SEASON_WYID.x, TEAMNAME, IMAGEDATAURL.y) %>%
  summarise(
    AntalAfleveringer = n(),
    Accuracy = round(mean(ACCURATE, na.rm = TRUE), 2),
    AntalKampe = n_distinct(MATCH_WYID),
    Gns_pass_kamp = round(AntalAfleveringer / AntalKampe, 2),
    Gns_længde = round(mean(LENGTH), 2),
    Main_cluster = as.numeric(names(sort(table(main_cluster), decreasing = TRUE))[1]),
    .groups = "drop"
  )

Match_summary <- allpasses %>%
  group_by(SEASON_WYID.x, TEAMNAME, MATCH_LABEL) %>%
  summarise(
    AntalAfleveringer = n(),
    Accuracy = round(mean(ACCURATE, na.rm = TRUE), 2),
    AntalKampe = n_distinct(MATCH_WYID),
    Gns_pass_kamp = round(AntalAfleveringer / AntalKampe, 2),
    Gns_længde = round(mean(LENGTH), 2),
    Main_cluster = as.integer(names(sort(table(main_cluster), decreasing = TRUE))[1]),
    .groups = "drop"
  )

pass_summary <- allpasses %>%
  group_by(SEASON_WYID.x, TEAMNAME, SHORTNAME, IMAGEDATAURL.x) %>%
  summarise(
    AntalAfleveringer = n(),
    Accuracy = round(mean(ACCURATE, na.rm = TRUE), 2)*100,
    SD_Accuracy = round(sd(ACCURATE, na.rm = TRUE), 2)*100,
    AntalKampe = n_distinct(MATCH_WYID),
    Gns_pass_kamp = round(AntalAfleveringer / AntalKampe, 2),
    Gns_længde = round(mean(LENGTH), 2),
    SD_længde = round(sd(LENGTH),2),
    Main_cluster = as.numeric(names(sort(table(main_cluster), decreasing = TRUE))[1]),
    .groups = "drop"
  )


# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Afleveringsoversigt – Superligaen"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stats", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Afleveringer i hele Superligaen", tabName = "overview", icon = icon("soccer-ball")),
      menuItem("Afleveringer i alle kampe", tabName = "kampanalyse", icon = icon("binoculars"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
      /* --------- GLOBALT --------- */
      body, .content-wrapper, .right-side { background-color: #002b7f; }
      /* Fix hvide margener */
      .skin-blue .wrapper { background: #002b7f; }

      /* --------- NAVBAR --------- */
      .main-header .navbar      { background-color: #002b7f !important; }
      .main-header .logo        { background-color: #002b7f !important; }
      .main-header .logo:hover  { background-color: #002b7f !important; }
      .navbar-brand,
      .main-header .logo .logo-lg,
      .main-header .logo .logo-mini { color: #ffffff !important; font-weight: bold; }

      /* --------- KNAPPER --------- */
      .btn-primary, .btn-primary:focus, .btn-primary:active {
        background-color: #ffd700 !important;
        border-color:      #ffd700 !important;
        color: black !important;
      }
      .btn-primary:hover {
        background-color: #ffc300 !important;
        border-color:     #ffc300 !important;
        color: black !important;
      }

      /* --------- INFO‑BOKSE --------- */
      .info-box, .box.info-box {
        background-color: #fffbea;
        border: 1px solid #cccccc;
        border-radius: 5px;
        padding: 15px;
        font-size: 15px;
      }

      /* --------- PLAYER / TEAM BILLEDER --------- */
      .player-button img, .team-button img {
        transition: transform 0.2s ease;
      }
      .player-button img:hover,
      .team-button img:hover   { transform: scale(1.1); }

      /* --------- CENTRERET KONTROL‑SEKTION --------- */
      .control-buttons { text-align: center; padding: 30px; }
    ")),
    
    tabItems(
      
      # --- TAB 3: STATS ---
      tabItem(
        tabName = "stats",
        
        # Sæsonvælger
        fluidRow(
          box(
            width = 12,
            selectInput("season_stats", "Vælg sæson til stats:", choices = NULL)
          )
        ),
        
        # Topspillere og afleveringer pr. hold
        fluidRow(
          box(
            width = 6,
            title = "Top 12 afleverende spillere",
            plotOutput("top_players_plot")
          ),
          box(
            width = 6,
            title = "Antal afleveringer pr. hold",
            plotOutput("passes_by_team_plot")
          )
        ),
        
        # Spillerstatistikker
        fluidRow(
          box(
            width = 12,
            title = "Spillerstatistikker",
            DTOutput("player_stats_table")
          )
        ),
        
        # Clustervælger og heatmaps
        fluidRow(
          box(
            width = 12,
            selectInput("selected_cluster", "Vælg cluster:", choices = 1:8, selected = 1)
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Startpositioner – Afleverings-heatmap",
            plotOutput("cluster_heatmap_start")
          ),
          box(
            width = 6,
            title = "Slutpositioner – Afleverings-heatmap",
            plotOutput("cluster_heatmap_end")
          )
        ),
        
        # Spillere i valgt cluster
        fluidRow(
          box(
            width = 12,
            title = "Spillere i valgt cluster",
            DTOutput("cluster_players_table")
          )
        )
      ),
      
      # --- TAB 1: OVERVIEW ---
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            selectInput("season_choice", "Vælg sæson:", choices = NULL)
          )
        ),
        fluidRow(
          box(
            width = 12, 
            title = "Klik på et holdlogo for at se spillere og afleveringsmønstre",
            plotOutput("logo_plot", height = "220px", click = "logo_click")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Mest hyppige afleveringsmønster",
            htmlOutput("cluster_info_team")
          )
        ),
        fluidRow(
          box(
            width = 6, title = "TEAM: Afleveringsstart",
            plotOutput("heatmap_team_start")
          ),
          box(
            width = 6, title = "TEAM: Afleveringsslut",
            plotOutput("heatmap_team_end")
          )
        ),
        fluidRow(
          box(
            width = 6, title = "SPILLER: Afleveringsstart",
            plotOutput("heatmap_player_start")
          ),
          box(
            width = 6, title = "SPILLER: Afleveringsslut",
            plotOutput("heatmap_player_end")
          ),
          box(
            width = 12,
            title = "Spillerens mest hyppige afleveringsmønster",
            htmlOutput("cluster_info_player")
          )
        ),
        fluidRow(
          box(
            width = 12, 
            title = "Spillere i valgt hold", 
            uiOutput("player_faces")
          )
        )
      ),
      
      # --- TAB 2: KAMPANALYSE ---
      tabItem(
        tabName = "kampanalyse",
        fluidRow(
          box(width = 6,
              selectInput("selected_match", "Vælg kamp:", choices = NULL)
          )
        ),
        fluidRow(
          box(width = 6, title = "Hold 1 cluster-id i kampen", htmlOutput("cluster_id_team1")),
          box(width = 6, title = "Hold 2 cluster-id i kampen", htmlOutput("cluster_id_team2"))
        ),
        fluidRow(
          box(width = 6, title = "Hold 1: Start-locations (hele kampen)",
              plotOutput("match_heatmap_team1_start")),
          box(width = 6, title = "Hold 2: Start-locations (hele kampen)",
              plotOutput("match_heatmap_team2_start"))
        ),
        fluidRow(
          box(width = 6,
              title = textOutput("title_team1_cluster"),
              plotOutput("match_heatmap_team1_cluster")),
          box(width = 6,
              title = textOutput("title_team2_cluster"),
              plotOutput("match_heatmap_team2_cluster"))
        ),
        fluidRow(
          box(width = 6,
              title = "Cluster-fordeling for Hold 1",
              plotOutput("match_barplot_team1_clusters")),
          box(width = 6,
              title = "Cluster-fordeling for Hold 2",
              plotOutput("match_barplot_team2_clusters"))
        )
      )
    ) # tabItems slutter her
  ) # dashboardBody slutter her
)

# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  # Top 12 spillere
  observe({
    updateSelectInput(
      session, "season_stats",
      choices = unique(allpasses$SEASON_WYID.x),
      selected = unique(allpasses$SEASON_WYID.x)[1]
    )
  })
  
  stats_data <- reactive({
    req(input$season_stats)
    allpasses %>% filter(SEASON_WYID.x == input$season_stats)
  })
  
  output$top_players_plot <- renderPlot({
    df <- stats_data() %>%
      group_by(SHORTNAME, IMAGEDATAURL.x) %>%
      summarise(AntalAfleveringer = n(), .groups = "drop") %>%
      arrange(desc(AntalAfleveringer)) %>%
      slice_head(n = 12)
    
    ggplot(df, aes(x = reorder(SHORTNAME, AntalAfleveringer), y = AntalAfleveringer)) +
      geom_col(fill = "#003366") +
      geom_image(aes(image = IMAGEDATAURL.x), size = 0.05, by = "height", asp = 1.5) +
      coord_flip() +
      labs(x = "Spiller", y = "Afleveringer") +
      theme_minimal()
  })
  
  output$team_passes_plot <- renderPlot({
    df <- stats_data() %>%
      group_by(TEAMNAME, IMAGEDATAURL.y) %>%
      summarise(AntalAfleveringer = n(), .groups = "drop") %>%
      arrange(desc(AntalAfleveringer))
    
    ggplot(df, aes(x = reorder(TEAMNAME, AntalAfleveringer), y = AntalAfleveringer)) +
      geom_col(fill = "#003366") +
      geom_image(aes(image = IMAGEDATAURL.y), size = 0.05, by = "height", asp = 1.5) +
      coord_flip() +
      labs(x = "Hold", y = "Afleveringer") +
      theme_minimal()
  })
  
  output$player_stats_table <- renderDT({
    df <- pass_summary %>%
      filter(SEASON_WYID.x == input$season_stats) %>%
      mutate(
        Accuracy = paste0(round(Accuracy, 1), "%"),
        SD_Accuracy = paste0(round(SD_Accuracy, 1), "%")
      ) %>%
      select(
        SHORTNAME,
        TEAMNAME,
        AntalKampe,
        AntalAfleveringer,
        Accuracy,
        SD_Accuracy,
        Gns_pass_kamp,
        Gns_længde,
        SD_længde,
        Main_cluster
      )
    
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  filtered_team_stats <- reactive({
    req(input$season_stats)
    team_summary %>%
      filter(SEASON_WYID.x == input$season_stats)
  })
  output$passes_by_team_plot <- renderPlot({
    df <- filtered_team_stats() %>%
      arrange(desc(AntalAfleveringer)) %>%
      slice_max(order_by = AntalAfleveringer, n = 12)
    
    ggplot(df, aes(x = reorder(TEAMNAME, AntalAfleveringer), y = AntalAfleveringer)) +
      geom_col(fill = "#003366") +
      geom_image(aes(image = IMAGEDATAURL.y), size = 0.06, by = "width", asp = 1.5) +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "Afleveringer", title = "Top 12 hold – afleveringer i sæsonen") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.y = element_text(face = "bold")
      )
  })
  
  
  ###heatmaps til introsiden:
  
  # --- CLUSTERANALYSE: Tabel over spillere i valgt cluster ---
  output$cluster_players_table <- renderDT({
    req(input$selected_cluster)
    
    cluster_id <- as.numeric(input$selected_cluster)
    
    player_data <- allpasses %>%
      filter(main_cluster == cluster_id) %>%
      group_by(SHORTNAME, TEAMNAME) %>%
      summarise(
        AntalAfleveringer = n(),
        Accuracy = round(mean(ACCURATE, na.rm = TRUE), 2) * 100,
        AntalKampe = n_distinct(MATCH_WYID),
        Gns_pass_kamp = round(AntalAfleveringer / AntalKampe, 2),
        Gns_længde = round(mean(LENGTH, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(AntalAfleveringer))
    
    datatable(player_data, rownames = FALSE, options = list(pageLength = 10))
  })
  
  # --- CLUSTERANALYSE: Heatmap over startpositioner ---
  output$cluster_heatmap_start <- renderPlot({
    req(input$selected_cluster)
    
    cluster_id <- as.numeric(input$selected_cluster)
    
    data_cluster <- allpasses %>% filter(main_cluster == cluster_id)
    
    ggplot(data_cluster) +
      annotate_pitch(fill = "gray", colour = "white") +
      stat_density_2d_filled(
        aes(x = LOCATIONX, y = LOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      labs(title = paste("Start-locations – Cluster", cluster_id)) +
      scale_fill_viridis_d(option = "magma")
  })
  
  # --- CLUSTERANALYSE: Heatmap over slutpositioner ---
  output$cluster_heatmap_end <- renderPlot({
    req(input$selected_cluster)
    
    cluster_id <- as.numeric(input$selected_cluster)
    
    data_cluster <- allpasses %>% filter(main_cluster == cluster_id)
    
    ggplot(data_cluster) +
      annotate_pitch(fill = "gray", colour = "white") +
      stat_density_2d_filled(
        aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      labs(title = paste("End-locations – Cluster", cluster_id)) +
      scale_fill_viridis_d(option = "magma")
  })
  #### HOLDLOGOER OSV ####
  # 0) Opdater sæson-valg ved app-start
  observe({
    updateSelectInput(
      session, "season_choice",
      choices = unique(allpasses$SEASON_WYID.x),
      selected = unique(allpasses$SEASON_WYID.x)[1]
    )
  })
  
  # 1) Filtrer data på valgt sæson
  filtered_data <- reactive({
    req(input$season_choice)
    allpasses %>% 
      filter(SEASON_WYID.x == input$season_choice)
  })
  
  # 2) Klargør hold + logo => giv dem x,y koordinater
  teams_data <- reactive({
    df <- filtered_data() %>%
      distinct(TEAMNAME, IMAGEDATAURL.y) %>%
      group_by(TEAMNAME) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(TEAMNAME)
    
    df <- df %>%
      mutate(xpos = row_number(),
             ypos = 0)
    df
  })
  
  # ReactiveVal til hold + spiller
  selected_team <- reactiveVal(NULL)
  selected_player <- reactiveVal(NULL)
  
  # PLOT: holdlogoer side om side
  output$logo_plot <- renderPlot({
    req(teams_data())
    
    ggplot(teams_data(), aes(x = xpos, y = ypos)) +
      geom_image(aes(image = IMAGEDATAURL.y), size = 0.7, by = "height") +
      theme_void() +
      scale_y_continuous(limits = c(-0.5, 0.5)) +
      scale_x_continuous(expand = expansion(mult = 0.05)) +
      labs(title = "Vælg hold, for at se deres mest hyppige afleveringsmønster") +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  # Klik på plot => find hold via nearPoints
  observeEvent(input$logo_click, {
    req(teams_data())
    clickData <- nearPoints(
      teams_data(),
      coordinfo = input$logo_click,
      xvar = "xpos",
      yvar = "ypos",
      maxpoints = 1,
      threshold = 30
    )
    
    if (nrow(clickData) == 1) {
      selected_team(clickData$TEAMNAME)
      selected_player(NULL) # nulstil spiller, hvis man vælger et nyt hold
    }
  })
  
  # (A) Team cluster-data (ud fra mest hyppige cluster) -------------------
  team_cluster_data <- reactive({
    req(selected_team())
    team_data <- filtered_data() %>% filter(TEAMNAME == selected_team())
    
    # Hent mest hyppige cluster:
    cluster_id <- as.integer(names(sort(table(team_data$main_cluster), decreasing = TRUE))[1])  # <-- Vigtig ændring
    
    # Filtrer allpasses på hele ligaens data for det cluster
    allpasses %>%
      filter(main_cluster == cluster_id)
  })
  
  # (B) Udskriv holdets cluster
  output$cluster_info_team <- renderUI({
    req(selected_team())
    holddata <- filtered_data() %>% filter(TEAMNAME == selected_team())
    
    # Mest hyppige cluster
    cluster_id <- as.integer(names(sort(table(holddata$main_cluster), decreasing = TRUE))[1])  # <-- Vigtig ændring
    
    HTML(paste0(
      "<div style='background:#fafafa; padding:10px; border-radius:5px'>",
      "<strong>Hold:</strong> ", selected_team(), "<br>",
      "<strong>Cluster:</strong> ", cluster_id,
      "</div>"
    ))
  })
  
  # (C) Team: Start-locations
  output$heatmap_team_start <- renderPlot({
    req(team_cluster_data())
    
    ggplot(team_cluster_data()) +
      annotate_pitch(fill = "gray", colour = "white") +
      stat_density_2d_filled(
        aes(x = LOCATIONX, y = LOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      labs(title = paste("Start-locations for", selected_team())) +
      scale_fill_viridis_d(option = "magma")
  })
  
  # (D) Team: End-locations
  output$heatmap_team_end <- renderPlot({
    req(team_cluster_data())
    
    ggplot(team_cluster_data()) +
      annotate_pitch(fill = "gray", colour = "white") +
      stat_density_2d_filled(
        aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      labs(title = paste("End-locations for", selected_team())) +
      scale_fill_viridis_d(option = "magma")
  })
  
  # 3) UI: Spillere i valgt hold
  output$player_faces <- renderUI({
    req(selected_team())
    players <- filtered_data() %>%
      filter(TEAMNAME == selected_team()) %>%
      distinct(SHORTNAME, IMAGEDATAURL.x)
    
    fluidRow(
      lapply(seq_len(nrow(players)), function(i) {
        actionButton(
          inputId = paste0("player_", i),
          label = tags$div(
            tags$img(src = players$IMAGEDATAURL.x[i], height = "150px"),
            tags$br(),
            players$SHORTNAME[i],
            class = "player-button"
          ),
          style = "margin:5px; padding:2px;"
        )
      })
    )
  })
  
  # OBSERVER: klik på spiller => gem i selected_player
  observe({
    req(selected_team())
    players <- filtered_data() %>%
      filter(TEAMNAME == selected_team()) %>%
      distinct(SHORTNAME, IMAGEDATAURL.x)
    
    lapply(seq_len(nrow(players)), function(i) {
      observeEvent(input[[paste0("player_", i)]], {
        selected_player(players$SHORTNAME[i])
      })
    })
  })
  
  # (A) Player cluster-data
  player_cluster_data <- reactive({
    req(selected_player())
    spillerdata <- filtered_data() %>% filter(SHORTNAME == selected_player())
    
    # Mest hyppige cluster
    cluster_id <- as.integer(names(sort(table(spillerdata$main_cluster), decreasing = TRUE))[1])  # <-- Vigtig ændring
    
    # Filtrer allpasses på hele ligaen for det cluster
    allpasses %>%
      filter(cluster == cluster_id)
  })
  
  # (B) Udskriv spillerens cluster
  output$cluster_info_player <- renderUI({
    req(selected_player())
    spillerdata <- filtered_data() %>% filter(SHORTNAME == selected_player())
    
    # Mest hyppige cluster
    cluster_id <- as.integer(names(sort(table(spillerdata$main_cluster), decreasing = TRUE))[1])  # <-- Vigtig ændring
    
    HTML(paste0(
      "<div style='background:#fafafa; padding:10px; border-radius:5px'>",
      "<strong>Spiller:</strong> ", selected_player(), "<br>",
      "<strong>Cluster:</strong> ", cluster_id,
      "</div>"
    ))
  })
  
  # (C) Spiller: Start-locations
  output$heatmap_player_start <- renderPlot({
    req(player_cluster_data())
    
    ggplot(player_cluster_data()) +
      annotate_pitch(fill = "gray", colour = "white") +
      stat_density_2d_filled(
        aes(x = LOCATIONX, y = LOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      labs(title = paste("Start-locations for", selected_player())) +
      scale_fill_viridis_d(option = "magma")
  })
  
  # (D) Spiller: End-locations
  output$heatmap_player_end <- renderPlot({
    req(player_cluster_data())
    
    ggplot(player_cluster_data()) +
      annotate_pitch(fill = "gray", colour = "white") +
      stat_density_2d_filled(
        aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      labs(title = paste("End-locations for", selected_player())) +
      scale_fill_viridis_d(option = "magma")
  })
  
  
  # ---- KAMPANALYSE (Opgave 4.2) ----
  observe({
    req(nrow(allpasses) > 0)
    updateSelectInput(session, "selected_match",
                      choices = unique(allpasses$MATCH_LABEL),
                      selected = head(unique(allpasses$MATCH_LABEL), 1))
  })
  
  # 1) match_data: Alle afleveringer for den valgte kamp
  match_data <- reactive({
    req(input$selected_match)
    allpasses %>%
      filter(MATCH_LABEL == input$selected_match)
  })
  
  # 2) Hent holdnavne i kampen
  hold1 <- reactive({
    unique(match_data()$TEAMNAME)[1]
  })
  hold2 <- reactive({
    unique(match_data()$TEAMNAME)[2]
  })
  
  # 3) Find dominerende cluster for hvert hold i kampen
  hold1_cluster <- reactive({
    match_data() %>%
      filter(TEAMNAME == hold1()) %>%
      count(cluster, sort = TRUE) %>%
      slice(1) %>%
      pull(cluster)
  })
  
  hold2_cluster <- reactive({
    match_data() %>%
      filter(TEAMNAME == hold2()) %>%
      count(cluster, sort = TRUE) %>%
      slice(1) %>%
      pull(cluster)
  })
  
  # 4) Tekst: Vis dominerende cluster for hvert hold
  output$cluster_id_team1 <- renderUI({
    req(hold1(), hold1_cluster())
    HTML(paste0("<strong>", hold1(), "</strong> – Dominerende cluster: <strong>", hold1_cluster(), "</strong>"))
  })
  
  output$cluster_id_team2 <- renderUI({
    req(hold2(), hold2_cluster())
    HTML(paste0("<strong>", hold2(), "</strong> – Dominerende cluster: <strong>", hold2_cluster(), "</strong>"))
  })
  
  output$title_team1_cluster <- renderText({
    paste("End-locations –", hold1(), "(Cluster", hold1_cluster(), ")")
  })
  output$title_team2_cluster <- renderText({
    paste("End-locations –", hold2(), "(Cluster", hold2_cluster(), ")")
  })
  
  # 5) START-locations heatmaps for de to holds mest hyppige cluster
  output$match_heatmap_team1_start <- renderPlot({
    req(hold1())
    cluster_id <- match_data() %>%
      filter(TEAMNAME == hold1()) %>%
      count(cluster, sort = TRUE) %>%
      slice(1) %>%
      pull(cluster)
    
    cluster_data <- allpasses %>%
      filter(cluster == cluster_id)
    
    ggplot(cluster_data) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(
        aes(x = LOCATIONX, y = LOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      scale_fill_viridis_d(option = "magma") +
      theme(legend.position = "right") +
      labs(title = paste("Start-locations –", hold1(), "(Cluster", cluster_id, ")"))
  })
  
  output$match_heatmap_team1_cluster <- renderPlot({
    req(hold1())
    cluster_id <- match_data() %>%
      filter(TEAMNAME == hold1()) %>%
      count(cluster, sort = TRUE) %>%
      slice(1) %>%
      pull(cluster)
    
    cluster_data <- allpasses %>%
      filter(cluster == cluster_id)
    
    ggplot(cluster_data) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(
        aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      scale_fill_viridis_d(option = "magma") +
      theme(legend.position = "right") +
      labs(title = paste("End-locations –", hold1(), "(Cluster", cluster_id, ")"))
  })
  
  output$match_heatmap_team2_start <- renderPlot({
    req(hold2())
    cluster_id <- match_data() %>%
      filter(TEAMNAME == hold2()) %>%
      count(cluster, sort = TRUE) %>%
      slice(1) %>%
      pull(cluster)
    
    cluster_data <- allpasses %>%
      filter(cluster == cluster_id)
    
    ggplot(cluster_data) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(
        aes(x = LOCATIONX, y = LOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      scale_fill_viridis_d(option = "magma") +
      theme(legend.position = "right") +
      labs(title = paste("Start-locations –", hold2(), "(Cluster", cluster_id, ")"))
  })
  
  output$match_heatmap_team2_cluster <- renderPlot({
    req(hold2())
    cluster_id <- match_data() %>%
      filter(TEAMNAME == hold2()) %>%
      count(cluster, sort = TRUE) %>%
      slice(1) %>%
      pull(cluster)
    
    cluster_data <- allpasses %>%
      filter(cluster == cluster_id)
    
    ggplot(cluster_data) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(
        aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY),
        alpha = 0.7, contour_var = "ndensity"
      ) +
      theme_pitch() +
      scale_fill_viridis_d(option = "magma") +
      theme(legend.position = "right") +
      labs(title = paste("End-locations –", hold2(), "(Cluster", cluster_id, ")"))
  })
  
  # 7) BARPLOT: cluster-fordeling i kampen (antal afleveringer pr. cluster i match_data)
  output$match_barplot_team1_clusters <- renderPlot({
    req(hold1())
    cluster_counts <- match_data() %>%
      filter(TEAMNAME == hold1()) %>%
      group_by(cluster) %>%
      summarise(antal = n(), .groups = "drop")
    
    ggplot(cluster_counts, aes(x = factor(cluster), y = antal)) +
      geom_col(fill = "#003366", alpha = 0.8) +
      labs(x = "Cluster", y = "Antal afleveringer",
           title = paste("Clusterfordeling for", hold1())) +
      theme_minimal()
  })
  
  output$match_barplot_team2_clusters <- renderPlot({
    req(hold2())
    cluster_counts <- match_data() %>%
      filter(TEAMNAME == hold2()) %>%
      group_by(cluster) %>%
      summarise(antal = n(), .groups = "drop")
    
    ggplot(cluster_counts, aes(x = factor(cluster), y = antal)) +
      geom_col(fill = "darkred", alpha = 0.8) +
      labs(x = "Cluster", y = "Antal afleveringer",
           title = paste("Clusterfordeling for", hold2())) +
      theme_minimal()
  })
}

shinyApp(ui, server)

#rsconnect::deployApp()