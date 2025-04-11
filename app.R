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

#setwd("/Users/lukasbachcouzy/Documents/DAL-Projects/2.semester/EksamenF25/Clusterpics/Cluster")


allpasses <- readRDS("allpasses.rds")
player_stats <- readRDS("player_stats.rds")
pass_data_scaled <- readRDS("pass_data_scaled.rds")
data.pca <- readRDS("data_pca.rds")
dftwss <- readRDS("dftwss.rds")


#### --- DATAINDSAMLING FRA MARIADB --- #####

pass_data <- allpasses %>% select(ANGLE, LENGTH, LOCATIONX, LOCATIONY, player_avgpass)


set.seed(123)
k_default <- 8
kmod <- kmeans(pass_data_scaled, centers = k_default, nstart = 10)
pass_data$cluster <- as.factor(kmod$cluster)




library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsoccer)
library(DT)
library(factoextra)

# --- CLUSTER BESKRIVELSER --- #
cluster_labels <- list(
  "1" = list(
    title = "Opbyggende afleveringer fra bagkæden",
    desc = "Afleveringerne i dette cluster starter dybt i egen banehalvdel og har karakter af at være opbyggende – med lange, flade afleveringer frem mod midtbanen eller det centrale angrebsområde. Typisk udført af forsvarsspillere med ansvar for spillets igangsættelse."
  ),
  "2" = list(
    title = "Skarpe fra højre midt",
    desc = "Afleveringerne udspringer fra højre midtbane og bliver splillet ind i modstanderens højre forsvarszone."
  ),
  "3" = list(
    title = "Fremadrettede afleveringer fra højre forsvar",
    desc = "Dette cluster dækker sikre, afleveringer fra højre forsvar mod centrale og venstre offensive områder. Typisk brugt til at sætte angreb i gang fra dybden – med stor præcision og god boldkontrol i opspillet."
  ),
  "4" = list(
    title = "Midtbanens distribution",
    desc = "Afleveringerne starter primært fra den centrale midtbane og fordeles symmetrisk til begge sider. Slutpositionerne samles i offensiv tredjedel med lav vinkel og høj præcision. Dette tyder på, at clusteret omfatter dybtliggende playmakere og midtbanespillere med ansvar for at kontrollere spillets rytme og fordele bolden bredt."
  ),
  "5" = list(
    title = "Venstresidens offensive indlæg",
    desc = "Afleveringerne i dette cluster starter overvejende fra den venstre midtbane og fløjen i den offensive halvdel. De ender typisk i eller omkring feltet, ofte fra indlægssituationer med høj vinkel og indadgående retning. Klyngen indikerer spillere, der ofte opererer som venstrekant eller wingback med indlægsansvar"
  ),
  "6" = list(
    title = "Højrerekantens indadgående oplæg",
    desc = "Afleveringerne i dette cluster starter konsekvent fra den højre offensive zone tæt på sidelinjen og bevæger sig ind mod feltet, hvilket er klassisk for en venstrekantspiller eller offensiv back, der søger at ramme medspillere i feltet. Mønstret afspejler et fokus på indlæg fra højresiden med høj præcision."
  ),
  "7" = list(
    title = "IVenstrefløjens direkte indspil",
    desc = "Afleveringerne i dette cluster udspringer primært fra venstresiden tæt ved midterlinjen og spilles direkte frem mod farezonen. Mønstret viser en tendens til at føre bolden op gennem banen uden store vinkler – en stil præget af effektivitet og direkte spil. Med høj præcision og kort længde tyder det på sikre afleveringer i en offensiv fase, ofte udført af venstrekant eller offensiv back."
  ),
  "8" = list(
    title = "Venstresidens dybe opspil",
    desc = "Afleveringerne starter dybt i venstre forsvar og midtbane og føres fremad mod centrale og højresidede områder tæt på feltet. Det indikerer en spilopbygning fra venstre side, hvor bolden skiftes over til modsat flanke eller centralt område for at åbne modstanderen. Med en moderat længde og høj præcision viser det et stabilt og velovervejet opspil."
  )
)

# --- UI --- #
ui <- dashboardPage(
  dashboardHeader(title = "K-Means Clustering Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Info", tabName = "info", icon = icon("info-circle")),
      menuItem("Cluster Explorer", tabName = "explorer", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "info",
              box(width = 12, title = "Velkommen", status = "primary", solidHeader = TRUE,
                  HTML("<h3>Analyse af afleveringsmønstre i Superligaen</h3><br><p>Vi har udført en clustering analyse, som vil sige at vi har gennem afleveringsdata skabt grupper af spiller, som har samme afleveringsmønstre.<br>Denne applikation skal hjælpe modtageren med at skabe et bedre overblik over hvilke typer afleveringer bliver skabt af de forskellige spillere i <strong>Superligaen</strong>.<br> Afleveringsdataen er baseret på sæsonerne 23/24 og en stor del af 24/25.<br><br><strong>Under Cluster explorer ser vi vores analyse</strong>, hvor vi via en dropdown kan vælge at studere de forskellige grupper af spillere, gennem heatmaps for hele gruppen, og en beskrivende overskrift til lettere formodling. </p>")
              )),
      tabItem(tabName = "explorer",
              fluidRow(
                box(width = 3, title = "Indstillinger", status = "info", solidHeader = TRUE,
                    selectInput("cluster_choice", "Vælg cluster:", choices = 1:8),
                    htmlOutput("cluster_info_box")
                ),
                box(
                  width = 9, 
                  title = textOutput("heatmap_box_title"),
                  status = "primary", 
                  solidHeader = TRUE,
                  fluidRow(
                    column(6, plotOutput("heatmap_start")),
                    column(6, plotOutput("heatmap_end"))
                  )
                )
              ), 
              fluidRow(
                box(width = 12, title = "Spillerinformation i valgt cluster", status = "info", solidHeader = TRUE,
                    DTOutput("cluster_player_table")
                )
              ),
              fluidRow(
                box(width = 12, title = "3D Visualisering af spillere i valgt cluster", status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_3d_xplorer", height = "500px"))
              )
      )
    )
  )
)

# --- SERVER --- #
server <- function(input, output, session) {
  
  kmeans_model_reactive <- reactive({
    kmeans(pass_data_scaled, centers = input$k, nstart = 10)
  })
  
  selected_cluster <- reactive({
    as.character(input$cluster_choice)
  })
  
  output$heatmap_start <- renderPlot({
    ggplot(allpasses %>% filter(cluster == selected_cluster())) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(aes(x = LOCATIONX, y = LOCATIONY), alpha = 0.7, contour_var = "ndensity") +
      theme_pitch() +
      labs(title = "Pass start") +
      scale_fill_viridis_d(option = "magma")
  })
  
  output$heatmap_box_title <- renderText({
    cluster_id <- selected_cluster()
    paste0("Heatmaps – Cluster ", cluster_id, ": ", cluster_labels[[cluster_id]]$title)
  })
  
  output$heatmap_end <- renderPlot({
    ggplot(allpasses %>% filter(cluster == selected_cluster())) +
      annotate_pitch(colour = "white", fill = "gray") +
      stat_density_2d_filled(aes(x = POSSESSIONENDLOCATIONX, y = POSSESSIONENDLOCATIONY), alpha = 0.7, contour_var = "ndensity") +
      theme_pitch() +
      labs(title = "Pass end") +
      scale_fill_viridis_d(option = "magma")
  })
  
  output$cluster_player_table <- renderDT({
    cluster_id <- as.numeric(input$cluster_choice)
    df <- player_stats %>%
      filter(main_cluster == cluster_id) %>%
      select(SHORTNAME, matches_played, total_passes,
             avg_passes_per_match, avg_pass_angle, pass_acc) %>%
      arrange(desc(total_passes))
    
    datatable(df,
              options = list(pageLength = 10),
              colnames = c("Spiller", "Kampe", "Total afleveringer",
                           "Afleveringer/kamp", "Gns vinkel", "Præcision (%)"))
  })
  
  output$cluster_info_box <- renderUI({
    cluster_id <- selected_cluster()
    cluster_data <- allpasses %>% filter(cluster == cluster_id)
    
    avg_length <- round(mean(cluster_data$LENGTH, na.rm = TRUE), 1)
    avg_angle <- round(mean(cluster_data$ANGLE, na.rm = TRUE), 1)
    avg_player_passes <- round(mean(cluster_data$player_avgpass, na.rm = TRUE), 1)
    avg_acc <- round(mean(cluster_data$ACCURATE, na.rm = TRUE) * 100, 1)
    total_passes <- nrow(cluster_data)
    
    HTML(paste0(
      "<div style='background:#f5f5f5;padding:10px;border-radius:5px;margin-top:10px;'>",
      "<strong>Cluster ", cluster_id, " info</strong><br>",
      "<b>Gennemsnitlig længde:</b> ", avg_length, "<br>",
      "<b>Gns. vinkel:</b> ", avg_angle, "<br>",
      "<b>Gns. spillerens passes pr kamp:</b> ", avg_player_passes, "<br>",
      "<b>Antal pass i cluster:</b> ", total_passes, "<br>",
      "<b>Gennemsnitlig præcision:</b> ", avg_acc, "%",
      "</div>"
    ))
  })
  
  output$plot_3d_xplorer <- renderPlotly({
    cluster_id <- as.numeric(input$cluster_choice)
    
    cluster_df <- player_stats %>% filter(main_cluster == cluster_id)
    
    if (nrow(cluster_df) == 0) return(plotly_empty())
    
    plot_ly(
      data = cluster_df,
      x = ~avg_pass_length,
      y = ~avg_pass_angle,
      z = ~pass_acc,
      type = "scatter3d",
      mode = "markers",
      color = ~as.factor(main_cluster),
      text = ~paste("Spiller:", SHORTNAME,
                    "<br>Gns. længde:", round(avg_pass_length,1),
                    "<br>Vinkel:", round(avg_pass_angle,1),
                    "<br>Præcision:", round(pass_acc,1), "%"),
      hoverinfo = "text"
    ) %>%
      layout(scene = list(
        xaxis = list(title = "Gns. længde"),
        yaxis = list(title = "Gns. vinkel"),
        zaxis = list(title = "Præcision (%)")
      ))
  })
}

# --- KØR APPEN --- #
shinyApp(ui, server)

