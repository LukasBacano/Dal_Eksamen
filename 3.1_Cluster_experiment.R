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

#setwd("/Users/lukasbachcouzy/Documents/DAL-Projects/2.semester/EksamenF25/Clusterpics/experiment/")


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



# --- UI --- #
ui <- dashboardPage(
  dashboardHeader(title = "K-Means Clustering Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Info", tabName = "info", icon = icon("info-circle")),
      menuItem("Eksperiment", tabName = "eksperiment", icon = icon("atom"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "info",
              box(width = 12, title = "Velkommen", status = "primary", solidHeader = TRUE,
                  HTML("<h3>Analyse af afleveringsmønstre i Superligaen</h3><br><p>Vi har udført en clustering analyse, som vil sige at vi har gennem afleveringsdata skabt grupper af spiller, som har samme afleveringsmønstre.<br>Denne applikation skal hjælpe modtageren med at skabe et bedre overblik over hvilke typer afleveringer bliver skabt af de forskellige spillere i <strong>Superligaen</strong>.<br> Afleveringsdataen er baseret på sæsonerne 23/24 og en stor del af 24/25.<br><br><strong>Under Cluster explorer ser vi vores analyse</strong>, hvor vi via en dropdown kan vælge at studere de forskellige grupper af spillere, gennem heatmaps for hele gruppen, og en beskrivende overskrift til lettere formodling. </p>")
              )),
      tabItem(tabName = "eksperiment",
              fluidRow(
                box(title = "Cluster antal", width = 12,
                    sliderInput("k", "Antal Clusters:", min = 1, max = 15, value = 4, step = 1)
                )
              ),
              fluidRow(
                box(title = "PCA Plot", width = 6, plotOutput("pca_plot")),
                box(title = "3D Plot", width = 6,
                    selectInput("x_var", "X Variabel", choices = names(player_stats)[3:10], selected = "total_passes"),
                    selectInput("y_var", "Y Variabel", choices = names(player_stats)[3:10], selected = "avg_pass_length"),
                    selectInput("z_var", "Z Variabel", choices = names(player_stats)[3:10], selected = "avg_pass_angle"),
                    plotlyOutput("plot_3d")
                )
              ),
              fluidRow(
                box(title = "Barplot - Main Cluster", width = 6, plotOutput("bar_plot")),
                box(title = "Elbow Plot (eksperiment)", width = 6, plotOutput("elbow_plot"))
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
  
  output$elbow_plot <- renderPlot({
    ggplot(dftwss, aes(x = k, y = twss)) +
      geom_line() + geom_point(color = "blue") +
      labs(title = "Eksperiment: Elbow", x = "K", y = "Tot. Within SS")
  })
  
  output$bar_plot <- renderPlot({
    model <- kmeans_model_reactive()
    df <- data.frame(cluster = as.factor(model$cluster))
    
    ggplot(df, aes(x = cluster)) +
      geom_bar(fill = "steelblue") +
      labs(x = "Cluster", y = "Antal observationer") +
      theme_minimal()
  })
  
  output$plot_3d <- renderPlotly({
    model <- kmeans_model_reactive()
    df <- as.data.frame(pass_data_scaled)
    df$cluster <- as.factor(model$cluster)
    
    plot_ly(
      data = df,
      x = ~LENGTH,
      y = ~ANGLE,
      z = ~player_avgpass,
      type = "scatter3d",
      mode = "markers",
      color = ~cluster,
      hoverinfo = "text"
    )
  })
  
  output$pca_plot <- renderPlot({
    model <- kmeans_model_reactive()
    df <- as.data.frame(pass_data_scaled)
    pca_res <- princomp(df)
    pca_scores_temp <- as.data.frame(pca_res$scores[, 1:2])
    pca_scores_temp$cluster <- as.factor(model$cluster)
    
    ggplot(pca_scores_temp, aes(x = Comp.1, y = Comp.2, color = cluster)) +
      geom_point(alpha = 0.6) +
      labs(title = paste("PCA med", input$k, "klynger")) +
      theme_minimal()
  })
  
  output$plot_3d <- renderPlotly({
    # Forbered data: kun de variable vi skal bruge
    df <- player_stats %>%
      select(SHORTNAME, matches_played:pass_acc)
    
    # Scale data til clustering
    df_scaled <- scale(df %>% select(matches_played:pass_acc))
    
    # Reaktiv kmeans baseret på input$k
    model <- kmeans(df_scaled, centers = input$k, nstart = 10)
    
    # Tilføj cluster til df
    df$cluster <- as.factor(model$cluster)
    
    # Plot med input-valgte variabler
    plot_ly(
      data = df,
      x = ~get(input$x_var),
      y = ~get(input$y_var),
      z = ~get(input$z_var),
      type = "scatter3d",
      mode = "markers",
      color = ~cluster,
      text = ~paste("Spiller:", SHORTNAME, "<br>Cluster:", cluster),
      hoverinfo = "text"
    ) %>%
      layout(scene = list(
        xaxis = list(title = input$x_var),
        yaxis = list(title = input$y_var),
        zaxis = list(title = input$z_var)
      ))
  })
  
}

# --- KØR APPEN --- #
shinyApp(ui, server)

