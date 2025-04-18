library(ggplot2)
library(ggsoccer)
library(dplyr)
library(purrr)
library(sf)
library(scales)
library(geometry)
library(units)
library(jsonlite)
library(tidyr)
library(logr)
library(futile.logger)
library(mongolite)
library(stringr)
library(plotly)
library(tibble)
library(StatsBombR)
#setwd("/Users/lukasbachcouzy/Documents/DAL-Projects/2.semester/EksamenF25/Tracking/")
tracking_meta <- fromJSON("tracking_meta.json")
tracking <- fromJSON("tracking.json")

tracking <- tracking %>%
  mutate(teamname = case_when(
    lastTouch == "away" ~ "OB",
    lastTouch == "home" ~ "VB",
    TRUE ~ "fejl"
  ))

rescale_coords <- function(df, x_col, y_col, from_x = c(-52.5, 52.5), from_y = c(-44, 44)) {
  df %>% mutate(
    x = rescale(.data[[x_col]], to = c(0, 120), from = from_x),
    y = rescale(.data[[y_col]], to = c(0, 80), from = from_y)
  )
}

extract_positions <- function(players_df, team_name) {
  if (is.null(players_df) || nrow(players_df) == 0) return(data.frame())
  players_df %>%
    mutate(
      x = map_dbl(xyz, ~ if (length(.) >= 1) as.numeric(.[[1]]) else NA),
      y = map_dbl(xyz, ~ if (length(.) >= 2) as.numeric(.[[2]]) else NA),
      team = team_name,
      id = if ("playerId" %in% colnames(players_df)) players_df$playerId else NA
    ) %>%
    select(team, id, x, y)
}

create_trapezoid_polygon <- function(ball, player, width_start = 0.5, width_end = 1, label = "A") {
  dx <- player$x - ball$x
  dy <- player$y - ball$y
  angle <- atan2(dy, dx)
  perp <- pi / 2
  
  p1 <- c(ball$x + width_start * cos(angle + perp), ball$y + width_start * sin(angle + perp))
  p2 <- c(player$x + width_end * cos(angle + perp), player$y + width_end * sin(angle + perp))
  p3 <- c(player$x + width_end * cos(angle - perp), player$y + width_end * sin(angle - perp))
  p4 <- c(ball$x + width_start * cos(angle - perp), ball$y + width_start * sin(angle - perp))
  
  tibble(
    x = c(p1[1], p2[1], p3[1], p4[1], p1[1]),
    y = c(p1[2], p2[2], p3[2], p4[2], p1[2]),
    group = label
  )
}

plot_tracking_frame <- function(tracking, tracking_meta, frame_index = 1) {
  frame_data <- tracking[frame_index, ]
  home_df <- extract_positions(frame_data$homePlayers[[1]], "VB")
  away_df <- extract_positions(frame_data$awayPlayers[[1]], "OB")
  
  ball_df <- tryCatch({
    coords <- unlist(frame_data$ball$xyz)
    if (length(coords) >= 2) tibble(x = as.numeric(coords[1]), y = as.numeric(coords[2])) else NULL
  }, error = function(e) NULL)
  
  all_players <- bind_rows(home_df, away_df)
  meta_df <- bind_rows(tracking_meta$homePlayers, tracking_meta$awayPlayers)
  
  all_players <- left_join(all_players, meta_df, by = c("id" = "ssiId")) %>%
    mutate(label = ifelse(!is.na(name), name, substr(id, 1, 5)))
  
  all_players <- rescale_coords(all_players, "x", "y")
  if (!is.null(ball_df)) {
    ball_df <- rescale_coords(ball_df, "x", "y")
  }
  
  triangle_list <- list()
  highlight_idx <- NULL
  
  if (!is.null(ball_df)) {
    last_touch <- frame_data$lastTouch[[1]]
    modtagere <- if (last_touch == "home") home_df else if (last_touch == "away") away_df else NULL
    modstandere <- if (last_touch == "home") away_df else if (last_touch == "away") home_df else NULL
    
    if (!is.null(modtagere)) {
      modtagere <- rescale_coords(modtagere, "x", "y")
      modstandere <- rescale_coords(modstandere, "x", "y")
      max_x <- if (last_touch == "home") -Inf else Inf
      
      for (i in 1:nrow(modtagere)) {
        m <- modtagere[i, ]
        poly_df <- create_trapezoid_polygon(ball_df, m, 0.5, 1, label = i)
        
        poly_path <- sf::st_polygon(list(cbind(poly_df$x, poly_df$y)))
        blocked <- any(sapply(1:nrow(modstandere), function(j) {
          pt <- sf::st_point(c(modstandere$x[j], modstandere$y[j]))
          sf::st_intersects(sf::st_sfc(poly_path), sf::st_sfc(pt), sparse = FALSE)[1, 1]
        }))
        
        poly_df$blocked <- blocked
        poly_df$highlight <- FALSE
        triangle_list[[i]] <- poly_df
        
        if (!blocked) {
          if (last_touch == "home" && m$x > max_x) {
            max_x <- m$x; highlight_idx <- i
          } else if (last_touch == "away" && m$x < max_x) {
            max_x <- m$x; highlight_idx <- i
          }
        }
      }
      
      all_polygons <- bind_rows(triangle_list)
      if (!is.null(highlight_idx)) {
        all_polygons$highlight[all_polygons$group == highlight_idx] <- TRUE
      }
    }
  }
  
  ggplot() +
    annotate_pitch(dimensions = pitch_statsbomb, fill = "#60A561", colour = "white") +
    theme_pitch() +
    theme(panel.background = element_rect(fill = "#60A561")) +
    {if (exists("all_polygons")) geom_polygon(data = all_polygons,
                                              aes(x = x, y = y, group = group,
                                                  fill = ifelse(highlight, "green", ifelse(blocked, "red", "blue")),
                                                  alpha = ifelse(highlight, 0.4, ifelse(blocked, 0.2, 0.1))), color = NA)} +
    geom_point(data = all_players, aes(x = x, y = y, color = team), size = 4) +
    scale_color_manual(values = c("OB" = "blue1", "VB" = "red1")) +
    geom_text(data = all_players, aes(x = x, y = y, label = label), size = 5.5) +
    {if (!is.null(ball_df)) geom_point(data = ball_df, aes(x = x, y = y), fill = "white", color = "black", shape = 21, size = 4)} +
    scale_fill_identity() +
    scale_alpha_identity() +
    coord_cartesian(xlim = c(0, 120), ylim = c(0, 80)) +
    theme(aspect.ratio = 2 / 3) +
    scale_y_reverse() +
    ggtitle(paste0("Frame ", frame_data$frameIdx))
}
#### active play ####
activeplay <- tracking[tracking$live == TRUE, ]

bbsidelse <- tibble(
  hold = c("OB", "VB"),
  antal_frames = c(46430, 37362)
) %>%
  mutate(
    total = sum(antal_frames),
    procent = antal_frames / total
  )

ggplot(bbsidelse, aes(x = 1, y = procent, fill = hold)) +
  geom_col(width = 0.4) +
  coord_flip() +
  scale_fill_manual(values = c("OB" = "blue", "VB" = "red")) +
  geom_text(aes(label = paste0(round(procent * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 6) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#002b7f", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

#### SHINY ####
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(" 
      body { background-color: #002b7f; }
      .navbar { background-color: #002b7f !important; }
      .navbar-default .navbar-brand { color: white !important; font-weight: bold; }
      .btn-primary { background-color: #ffd700; border-color: #ffd700; color: black; }
      .btn-primary:hover { background-color: #ffc300; border-color: #ffc300; color: black; }
      .control-buttons { text-align: center; padding: 30px; }
      .info-box {
        background-color: #fffbea;
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 15px;
        margin-top: 15px;
        font-size: 15px;
      }
    "))
  ),
  
  navbarPage("Trackinganalyse",
             tabPanel("Visualisering",
                      sidebarLayout(
                        sidebarPanel(
                          div(class = "control-buttons",
                              actionButton("prev", "<< Forrige", class = "btn btn-primary btn-lg"),
                              actionButton("næste", "Næste >>", class = "btn btn-primary btn-lg"),
                              br(), br(),
                              sliderInput("frame", "Vælg frame:", min = 1, max = nrow(tracking), value = 1, step = 100),
                              actionButton("play", "Afspil", class = "btn btn-primary btn-lg"),
                              actionButton("stop", "Stop", class = "btn btn-primary btn-lg")
                          ),
                          div(class = "info-box",
                              HTML(" <strong>Brugsanvisning:</strong><br>
                Brug knapperne til at navigere frem og tilbage mellem frames.<br>
                <br>
                'Afspil' starter automatisk afspilning af hver 25. frame.<br>
                <br>
                 Du kan bruge slideren til at hoppe mellem frames.<br>
                <br>
                <strong>Afleveringsindikationer</strong><br>
                <br>
                De grønne polygoner symboliserer den spiller som er længst fremme på modstanders bane og fri. <br>
                <br>
                De røde poligoner symboliserer at der er en modspiller mellem. <br>
                <br>
                De blå poligoner symboliserer frie spillere, men ikke dem som er længst fremme på banen.")
                          )
                        ),
                        mainPanel(
                          plotOutput("plot", height = "700px"),
                          div(style = "background-color: #fffbea; border: 1px solid #ccc; border-radius: 5px;
               padding: 15px; margin-top: 20px; font-size: 15px;text-align: center;",
                              HTML("<strong>'Live' boldbesidelse:</strong><br>
                  Blå viser OB's boldbesiddelse, Rød viser VB's."),  
                              plotOutput("possession_plot", height = "80px")
                          )
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  current_frame <- reactiveVal(1)
  playing <- reactiveVal(FALSE)
  timer <- reactiveTimer(1500)  # kortere interval = glattere animation
  
  observe({
    if (playing()) {
      timer()
      isolate({
        next_frame <- current_frame() + 15  # mindre spring giver mere glidende bevægelse
        if (next_frame <= nrow(tracking)) {
          current_frame(next_frame)
          updateSliderInput(session, "frame", value = next_frame)
        } else {
          playing(FALSE)
        }
      })
    }
  })
  
  observeEvent(input$frame, {
    current_frame(input$frame)
  })
  
  observeEvent(input$næste, {
    new_val <- min(current_frame() + 1, nrow(tracking))
    current_frame(new_val)
    updateSliderInput(session, "frame", value = new_val)
  })
  
  observeEvent(input$prev, {
    new_val <- max(current_frame() - 1, 1)
    current_frame(new_val)
    updateSliderInput(session, "frame", value = new_val)
  })
  
  observeEvent(input$play, {
    playing(TRUE)
  })
  
  observeEvent(input$stop, {
    playing(FALSE)
  })
  
  output$plot <- renderPlot({
    plot_tracking_frame(tracking, tracking_meta, frame_index = current_frame())
  })
  
  output$possession_plot <- renderPlot({
    tracking_live <- tracking[1:current_frame(), ] %>%
      filter(live == TRUE) %>%
      mutate(teamname = case_when(
        lastTouch == "home" ~ "VB",
        lastTouch == "away" ~ "OB",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(teamname)) %>%
      count(teamname, name = "antal") %>%
      mutate(total = sum(antal),
             procent = round(antal / total * 100))
    
    ggplot(tracking_live, aes(x = 1, y = procent, fill = teamname)) +
      geom_col(width = 2) +
      coord_flip() +
      geom_text(aes(label = paste0(procent, "%")), 
                position = position_stack(vjust = 0.5), color = "white", size = 6) +
      scale_fill_manual(values = c("OB" = "blue1", "VB" = "red1")) +
      theme_void() +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)

