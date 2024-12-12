# Load packages
library(shiny)
library(rjson)
library(shinythemes)
library(stringr)
library(ggplot2)
library(fmsb)

# Load data
json_file <- "vct-international.json"
data <- fromJSON(paste(readLines(json_file), collapse=""))
df <- do.call(rbind, lapply(data, as.data.frame))

# Data cleaning
colnames(df) <- c(
  "Player", "Team", "Rating", "Region",
  "Average Combat Score", "Kill Death Ratio", "Kill Assist Survive Traded",
  "Average Damage Per Round", "Kills Per Round", "Assists Per Round",
  "First Kills Per Round", "First Deaths Per Round", "Headshot Percentage",
  "Clutch Success Percentage", "Agent", "Player Category"
)

df <- df[, !colnames(df) %in% "Player Category"]

df$Agent <- str_to_title(df$Agent)

numeric_columns <- c(
  "Rating", "Average Combat Score", "Kill Death Ratio", 
  "Average Damage Per Round", "Kills Per Round", "Assists Per Round", 
  "First Kills Per Round", "First Deaths Per Round"
)

percentage_columns <- c(
  "Kill Assist Survive Traded", "Headshot Percentage", "Clutch Success Percentage"
)

df[numeric_columns] <- lapply(df[numeric_columns], as.numeric)

df[percentage_columns] <- lapply(df[percentage_columns], function(x) {
  as.numeric(sub("%", "", x)) / 100
})

# Check data structure
str(df)
# write.csv(df, "C:/Users/Chloe Lim/Downloads/filename.csv", row.names = FALSE)


# R Shiny
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Player Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", label = "Select Region", choices = unique(df$Region)),
      selectInput("team", label = "Select Team", choices = NULL),
      selectInput("player", label = "Select Player", choices = NULL)
    ),
    mainPanel(
      # Section titles and outputs
      tags$div(
        tags$h4("Player Performance Data"),
        tableOutput("oneStatRow")
      ),
      tags$br(),
      tags$div(
        tags$h4("Agents Played"),
        uiOutput("agents")
      ),
      tags$br(),
      tags$div(
        tags$h4("Rankings for All Metrics"),
        uiOutput("ranking")
      ),
      tags$br(),
      tags$div(
        tags$h4("Performance Metrics Comparison"),
        plotOutput("performancePlot")
      ),
      tags$br(),
      tags$div(
        tags$h4("Player Radar Chart"),
        plotOutput("radarChart")
      ),
    )
  )
)


server <- function(input, output, session) {
  # Region Selection
  observeEvent(input$region, {
    selected_region_teams <- df[df$Region == input$region, "Team"]
    updateSelectInput(session, "team", choices = unique(selected_region_teams))
    updateSelectInput(session, "player", choices = NULL)
  })
  
  # Team Selection
  observeEvent(input$team, {
    selected_team_players <- df[df$Team == input$team, "Player"]
    updateSelectInput(session, "player", choices = unique(selected_team_players))
  })
  
  # Filtered Data
  filteredData <- reactive({
    req(input$region, input$team, input$player)
    df[df$Region == input$region & df$Team == input$team & df$Player == input$player, ]
  })
  
  # Statistics Section
  output$oneStatRow <- renderTable({
    req(filteredData())
    filteredData()[1, !colnames(filteredData()) %in% "Agent", drop = FALSE]
  })
  
  # Agents Section
  output$agents <- renderUI({
    req(filteredData())
    unique_agents <- unique(filteredData()$Agent)
    tags$p(HTML(paste("Agents Played:", paste(unique_agents, collapse = ", "))))
  })
  
  # Ranking
  output$ranking <- renderUI({
    req(filteredData())
    player_data <- filteredData()[1, ]
    
    # Ensure only one row per player
    df_unique <- df[!duplicated(df$Player), ]
    
    metrics <- c(
      "Average Combat Score", "Kill Death Ratio", "Kill Assist Survive Traded",
      "Average Damage Per Round", "Kills Per Round", "Assists Per Round",
      "First Kills Per Round", "First Deaths Per Round", "Headshot Percentage",
      "Clutch Success Percentage"
    )
    
    # Total players for consistent rankings
    total_players <- nrow(df_unique)
    
    ranks <- c()
    for (metric in metrics) {
      metric_values <- as.numeric(df_unique[[metric]])
      
      metric_ranks <- rank(-metric_values, ties.method = "min")
      
      player_index <- which(df_unique$Player == player_data$Player)
      if (length(player_index) > 0) {
        player_rank <- metric_ranks[player_index[1]]
      } else {
        player_rank <- NA
      }
      
      ranks <- c(ranks, player_rank)
    }

    rankings <- data.frame(
      Metric = metrics,
      Rank = ranks,
      TotalPlayers = total_players
    )
    
    rankings_text <- paste(
      paste0(
        rankings$Metric, ": Rank ", rankings$Rank, "/", rankings$TotalPlayers
      ),
      collapse = "<br>"
    )
    HTML(rankings_text)
  })
  
  # Performance Plot Section
  output$performancePlot <- renderPlot({
    req(filteredData())
    player_data <- filteredData()[1, ]
    
    global_avg <- data.frame(
      Metric = c("Average Combat Score", "Kills Per Round", "Assists Per Round"),
      Value = c(
        mean(as.numeric(df$`Average Combat Score`), na.rm = TRUE),
        mean(as.numeric(df$`Kills Per Round`), na.rm = TRUE),
        mean(as.numeric(df$`Assists Per Round`), na.rm = TRUE)
      ),
      Type = "Global Average"
    )
    
    player_metrics <- data.frame(
      Metric = c("Average Combat Score", "Kills Per Round", "Assists Per Round"),
      Value = c(
        as.numeric(player_data$`Average Combat Score`),
        as.numeric(player_data$`Kills Per Round`),
        as.numeric(player_data$`Assists Per Round`)
      ),
      Type = paste(player_data$Player, "Performance")
    )
    
    combined_metrics <- rbind(global_avg, player_metrics)
    
    ggplot(combined_metrics, aes(x = Type, y = Value, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Metric, scales = "free_y") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      labs(
        title = paste("Performance Metrics Comparison for", player_data$Player), 
        x = NULL, 
        y = "Value",
        fill = "Legend"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold")
      )
  })
  
  # Radar Chart Section
  output$radarChart <- renderPlot({
    req(filteredData())
    player_data <- filteredData()[1, ]
    
    kill_death_ratio <- as.numeric(player_data$`Kill Death Ratio`)
    headshot_percentage <- as.numeric(sub("%", "", player_data$`Headshot Percentage`)) / 100
    clutch_success <- as.numeric(sub("%", "", player_data$`Clutch Success Percentage`)) / 100
    
    metrics <- data.frame(
      KillDeathRatio = kill_death_ratio / max(kill_death_ratio, 1),
      HeadshotPercentage = headshot_percentage,
      ClutchSuccess = clutch_success
    )
    metrics <- rbind(rep(1, ncol(metrics)), rep(0, ncol(metrics)), metrics)
    axis_labels <- c("Kill/Death Ratio", "Headshot %", "Clutch Success %")
    corrected_axis_labels <- c("0%", "25%", "50%", "75%", "100%")
    
    radarchart(metrics, axistype = 1,
               pcol = "blue", pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 2,
               cglcol = "grey", cglty = 1, axislabcol = "black", cglwd = 0.8,
               vlcex = 0.8, vlabels = axis_labels,
               caxislabels = corrected_axis_labels,  # Corrected axis labels
               title = paste("Radar Chart for", player_data$Player))
    
  })
}

shinyApp(ui, server)
