Sys.setenv(VROOM_CONNECTION_SIZE = 131072 * 10)
data_dir <- "Data"  

library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(tidyr)
    
# DATA COLLECTION: Joel Embiid's Scoring Evolution

# collecting PPG data from 2017-2024
years <- 2017:2024
top20_list <- list()

# collecting top 150 players in PPG for each year
for (year in years) {
  file_path <- file.path(data_dir, paste0(year, ".xlsx"))
    
  
  df <- read_excel(file_path) %>%
    filter(!is.na(Player), Player != "Player") %>%
    mutate(Season = as.character(year)) %>%
    group_by(Player) %>%
    slice_max(order_by = G, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(PTS)) %>%
    slice(1:150)
  
  top20_list[[as.character(year)]] <- df
}

# combine all seasons
top_all <- bind_rows(top20_list)

# only include players with at least 4 years of data
players_with_4_years <- top_all %>%
  group_by(Player) %>%
  summarise(years_played = n(), avg_ppg = mean(PTS, na.rm = TRUE)) %>%
  filter(years_played >= 4) %>%
  arrange(desc(avg_ppg)) %>%
  slice(1:15)

# filter main data using 4-year players
top15_filtered <- top_all %>%
  filter(Player %in% players_with_4_years$Player)

# DATA COLLECTION: Joel Embiid vs Other Scorers in 2024

# load 2024 top 100 PPG leaders
PPG_2024 <- read_excel(file.path(data_dir, "2024.xlsx")) %>%
  filter(!is.na(Player), Player != "Player") %>%
  group_by(Player) %>%
  slice_max(order_by = G, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(PTS)) %>%
  slice(1:100)

embiid_2024 <- PPG_2024 %>% filter(Player == "Joel Embiid")

# DATA COLLECTION: Joel Embiid Shot Diet Bar Graph 2024

# loading the relevant datasets
fg_data <- read_excel(file.path(data_dir, "FG by Distance.xlsx")) %>%
  filter(Season == "2024-25")

fga_data <- read_excel(file.path(data_dir, "FGA by Distance.xlsx")) %>%
  filter(Season == "2024-25")

fg_long <- fg_data %>%
  select(Season, `0-3`, `3-10`, `10-16`, `16-3P`, `3P`) %>%
  pivot_longer(cols = -Season, names_to = "Zone", values_to = "FG%")

fga_long <- fga_data %>%
  select(Season, `0-3`, `3-10`, `10-16`, `16-3P`, `3P`) %>%
  pivot_longer(cols = -Season, names_to = "Zone", values_to = "FGA%")

# combine the two
combined <- left_join(fga_long, fg_long, by = c("Season", "Zone"))

# UI

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  h1("Joel Embiid: The Story of The Greatest Scorer in the NBA"),
fluidRow(
  column(
    width = 2,
    tags$img(src = "embiid.jpg", height = "250px", style = "margin-top: 20px; max-width: 100%;")
  ),
  column(
    width = 5,
    h3("Welcome to the Joel Embiid statistical analysis interactive dashboard!"),
    div(class = "text", "Joel Embiid will end his career as one of the most prolific scorers in the history and is the undisputed best at that ability in the league today. This dashboard aims to present you with a number of visualisations that demonstrate his impressive scoring prowess. The following visualisations include a look at his scoring evolution, his unmatched scoring during the 2024 season in particular, and his shot composition and diet during that season.")
  )
),

  tabsetPanel(
    id = "tabs",

    tabPanel("Joel Embiid's Scoring Evolution", value = "evolution",
      sidebarLayout(
        sidebarPanel(
          selectInput("highlight_player", "Select a Player to Highlight:", 
                      choices = sort(unique(top15_filtered$Player)),
                      selected = "Joel Embiid"),
          
          sliderInput("year_range", "Select Year Range:",
                      min = 2017, max = 2024,
                      value = c(2017, 2024),
                      step = 1,
                      sep = "")
        ),
        mainPanel(
        tagList(
        plotlyOutput("ppg_line_plot", height = "600px"),
        br(),
        uiOutput("dynamic_linegraph_text"),
        p("This line graph shows how Joel Embiid's scoring has evolved over the years compared to Top 15 scorers throughout Embiid's entire career (2017-2024) with at least four seasons of top-tier performance. Besides a drop in scoring during the COVID-19 seasoon, his scoring has been steadily increasing, culminating in a league-leading 34.7 PPG. You can select a player to compare alongside Embiid and adjust to slider to whatever year range you wish.")
  )
)

      )
    ),

    tabPanel("Joel Embiid vs Other Scorers in 2024", value = "boxplot",
  fluidPage(
    plotlyOutput("box_plot_2024", height = "600px"),
    br(),
    checkboxInput("show_jitter", label = tags$strong("Show Individual Player Dots (Jitter)?"), value = TRUE),
    p("This box plot visualises the distribution of points per game among the top 100 scorers in the 2024 NBA season using a box and whisker plot. Even among the Top 100 Scorers in the league, Embiid is an outlier sitting at more than 24 PPG higher than the medium of 19.95. You can choose to enable jitter (individual player dots) to see specific player PPG values. If you enable jitter, Joel Embiid is highlighted in blue to emphasize his dominant scoring relative to his peers. If you choose to disabled jitter, it shows two outlier datta points at the top of the graph and those represent Luka Doncic and Joel Embiid, the latter being the highest data point.")
  )
),

    tabPanel("Joel Embiid Shot Diet", value = "heatmap",
    fluidPage(
    
    plotlyOutput("shooting_heatmap", height = "600px"),
    br(),
    radioButtons("zone_view", "Select Shot Zone View:",
                 choices = c("Detailed Shot Zones" = "detailed",
                             "2P Combined vs 3P" = "combined"),
                 selected = "detailed"),
    p("This bar chart displays Joel Embiid's shot selection in the 2024 season. The x-axis shows the shot distance zones, the bar height indicates the frequency of attempts, and the color shows the field goal percentage. This shows Embiid's effectiveness in closer ranges in constrast to his down year efficiency wise at the 3P line. The Select Shot Zone View allows you to view the bar graph as 2P vs 3P, or have the 2P be seperated into its own distinct zones for a more extensive look. Furthermore, the bar chart colours have been filled in with colorblind-friendly Reds-Yellow-Green.")
  
  )
)

  )
)

# SERVER

server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$highlight_player)
    top15_filtered %>%
      filter(as.numeric(Season) >= input$year_range[1],
             as.numeric(Season) <= input$year_range[2]) %>%
      mutate(LineType = case_when(
        Player == "Joel Embiid" ~ "Embiid",
        Player == input$highlight_player ~ "Selected",
        TRUE ~ "Others"
      ))
  })

  # dynamic subtitles for line graph 
  output$dynamic_linegraph_text <- renderUI({
    req(input$highlight_player, input$year_range)
    
    if (input$highlight_player == "Joel Embiid") {
      HTML(paste0("<em>Showing Joel Embiid's scoring progression from ", 
                  input$year_range[1], " to ", input$year_range[2], ".</em>"))
    } else {
      HTML(paste0("<em>Comparing Joel Embiid to ", input$highlight_player, 
                  " from ", input$year_range[1], " to ", input$year_range[2], ".</em>"))
    }
  })
  
  # LINE GRAPH
  output$ppg_line_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = as.numeric(Season), y = PTS, group = Player,
                                     color = LineType, text = paste("Player:", Player, "<br>PPG:", PTS))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Embiid" = "blue", "Selected" = "red", "Others" = "gray80")) +
      labs(
        title = "Line Graph of PPG Evolution of Top 15 Scorers (2017–2024)",
        x = "Season",
        y = "Points Per Game (PPG)",
        color = "Line Type"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })

 # BOX PLOT
output$box_plot_2024 <- renderPlotly({
  PPG_2024_mod <- PPG_2024 %>%
    mutate(Is_Embiid = ifelse(Player == "Joel Embiid", "Joel Embiid", "Other Players"))

  p <- ggplot(PPG_2024_mod, aes(x = "All Players", y = PTS)) +
    geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5)

  if (input$show_jitter) {
    p <- p + geom_jitter(aes(
      text = paste("Player:", Player, "<br>PPG:", round(PTS, 1)),
      color = Is_Embiid
    ),
    width = 0.2, size = 2, alpha = 0.7)
  }

  p <- p +
    scale_color_manual(values = c("Joel Embiid" = "blue", "Other Players" = "gray50")) +
    labs(
      title = "Box Plot of PPG of Top 100 Scorers (2024 season)",
      x = "",
      y = "Points Per Game (PPG)",
      color = "Player"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  ggplotly(p, tooltip = "text")
})
  
 # BAR GRAPH

# more descriptive zone labels
combined <- combined %>%
  mutate(
    Zone_Label = case_when(
      Zone == "0-3" ~ "Paint (0-3 ft)",
      Zone == "3-10" ~ "Close Range (3-10 ft)",
      Zone == "10-16" ~ "Mid Range (10-16 ft)",
      Zone == "16-3P" ~ "Long 2s (16ft-3P)",
      Zone == "3P" ~ "Three Pointers",
      TRUE ~ Zone
    ),
    Zone_Label = factor(Zone_Label, levels = c(
      "Paint (0-3 ft)",
      "Close Range (3-10 ft)",
      "Mid Range (10-16 ft)",
      "Long 2s (16ft-3P)",
      "Three Pointers"
    ))
  )

shot_data_reactive <- reactive({
  if (input$zone_view == "detailed") {
    combined
  } else {
    combined %>%
      mutate(TwoPoint = ifelse(Zone %in% c("0-3", "3-10", "10-16", "16-3P"), "2P", "3P")) %>%
      group_by(Season, TwoPoint) %>%
      summarise(
        FGA_percent = sum(`FGA%`),
        FG_percent = sum(`FGA%` * `FG%`) / sum(`FGA%`)
      ) %>%
      ungroup() %>%
      mutate(
        Zone_Label = ifelse(TwoPoint == "2P", "2P Combined", "Three Pointers"),
        `FGA%` = FGA_percent,
        `FG%` = FG_percent
      ) %>%
      select(Season, Zone_Label, `FGA%`, `FG%`) %>%
      arrange(factor(Zone_Label, levels = c("2P Combined", "Three Pointers")))
  }
})

output$shooting_heatmap <- renderPlotly({
  p <- ggplot(shot_data_reactive(), aes(x = Zone_Label, y = `FGA%`, fill = `FG%`)) +
    geom_bar(stat = "identity", width = 0.6) +
    scale_fill_gradientn(
    colors = c("#d73027", "#fdae61", "#1a9850"),  # colorblind-friendly Reds-Yellow-Green
    values = scales::rescale(c(0, 0.45, 1)),
    labels = scales::percent_format(accuracy = 1)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Bar Graph of Joel Embiid's Shot Diet (2024 Season)",
      x = "Shot Distance Zone",
      y = "% of Field Goal Attempts",
      fill = "FG%"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggplotly(p, tooltip = c("x", "y", "fill"))
})

}

# RUN APP
shinyApp(ui = ui, server = server)
