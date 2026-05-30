# Movie Data Interactive Dashboard
# Static Shiny App via Shinylive & GitHub Pages

# Load Required Packages
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

# Load Data
df_eda <- read.csv("clean_data.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# ========================
# Data Preprocessing
# ========================
dashboard_data <- df_eda %>%
  filter(
    Worldwide_Gross > 0,
    !is.na(Rating),
    !is.na(Year),
    !is.na(Duration_min),
    Year >= 2000 & Year <= 2024,
    !is.na(Year_Group)
  ) %>%
  rename(
    Movie_Name = Release_Group
  )

# ========================
# UI Layout
# ========================
ui <- dashboardPage(
  # Apply blue skin theme for a consistent color scheme
  skin = "blue",
  dashboardHeader(title = "Movie Data Interactive Dashboard"),
  
  dashboardSidebar(
    sliderInput(
      inputId = "year_range",
      label = "Year Range",
      min = 2000, max = 2024,
      value = c(2000, 2024), sep = ""
    ),
    sliderInput(
      inputId = "rating_range",
      label = "Audience Rating",
      min = 0, max = 10, value = c(0, 10)
    ),
    sliderInput(
      inputId = "gross_range",
      label = "Worldwide Gross (Million USD)",
      min = 0, max = round(max(dashboard_data$Worldwide_Gross)/1e6),
      value = c(0, round(max(dashboard_data$Worldwide_Gross)/1e6))
    ),
    hr(),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trend Analysis", tabName = "trend", icon = icon("chart-line")),
      menuItem("Distribution", tabName = "distribution", icon = icon("box")),
      # Add scatter plot icon for Relationship tab
      menuItem("Relationship", tabName = "relationship", icon = icon("link")),
      menuItem("Top Movies", tabName = "topmovies", icon = icon("trophy"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_movies", width = 3),
                valueBoxOutput("avg_gross", width = 3),
                valueBoxOutput("avg_rating", width = 3),
                valueBoxOutput("avg_duration", width = 3)
              ),
              br(),
              box(title = "Movie Dataset", DTOutput("movie_table"), width = 12)
      ),
      
      tabItem(tabName = "trend",
              fluidRow(
                box(title = "Annual Average Box Office", plotlyOutput("trend_plot"), width = 8),
                box(title = "Annual Movie Count", plotlyOutput("count_plot"), width = 4)
              )
      ),
      
      tabItem(tabName = "distribution",
              fluidRow(
                box(title = "Box Office Distribution by Year Group", plotlyOutput("box_plot"), width = 12)
              )
      ),
      
      tabItem(tabName = "relationship",
              fluidRow(
                box(title = "Duration vs Rating", plotlyOutput("scatter_plot"), width = 12)
              )
      ),
      
      tabItem(tabName = "topmovies",
              fluidRow(
                box(title = "Top 20 by Box Office", DTOutput("top_gross"), width = 6),
                box(title = "Top 20 by Rating", DTOutput("top_rating"), width = 6)
              ),
              fluidRow(
                box(title = "Movie Performance Quadrant", plotlyOutput("quadrant_plot"), width = 12)
              )
      )
    )
  )
)

# ========================
# Server Logic
# ========================
server <- function(input, output, session) {
  
  # Reactive data filtering based on user input
  filtered_data <- reactive({
    dashboard_data %>%
      filter(
        Year >= input$year_range[1] & Year <= input$year_range[2],
        Rating >= input$rating_range[1] & Rating <= input$rating_range[2],
        Worldwide_Gross/1e6 >= input$gross_range[1] & Worldwide_Gross/1e6 <= input$gross_range[2]
      )
  })
  
  # 1. Overview - Key metrics (all blue-themed value boxes)
  output$total_movies <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total Movies", icon = icon("film"), color = "blue")
  })
  output$avg_gross <- renderValueBox({
    avg <- round(mean(filtered_data()$Worldwide_Gross, na.rm=TRUE)/1e6, 1)
    valueBox(paste0(avg, " Million USD"), "Avg Worldwide Gross", icon = icon("dollar"), color = "light-blue")
  })
  output$avg_rating <- renderValueBox({
    avg <- round(mean(filtered_data()$Rating, na.rm=TRUE), 1)
    valueBox(avg, "Avg Audience Rating", icon = icon("star"), color = "teal")
  })
  output$avg_duration <- renderValueBox({
    avg <- round(mean(filtered_data()$Duration_min, na.rm=TRUE), 0)
    valueBox(paste0(avg, " min"), "Avg Duration", icon = icon("clock"), color = "aqua")
  })
  
  # Render interactive movie dataset table
  output$movie_table <- renderDT({
    filtered_data() %>%
      select(Movie_Name, Year, Worldwide_Gross, Rating, Duration_min, Year_Group) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # 2. Trend analysis plots
  output$trend_plot <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Year) %>%
      summarise(avg_gross = mean(Worldwide_Gross, na.rm=TRUE)/1e6) %>%
      ggplot(aes(x = Year, y = avg_gross)) +
      geom_line(color = "#1565C0", linewidth = 1.2) +
      labs(title = "Annual Average Box Office", x = "Year", y = "Gross (Million USD)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Count plot updated to blue color
  output$count_plot <- renderPlotly({
    p <- filtered_data() %>%
      count(Year) %>%
      ggplot(aes(x = Year, y = n)) +
      geom_col(fill = "#42A5F5") +
      labs(x = "Year", y = "Number of Movies") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 3. Box plot with blue-themed color palette
  output$box_plot <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = Year_Group, y = Worldwide_Gross/1e6, fill = Year_Group)) +
      geom_boxplot(alpha=0.7) +
      scale_fill_manual(values = c("#BBDEFB", "#64B5F6", "#1976D2")) +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Year Group", y = "Worldwide Gross (Million$)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 4. Scatter plot updated to blue color
  output$scatter_plot <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = Duration_min, y = Rating)) +
      geom_point(alpha = 0.6, color = "#1976D2") +
      labs(x = "Duration (minutes)", y = "Audience Rating") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 5. Top movies tables
  output$top_gross <- renderDT({
    filtered_data() %>%
      arrange(desc(Worldwide_Gross)) %>%
      slice(1:20) %>%
      select(Movie_Name, Year, Worldwide_Gross, Rating) %>%
      datatable()
  })
  
  output$top_rating <- renderDT({
    filtered_data() %>%
      arrange(desc(Rating)) %>%
      slice(1:20) %>%
      select(Movie_Name, Year, Rating, Worldwide_Gross) %>%
      datatable()
  })
  
  # Quadrant plot with consistent blue color
  output$quadrant_plot <- renderPlotly({
    avg_r <- mean(filtered_data()$Rating, na.rm=TRUE)
    avg_g <- mean(filtered_data()$Worldwide_Gross, na.rm=TRUE)/1e6
    
    p <- filtered_data() %>%
      ggplot(aes(x = Rating, y = Worldwide_Gross/1e6)) +
      geom_vline(xintercept = avg_r, linetype = "dashed", color = "gray50", linewidth=1) +
      geom_hline(yintercept = avg_g, linetype = "dashed", color = "gray50", linewidth=1) +
      geom_point(alpha = 0.6, color = "#1565C0") +
      labs(x = "Audience Rating", y = "Worldwide Gross (Million USD)") +
      theme_minimal()
    ggplotly(p)
  })
}

# ========================
# Launch dashboard
# ========================
shinyApp(ui = ui, server = server)
