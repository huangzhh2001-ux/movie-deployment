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

# Load raw dataset from local csv file
df_eda <- read.csv("clean_data.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# ========================
# Data Preprocessing Section
# Filter valid records and rename movie name column for dashboard usage
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
# UI Layout Definition
# Define overall dashboard page structure, sidebar and body content
# ========================
ui <- dashboardPage(
  # Apply blue skin theme for consistent page color scheme
  skin = "blue",
  dashboardHeader(title = "Movie Data Interactive Dashboard"),
  
  dashboardSidebar(
    # Year selection slider
    sliderInput(
      inputId = "year_range",
      label = "Year Range",
      min = 2000, max = 2024,
      value = c(2000, 2024), sep = ""
    ),
    # Audience rating filter slider
    sliderInput(
      inputId = "rating_range",
      label = "Audience Rating",
      min = 0, max = 10, value = c(0, 10)
    ),
    # Worldwide gross revenue filter slider (unit: million USD)
    sliderInput(
      inputId = "gross_range",
      label = "Worldwide Gross (Million USD)",
      min = 0, max = round(max(dashboard_data$Worldwide_Gross)/1e6),
      value = c(0, round(max(dashboard_data$Worldwide_Gross)/1e6))
    ),
    hr(),
    # Sidebar navigation menu list
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trend Analysis", tabName = "trend", icon = icon("chart-line")),
      menuItem("Distribution", tabName = "distribution", icon = icon("box")),
      menuItem("Relationship", tabName = "relationship", icon = icon("link")),
      menuItem("Top Movies", tabName = "topmovies", icon = icon("trophy")),
      menuItem("About This App", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    # Custom CSS: center all text inside valueBox cards and adjust font size
    tags$head(
      tags$style(HTML("
        .small-box .inner {
          padding: 10px;
          text-align:center !important;
        }
        .small-box .inner h3 {
          font-size: 24px !important;
          line-height: 1.2;
          text-align:center !important;
        }
        .small-box .inner p {
          font-size: 12px !important;
          line-height: 1.3;
          text-align:center !important;
        }
      "))
    ),
    
    # Each tab content panel definition
    tabItems(
      # Overview page: summary KPIs + full dataset table
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
      
      # Trend Analysis page: annual gross & annual release count visualization
      tabItem(tabName = "trend",
              fluidRow(
                box(title = "Annual Average Box Office", plotlyOutput("trend_plot"), width = 8),
                box(title = "Annual Movie Count", plotlyOutput("count_plot"), width = 4)
              )
      ),
      
      # Distribution page: boxplot grouped by release year group
      tabItem(tabName = "distribution",
              fluidRow(
                box(title = "Box Office Distribution by Year Group", plotlyOutput("box_plot"), width = 12)
              )
      ),
      
      # Relationship page: runtime vs audience rating scatter plot
      tabItem(tabName = "relationship",
              fluidRow(
                box(title = "Duration vs Rating", plotlyOutput("scatter_plot"), width = 12)
              )
      ),
      
      # Top Movies page: top ranking tables + four-quadrant performance analysis
      tabItem(tabName = "topmovies",
              fluidRow(
                box(title = "Top 20 by Box Office", DTOutput("top_gross"), width = 6),
                box(title = "Top 20 by Rating", DTOutput("top_rating"), width = 6)
              ),
              fluidRow(
                box(title = "Movie Performance Quadrant", plotlyOutput("quadrant_plot"), width = 12)
              )
      ),
      
      # About page: project introduction, course info and analysis description
      tabItem(tabName = "about",
              box(width = 12, solidHeader = FALSE,
                  h2(icon("book"),"About This Application"),hr(),
                  h4("Course: WQD7004 - Programming for Data Science"),
                  h4("Dataset: Global Movie Boxoffice & Rating Dataset (2000–2024)"),
                  h4("Core Analysis Modules: Trend Analysis | Distribution | Correlation | Quadrant Classification"),
                  br(),
                  h3("Purpose"),
                  p("This interactive dashboard explores global commercial performance & audience rating characteristics of modern movies, analyzing how release year, runtime affect box-office revenue and audience evaluation across different year cohorts."),
                  br(),
                  h3("Methodology"),
                  tags$ul(
                    tags$li("Time-series trend visualization for annual average box-office & movie release count"),
                    tags$li("Boxplot distribution analysis of gross revenue grouped by release period"),
                    tags$li("Scatter correlation between movie runtime and audience rating"),
                    tags$li("Quadrant segmentation: Blockbuster / Commercial Hit / Cult Classic / Low Performer by rating & gross cutoff")
                  ),
                  br(),
                  p("Note: Worldwide Gross unit converted to Million USD for unified visualization.")
              )
      )
    )
  )
)

# ========================
# Server Logic Section
# Define reactive filter and all plot/table rendering functions
# ========================
server <- function(input, output, session) {
  
  # Reactive dataset: dynamically filter data based on user sidebar input
  filtered_data <- reactive({
    dashboard_data %>%
      filter(
        Year >= input$year_range[1] & Year <= input$year_range[2],
        Rating >= input$rating_range[1] & Rating <= input$rating_range[2],
        Worldwide_Gross/1e6 >= input$gross_range[1] & Worldwide_Gross/1e6 <= input$gross_range[2]
      )
  })
  
  # Render four key indicator value boxes for overview tab
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
  
  # Render full interactive dataset table, ALL COLUMN CENTER
  output$movie_table <- renderDT({
    filtered_data() %>%
      select(Movie_Name, Year, Worldwide_Gross, Rating, Duration_min, Year_Group) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10,
                               columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  # Annual average box office line chart (convert ggplot to plotly interactive), title center
  output$trend_plot <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Year) %>%
      summarise(avg_gross = mean(Worldwide_Gross, na.rm=TRUE)/1e6) %>%
      ggplot(aes(x = Year, y = avg_gross)) +
      geom_line(color = "#1565C0", linewidth = 1.2) +
      labs(title = "Annual Average Box Office", x = "Year", y = "Gross (Million USD)") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # Annual movie release count bar chart, title center
  output$count_plot <- renderPlotly({
    p <- filtered_data() %>%
      count(Year) %>%
      ggplot(aes(x = Year, y = n)) +
      geom_col(fill = "#42A5F5") +
      labs(x = "Year", y = "Number of Movies", title = "Annual Movie Count") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # Box office distribution boxplot grouped by year group with log y-axis, title center
  output$box_plot <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = Year_Group, y = Worldwide_Gross/1e6, fill = Year_Group)) +
      geom_boxplot(alpha=0.7) +
      scale_fill_manual(values = c("#BBDEFB", "#64B5F6", "#1976D2")) +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Year Group", y = "Worldwide Gross (Million$)", title = "Box Office Distribution by Year Group") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # Runtime vs Rating scatter plot, title center
  output$scatter_plot <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = Duration_min, y = Rating)) +
      geom_point(alpha = 0.6, color = "#1976D2") +
      labs(x = "Duration (minutes)", y = "Audience Rating", title = "Duration vs Rating") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  # Top20 movies sorted by worldwide gross revenue, table center
  output$top_gross <- renderDT({
    filtered_data() %>%
      arrange(desc(Worldwide_Gross)) %>%
      slice(1:20) %>%
      select(Movie_Name, Year, Worldwide_Gross, Rating) %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  # Top20 movies sorted by audience rating, table center
  output$top_rating <- renderDT({
    filtered_data() %>%
      arrange(desc(Rating)) %>%
      slice(1:20) %>%
      select(Movie_Name, Year, Rating, Worldwide_Gross) %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  # Four quadrant analysis scatter plot split by average rating and average gross, title center
  output$quadrant_plot <- renderPlotly({
    avg_r <- mean(filtered_data()$Rating, na.rm=TRUE)
    avg_g <- mean(filtered_data()$Worldwide_Gross, na.rm=TRUE)/1e6
    
    p <- filtered_data() %>%
      ggplot(aes(x = Rating, y = Worldwide_Gross/1e6)) +
      geom_vline(xintercept = avg_r, linetype = "dashed", color = "gray50", linewidth=1) +
      geom_hline(yintercept = avg_g, linetype = "dashed", color = "gray50", linewidth=1) +
      geom_point(alpha = 0.6, color = "#1565C0") +
      labs(x = "Audience Rating", y = "Worldwide Gross (Million USD)", title = "Movie Performance Quadrant") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
}

# ========================
# Run shiny application
# ========================
shinyApp(ui = ui, server = server)
