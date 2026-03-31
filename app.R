# ============================================================================
# app.R - Interactive Journal Citation Asymmetry Explorer (Pure Plotly)
# ============================================================================

# Load required libraries (NO GGPLOT2!)
library(shiny)
library(dplyr)
library(stringr)
library(plotly)

# ============================================================================
# DATA LOADING
# ============================================================================

# Load cluster legend CSV file
cluster_legend <- read.csv("cluster_legend.csv", stringsAsFactors = FALSE)

# Filter to keep only clusters 1 through 34
cluster_legend <- cluster_legend %>% 
  filter(cluster >= 1 & cluster <= 34)

# Ensure clusters are in numeric order for dropdown
cluster_legend <- cluster_legend %>% arrange(cluster)

# Create named vector for cluster name lookup
cluster_names <- setNames(cluster_legend$cluster_label, cluster_legend$cluster)

# Create named vector for dropdown choices
dropdown_choices <- setNames(
  cluster_legend$cluster,
  paste0(cluster_legend$cluster_label, " (", cluster_legend$cluster, ")")
)

# Load the asymmetry data
journal_asymmetry <- readRDS("data/journal_asymmetry.rds")

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- fluidPage(
  titlePanel(
    title = div(
      "Journal Citation Asymmetry Explorer",
      h6("Interactive heatmap - hover over any cell to see values")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Cluster Selection"),
      hr(),
      selectInput(
        inputId = "citing_cluster",
        label = "Row Cluster (Journal i - Y axis)",
        choices = dropdown_choices,
        selected = "3"
      ),
      selectInput(
        inputId = "cited_cluster",
        label = "Column Cluster (Journal j - X axis)",
        choices = dropdown_choices,
        selected = "3"
      ),
      hr(),
      h4("Visualization Options"),
      hr(),
      checkboxInput(
        inputId = "truncate_labels",
        label = "Truncate X-axis labels to 28 characters",
        value = TRUE
      ),
      sliderInput(
        inputId = "label_size",
        label = "X-axis label font size",
        min = 8,
        max = 14,
        value = 10,
        step = 0.5
      ),
      numericInput(
        inputId = "asym_limit",
        label = "Asymmetry color scale limit (+/-)",
        value = 0.25,
        min = 0.1,
        max = 1.0,
        step = 0.05
      ),
      checkboxInput(
        inputId = "reverse_y",
        label = "Reverse Y-axis order (top to bottom)",
        value = TRUE
      ),
      hr(),
      h4("Export"),
      hr(),
      helpText("Use the camera icon on the plot to download as PNG."),
      br(),
      h4("Color legend"),
      helpText(
        "Positive values (blue): journal i relies more on journal j",
        br(),
        "Negative values (red): journal j relies more on journal i",
        br(),
        br(),
        "💡 Hover over any cell to see the exact asymmetry value"
      )
    ), 
    
    mainPanel(
      width = 9,
      verbatimTextOutput("selection_info"),
      plotlyOutput("heatmap_plot", width = "100%", height = "800px")
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  get_cluster_name <- function(cluster_code) {
    cluster_names[as.character(as.numeric(cluster_code))]
  }
  
  filtered_data <- reactive({
    citing_code <- as.numeric(input$citing_cluster)
    cited_code <- as.numeric(input$cited_cluster)
    
    journal_asymmetry %>%
      filter(citingcluster == citing_code,
             citedcluster == cited_code)
  })
  
  output$selection_info <- renderPrint({
    citing_code <- as.numeric(input$citing_cluster)
    cited_code <- as.numeric(input$cited_cluster)
    data <- filtered_data()
    
    cat("Current selection:\n")
    cat("  Citing cluster (i): ", citing_code, " - ", get_cluster_name(citing_code), "\n")
    cat("  Cited cluster (j):  ", cited_code, " - ", get_cluster_name(cited_code), "\n")
    cat("  Number of journal pairs: ", nrow(data), "\n")
    if(nrow(data) > 0) {
      cat("  Time periods: ", paste(unique(data$work_period), collapse = ", "), "\n")
    }
  })
  
  output$heatmap_plot <- renderPlotly({
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, 
           "No data available for the selected cluster combination.")
    )
    
    citing_name <- get_cluster_name(as.numeric(input$citing_cluster))
    cited_name <- get_cluster_name(as.numeric(input$cited_cluster))
    
    time_periods <- unique(data$work_period)
    plots <- list()
    
    for(period in time_periods) {
      period_data <- data %>% filter(work_period == period)
      
      work_names <- unique(period_data$work_name)
      ref_names <- unique(period_data$ref_name)
      
      if(input$reverse_y) {
        work_names <- rev(work_names)
      }
      
      ref_labels <- if(input$truncate_labels) {
        str_sub(ref_names, 1, 28)
      } else {
        ref_names
      }
      
      z_matrix <- matrix(NA, nrow = length(work_names), ncol = length(ref_names))
      for(i in seq_along(work_names)) {
        for(j in seq_along(ref_names)) {
          val <- period_data %>%
            filter(work_name == work_names[i], ref_name == ref_names[j]) %>%
            pull(journal_asymmetry)
          if(length(val) > 0) {
            z_matrix[i, j] <- val
          }
        }
      }
      
      p <- plot_ly(
        x = ref_labels,
        y = work_names,
        z = z_matrix,
        type = "heatmap",
        colorscale = list(
          list(0, "rgb(215,48,39)"),
          list(0.5, "rgb(255,255,255)"),
          list(1, "rgb(69,117,180)")
        ),
        zmin = -input$asym_limit,
        zmax = input$asym_limit,
        hovertemplate = "Journal i: %{y}<br>Journal j: %{x}<br>Asymmetry: %{z:.4f}<extra></extra>"
      ) %>%
        layout(
          title = list(text = period, font = list(size = 12)),
          xaxis = list(tickangle = 90, tickfont = list(size = input$label_size)),
          yaxis = list(title = "")
        )
      
      plots[[length(plots) + 1]] <- p
    }
    
    if(length(plots) > 0) {
      subplot(plots, nrows = ceiling(length(plots) / 3), margin = 0.05) %>%
        layout(
          title = paste("Citation Asymmetry:", citing_name, "vs", cited_name),
          annotations = list(
            list(x = 0.5, y = -0.1, text = "Column Journals (j)", showarrow = FALSE, xref = "paper", yref = "paper"),
            list(x = -0.08, y = 0.5, text = "Row Journals (i)", showarrow = FALSE, xref = "paper", yref = "paper", textangle = 90)
          )
        ) %>%
        config(toImageButtonOptions = list(format = "png", filename = paste0("asymmetry_", citing_name, "_vs_", cited_name)))
    } else {
      plotly_empty() %>% layout(title = "No data available")
    }
  })
}

shinyApp(ui = ui, server = server)
