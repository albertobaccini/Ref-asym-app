# ============================================================================
# app.R - Interactive Journal Citation Asymmetry Explorer (with Plotly)
# ============================================================================
# This Shiny app allows users to explore citation asymmetry patterns between
# journal clusters. Users can select citing/cited clusters and visualize
# the asymmetry heatmap with faceting by time period.
# 
# NEW FEATURE: Hover over any cell to see the exact asymmetry value!
# ============================================================================

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(plotly)  # <-- NEW: For interactive tooltips

# ============================================================================
# DATA LOADING (runs once when app starts)
# ============================================================================

# Load cluster legend CSV file
# Expected structure: cluster (integer), cluster_label (character)
cluster_legend <- read.csv("cluster_legend.csv", stringsAsFactors = FALSE)

# Filter to keep only clusters 1 through 34
cluster_legend <- cluster_legend %>% 
  filter(cluster >= 1 & cluster <= 34)

# Ensure clusters are in numeric order for dropdown
cluster_legend <- cluster_legend %>% arrange(cluster)

# Create named vector for cluster name lookup: cluster_code -> cluster_name
# Example: cluster_names[3] returns "Computer Science"
cluster_names <- setNames(cluster_legend$cluster_label, cluster_legend$cluster)

# Create named vector for dropdown choices: label = "Name (code)", value = code
# Example: "Computer Science (3)" returns 3 internally
dropdown_choices <- setNames(
  cluster_legend$cluster,  # Internal value (e.g., 3)
  paste0(cluster_legend$cluster_label, " (", cluster_legend$cluster, ")")  # Display label
)

# NOTE: Make sure 'journal_asymmetry' dataframe is loaded in your environment
# If needed, uncomment the line below and adjust the path:
# journal_asymmetry <- readRDS("journal_asymmetry.rds")

# ============================================================================
# USER INTERFACE (UI)
# ============================================================================

ui <- fluidPage(
  
  # Application title
  titlePanel(
    title = div(
      "Journal Citation Asymmetry Explorer",
      h6("Interactive heatmap with tooltips - hover over any cell to see values")
    )
  ),
  
  # Sidebar layout with input controls on the left and plot on the right
  sidebarLayout(
    
    # ===== SIDEBAR PANEL: User inputs =====
    sidebarPanel(
      width = 3,
      
      h4("Cluster Selection"),
      hr(),
      
      # Dropdown for citing cluster (Y-axis / Journal i)
      selectInput(
        inputId = "citing_cluster",
        label = "Row Cluster (Journal i - Y axis)",
        choices = dropdown_choices,
        selected = "3"  # Default to cluster 3
      ),
      
      # Dropdown for cited cluster (X-axis / Journal j)
      selectInput(
        inputId = "cited_cluster",
        label = "Column Cluster (Journal j - X axis)",
        choices = dropdown_choices,
        selected = "3"  # Default to cluster 3
      ),
      
      hr(),
      
      # ===== Visualization options =====
      h4("Visualization Options"),
      hr(),
      
      # Checkbox for truncating long journal names
      checkboxInput(
        inputId = "truncate_labels",
        label = "Truncate X-axis labels to 28 characters",
        value = TRUE
      ),
      
      # Slider for X-axis label font size
      sliderInput(
        inputId = "label_size",
        label = "X-axis label font size",
        min = 4,
        max = 12,
        value = 6,
        step = 0.5
      ),
      
      # Numeric input for asymmetry color scale limits
      numericInput(
        inputId = "asym_limit",
        label = "Asymmetry color scale limit (+/-)",
        value = 0.25,
        min = 0.1,
        max = 1.0,
        step = 0.05
      ),
      
      # Checkbox for reversing Y-axis order (top to bottom)
      checkboxInput(
        inputId = "reverse_y",
        label = "Reverse Y-axis order (top to bottom)",
        value = TRUE
      ),
      
      hr(),
      
      # ===== Export options =====
      h4("Export"),
      hr(),
      
      # Note about plotly export
      helpText("Use the camera icon on the plot to download as PNG."),
      
      br(),
      
      h4("Color legend"),
      
      # Help text explaining asymmetry interpretation
      helpText(
        "Reading across rows:",
        br(),
        "positive values (blue) indicate that journal i relies more intensively on knowledge from journal j;",
        br(),
        "negative values (red) indicate that journal j relies more intensively on knowledge from journal i.",
        br(),
        "Reading down columns: the interpretation is reversed.",
        br(),
        br(),
        br(),
        "💡 Hover over any cell to see the exact asymmetry value"
      )
    ), 
    
    # ===== MAIN PANEL: Output displays =====
    mainPanel(
      width = 9,
      
      # Text output showing current selection information
      verbatimTextOutput("selection_info"),
      
      # Interactive plotly heatmap (replaces static plotOutput)
      plotlyOutput("heatmap_plot", width = "100%", height = "800px")
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Helper function to get cluster name from cluster code
  # Converts input (character) to numeric for lookup
  get_cluster_name <- function(cluster_code) {
    cluster_names[as.character(as.numeric(cluster_code))]
  }
  
  # ===== Reactive data filtering =====
  # Filters the asymmetry data based on selected clusters
  filtered_data <- reactive({
    # Convert inputs from character to numeric (dropdown returns strings)
    citing_code <- as.numeric(input$citing_cluster)
    cited_code <- as.numeric(input$cited_cluster)
    
    # Filter the dataset
    journal_asymmetry %>%
      filter(citingcluster == citing_code,
             citedcluster == cited_code)
  })
  
  # ===== Selection information display =====
  # Shows current cluster selection and data summary
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
  
  # ===== Interactive heatmap plot with Plotly =====
  # Generates the ggplot2 heatmap and converts to interactive plotly
  output$heatmap_plot <- renderPlotly({
    data <- filtered_data()
    
    # Validate that data exists for the selected clusters
    validate(
      need(nrow(data) > 0, 
           "No data available for the selected cluster combination. Please choose different clusters.")
    )
    
    # Get cluster names for plot title
    citing_name <- get_cluster_name(as.numeric(input$citing_cluster))
    cited_name <- get_cluster_name(as.numeric(input$cited_cluster))
    
    # Base plot
    p <- ggplot(data, aes(x = factor(ref_name), 
                          y = factor(work_name), 
                          fill = journal_asymmetry,
                          # Add custom text for tooltip (optional enhancement)
                          text = paste("Journal i:", work_name,
                                       "<br>Journal j:", ref_name,
                                       "<br>Asymmetry:", round(journal_asymmetry, 4)))) +
      geom_tile(color = "white", size = 0.1)
    
    # Apply Y-axis reversal if selected
    if (input$reverse_y) {
      p <- p + scale_y_discrete(limits = rev)
    }
    
    # Apply X-axis label truncation if selected
    if (input$truncate_labels) {
      p <- p + scale_x_discrete(labels = function(x) str_sub(x, 1, 28))
    }
    
    # Apply color scale with user-defined limits
    asym_val <- input$asym_limit
    p <- p + scale_fill_gradientn(
      colors = brewer.pal(11, "RdBu"),
      values = scales::rescale(seq(-asym_val, asym_val, length.out = 11)),
      limits = c(-asym_val, asym_val),
      na.value = "grey90",
      name = "Reference asymmetry"
    )
    
    # Facet by time period (3 columns for compact display)
    p <- p + facet_wrap(~ work_period, ncol = 3)
    
    # Add labels and title
    p <- p + labs(x = "", y = "", fill = "Asymmetry") +
      ggtitle(paste("Citation Asymmetry:", citing_name, "(cluster", input$citing_cluster, 
                    ") vs", cited_name, "(cluster", input$cited_cluster, ")")) +
      theme(plot.title = element_text(hjust = 0.5, size = 12))
    
    # Apply clean theme with custom text formatting
    p <- p + theme_bw() +
      theme(
        axis.text.x = element_text(
          angle = 90, 
          hjust = 1, 
          vjust = 0.5, 
          size = input$label_size
        ),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(face = "bold", size = 10),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm")
      )
    
    # ===== CONVERT TO INTERACTIVE PLOTLY =====
    # This adds hover tooltips showing x, y, and fill values
    # The 'tooltip' parameter specifies which aesthetics to display
    ggplotly(p, tooltip = c("x", "y", "fill", "text")) %>%
      layout(
        # Improve hoverlabel appearance
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12, color = "black"),
          bordercolor = "gray"
        ),
        # Adjust plot margins
        margin = list(b = 50, t = 80, l = 100, r = 30)
      ) %>%
      # Configure mode bar (remove some buttons to simplify)
      config(
        modeBarButtonsToRemove = c(
          "zoom2d",
          "zoomIn2d",
          "zoomOut2d",
          "pan2d",
          "select2d",
          "lasso2d",
          "resetScale2d",
          "hoverClosestCartesian", 
          "hoverCompareCartesian"
        )) %>%
      config(
        displayModeBar = TRUE,
        toImageButtonOptions = list(
          format = "png",
          filename = paste0("asymmetry_", citing_name, "_vs_", cited_name),
          width = 1200,
          height = 800,
          scale = 2
        )
      )
  })
}

# ============================================================================
# RUN THE APPLICATION
# ============================================================================

# This launches the Shiny app
shinyApp(ui = ui, server = server)