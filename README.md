# Citation Asymmetry Explorer

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)

An interactive Shiny app to explore citation asymmetry patterns between journal clusters.
Users can select citing and cited clusters and visualize asymmetry heatmaps faceted by time period.

## Features

- Select citing and cited journal clusters interactively
- Visualize citation asymmetry as an interactive heatmap
- Faceting by time period
- Built with Plotly for dynamic, zoomable charts

## Requirements

R (>= 4.0) and the following packages:
```r
install.packages(c("shiny", "ggplot2", "dplyr", "stringr", "RColorBrewer", "plotly"))
```

## How to run

Clone the repository and run in R:
```r
shiny::runApp()
```

Or directly from GitHub:
```r
shiny::runGitHub("shiny-plotly-app", "alberto.baccini")
```

## Data

Place the required data files in the `/data` folder before running the app.

## License

MIT License