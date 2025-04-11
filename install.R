# install.R – Install required packages for Tourismusdashboard using versions from a specific date

# Set the snapshot date
snapshot_date <- as.Date("2025-04-01")  # <-- Change this as needed

# Load or install renv
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
library(renv)

# Initialize renv if not already initialized
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

# Set snapshot date to the one provided
options(renv.config.snapshot.date.override = snapshot_date)

# Define required packages
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "dplyr", "tidyr", "lubridate", "stringr",
  "ggplot2", "plotly",
  "readr", "jsonlite",
  "httr", "xml2",
  "sf", "leaflet",
  "purrr", "glue", "DT"
)

# Install specific versions from the CRAN snapshot on the chosen date
renv::install(required_packages)

# Optionally snapshot the current state (generates renv.lock)
renv::snapshot()
