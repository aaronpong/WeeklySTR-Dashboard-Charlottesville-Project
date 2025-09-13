# setup_project.R - Run this first to ensure everything works

# Install required packages
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", 
                       "dplyr", "readxl", "lubridate", "ggplot2", 
                       "tidyverse", "here")

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% rownames(installed.packages())]
  if(length(missing) > 0) {
    cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(required_packages)

# Check project structure
cat("=== PROJECT SETUP CHECK ===\n")
cat("Current directory:", getwd(), "\n")
cat("Project files found:\n")

required_dirs <- c("app", "data", "finaldata")
required_files <- c("app/ui.R", "app/server.R", "app/data_manip.R", "app/global.R")

for(dir in required_dirs) {
  if(dir.exists(dir)) {
    cat("✓", dir, "directory found\n")
  } else {
    cat("✗", dir, "directory MISSING\n")
  }
}

for(file in required_files) {
  if(file.exists(file)) {
    cat("✓", file, "found\n")
  } else {
    cat("✗", file, "MISSING\n")
  }
}

# Check data files
data_files <- list.files("data", pattern = "Week Ending.*\\.xlsx", recursive = TRUE)
cat("STR data files found:", length(data_files), "\n")

if(length(data_files) > 0) {
  cat("✓ Data files look good!\n")
  cat("Ready to run: runApp('app')\n")
} else {
  cat("✗ No STR data files found. Check your data directory.\n")
}