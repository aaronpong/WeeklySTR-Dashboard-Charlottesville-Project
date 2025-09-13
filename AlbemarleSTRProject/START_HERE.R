cat("Charlottesville STR Dashboard\n")
cat("Setting up environment and launching dashboard...\n\n")

# Function to install missing packages
install_if_missing <- function(packages) {
  missing <- packages[!packages %in% rownames(installed.packages())]
  if(length(missing) > 0) {
    cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
    cat("This may take a few minutes on first run...\n\n")
    install.packages(missing, dependencies = TRUE, repos = "https://cran.rstudio.com/")
    cat("Package installation complete!\n\n")
  } else {
    cat("All required packages are already installed.\n\n")
  }
}

# Required packages for the dashboard
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "DT",
  "plotly",
  "dplyr",
  "readxl",
  "lubridate",
  "ggplot2",
  "tidyverse",
  "here"
)

# Step 1: Install any missing packages
cat("Step 1: Checking and installing required packages...\n")
install_if_missing(required_packages)

# Step 2: Set up correct working directory
cat("Step 2: Setting up file paths...\n")

# Function to find project root
find_project_root <- function() {
  current_dir <- getwd()
  
  # Check if we're already in the project root
  if(file.exists("AlbermarleSTRProject.Rproj") || 
     (dir.exists("app") && dir.exists("data"))) {
    return(current_dir)
  }
  
  # Check if we're in a subdirectory
  if(file.exists("../AlbermarleSTRProject.Rproj") ||
     (dir.exists("../app") && dir.exists("../data"))) {
    return(dirname(current_dir))
  }
  
  # Try to find it by looking for key directories
  test_dirs <- c(".", "..", "../..", "../../..")
  for(test_dir in test_dirs) {
    full_path <- normalizePath(test_dir, mustWork = FALSE)
    if(dir.exists(file.path(full_path, "app")) && 
       dir.exists(file.path(full_path, "data"))) {
      return(full_path)
    }
  }
  
  return(current_dir)
}

# Set working directory to project root
project_root <- find_project_root()
setwd(project_root)

cat("Working from:", getwd(), "\n")

# Step 3: Verify project structure
cat("\nStep 3: Verifying project structure...\n")

required_dirs <- c("app", "data")
required_files <- c("app/ui.R", "app/server.R", "app/data_manip.R", "app/global.R")

structure_ok <- TRUE

for(dir in required_dirs) {
  if(dir.exists(dir)) {
    cat("Found:", dir, "directory\n")
  } else {
    cat("MISSING:", dir, "directory\n")
    structure_ok <- FALSE
  }
}

for(file in required_files) {
  if(file.exists(file)) {
    cat("Found:", basename(file), "\n")
  } else {
    cat("MISSING:", basename(file), "\n")
    structure_ok <- FALSE
  }
}

# Check for data files
if(dir.exists("data")) {
  data_files <- list.files("data", pattern = "Week Ending.*\\.xlsx", recursive = TRUE)
  cat("STR data files found:", length(data_files), "\n")
  
  if(length(data_files) == 0) {
    cat("Warning: No STR Excel files found in data directory\n")
    cat("Make sure your 'Week Ending...' Excel files are in the data folder\n")
  }
} else {
  cat("MISSING: Data directory not found\n")
  structure_ok <- FALSE
}

# Create finaldata directory if it doesn't exist
if(!dir.exists("finaldata")) {
  dir.create("finaldata")
  cat("Created finaldata directory\n")
} else {
  cat("Found: finaldata directory\n")
}

# Step 4: Launch the dashboard
if(structure_ok) {
  cat("\nStep 4: Launching dashboard...\n")
  cat("The dashboard will open in your web browser shortly.\n")
  cat("Keep this R console window open while using the dashboard.\n")
  cat("To stop the dashboard, press Ctrl+C (or Cmd+C on Mac) in this console.\n\n")
  
  cat("Launching dashboard...\n")
  
  # Load required libraries
  library(shiny)
  
  # Launch the app
  tryCatch({
    runApp("app", launch.browser = TRUE)
  }, error = function(e) {
    cat("\nError launching dashboard:\n")
    cat(e$message, "\n\n")
    cat("Troubleshooting suggestions:\n")
    cat("1. Make sure all Excel files are in the 'data' folder\n")
    cat("2. Try running: runApp('app') manually in R console\n")
    cat("3. Check that you have the latest version of R and RStudio\n")
    cat("4. Contact the team lead for support\n\n")
  })
  
} else {
  cat("\nCannot launch dashboard - missing required files/directories\n")
  cat("\nPlease check:\n")
  cat("1. You downloaded the complete project folder\n")
  cat("2. All files are unzipped/extracted properly\n")
  cat("3. The 'app' and 'data' folders are present\n")
  cat("4. Your STR Excel files are in the 'data' folder\n\n")
  cat("If problems persist, contact the team lead for support.\n")
}

cat("\nNote: This console will stay open while the dashboard runs.\n")
cat("To stop the dashboard, press Ctrl+C (or Cmd+C on Mac) here.\n")

