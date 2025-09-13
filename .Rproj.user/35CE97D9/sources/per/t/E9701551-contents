# global.R - Updated to use corrected data_manip.R

# List of required packages
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "DT",
  "plotly",
  "dplyr",
  "readxl",
  "lubridate",
  "ggplot2",
  "tidyverse"
)

# Function to check and install packages
install_if_missing <- function(packages) {
  # Get list of installed packages
  installed_packages <- rownames(installed.packages())
  
  # Find missing packages
  missing_packages <- packages[!packages %in% installed_packages]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("This may take a few minutes...\n")
    
    # Install missing packages
    install.packages(missing_packages, dependencies = TRUE)
    
    cat("Package installation complete!\n")
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Check and install missing packages
install_if_missing(required_packages)

# Load all required packages
cat("Loading required packages...\n")
for (package in required_packages) {
  library(package, character.only = TRUE)
  cat("✓", package, "loaded\n")
}

cat("All packages loaded successfully!\n")

# Source the CORRECTED data manipulation script
cat("Sourcing corrected data_manip.R script...\n")
source("data_manip.R")

# Function to get the latest processed data file
get_latest_data <- function() {
  
  finaldata_dir <- "../finaldata"
  
  if(!dir.exists(finaldata_dir)) {
    cat("No finaldata directory found\n")
    return(NULL)
  }
  
  # Find all CSV files in finaldata (prioritize CORRECTED versions)
  csv_files <- list.files(
    finaldata_dir, 
    pattern = "charlottesville_str_daily.*\\.csv$", 
    full.names = TRUE
  )
  
  if(length(csv_files) == 0) {
    cat("No processed data files found\n")
    return(NULL)
  }
  
  # Prioritize CORRECTED files
  corrected_files <- csv_files[str_detect(basename(csv_files), "CORRECTED")]
  if(length(corrected_files) > 0) {
    latest_file <- corrected_files[which.max(file.mtime(corrected_files))]
    cat("Loading CORRECTED data:", basename(latest_file), "\n")
  } else {
    latest_file <- csv_files[which.max(file.mtime(csv_files))]
    cat("Loading processed data:", basename(latest_file), "\n")
    cat("⚠️ This may be uncorrected data - consider reprocessing\n")
  }
  
  # Read and return the data
  data <- read_csv(latest_file, show_col_types = FALSE)
  
  cat("Loaded", nrow(data), "records from processed data\n")
  cat("Date range:", min(data$Date), "to", max(data$Date), "\n")
  
  return(data)
}

# Load and process the data automatically
cat("=== LOADING DATA IN GLOBAL.R ===\n")

# Try to load existing processed data first
hotel_data <- get_latest_data()

# If no processed data exists, process it
if(is.null(hotel_data)) {
  cat("⚠️ No processed data found. Processing with corrected mapping...\n")
  hotel_data <- refresh_data_if_needed()
}

if(!is.null(hotel_data) && nrow(hotel_data) > 0) {
  
  cat("=== DATA LOADING SUCCESSFUL ===\n")
  cat("Total records loaded:", nrow(hotel_data), "\n")
  cat("Date range:", as.character(min(hotel_data$Date)), "to", as.character(max(hotel_data$Date)), "\n")
  
  # Verify data quality by checking May 18, 2024
  may18_check <- hotel_data %>% filter(Date == "2024-05-18")
  if(nrow(may18_check) > 0) {
    cat("\n🎯 DATA QUALITY CHECK (May 18, 2024 Saturday):\n")
    cat("   Occupancy:", round(may18_check$Occupancy, 2), "%\n")
    cat("   ADR: $", round(may18_check$ADR, 2), "\n")
    cat("   RevPAR: $", round(may18_check$RevPAR, 2), "\n")
    
    # Check if this looks like corrected data
    if(may18_check$ADR > 400) {
      cat("✅ Data appears to be CORRECTED (Saturday ADR > $400)\n")
    } else {
      cat("⚠️ Data may be UNCORRECTED (Saturday ADR < $400)\n")
      cat("🔧 Consider running: source('reprocess_data.R')\n")
    }
  }
  
  # Show yearly breakdown
  yearly_breakdown <- hotel_data %>%
    group_by(Year = year(Date)) %>%
    summarise(Records = n(), .groups = "drop") %>%
    arrange(Year)
  
  cat("Records by year:\n")
  for(i in 1:nrow(yearly_breakdown)) {
    cat(" ", yearly_breakdown$Year[i], ":", yearly_breakdown$Records[i], "records\n")
  }
  
  # Show recent data if available
  recent_data <- hotel_data %>%
    filter(Date >= as.Date("2025-01-01")) %>%
    group_by(Month = floor_date(Date, "month")) %>%
    summarise(Records = n(), .groups = "drop") %>%
    arrange(Month)
  
  if(nrow(recent_data) > 0) {
    cat("2025 monthly breakdown:\n")
    for(i in 1:nrow(recent_data)) {
      cat(" ", format(recent_data$Month[i], "%Y-%m"), ":", recent_data$Records[i], "records\n")
    }
  }
  
  # Create summary data for quick access
  day_summary <- hotel_data %>%
    group_by(DayOfWeek, DayAbbrev) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Avg_RevPAR)
  
  # Seasonal summary
  season_summary <- hotel_data %>%
    group_by(Season, DayType) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # Monthly performance
  monthly_summary <- hotel_data %>%
    group_by(Month, Season) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Season, Avg_RevPAR)
  
  # Holiday summary for quick access
  holiday_summary <- hotel_data %>%
    filter(Holiday != "Regular Day") %>%
    group_by(Holiday) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Avg_RevPAR)
  
  cat("Summary tables created successfully!\n")
  cat("=== GLOBAL.R INITIALIZATION COMPLETE ===\n")
  
} else {
  cat("=== DATA LOADING FAILED ===\n")
  cat("No data could be loaded. Please check:\n")
  cat("1. Your ../data directory exists\n")
  cat("2. It contains Excel files with 'Week Ending' in the filename\n")
  cat("3. The files contain Charlottesville data\n")
  cat("4. The file format matches expected column structure\n")
  
  # Set hotel_data to empty tibble to prevent errors
  hotel_data <- tibble(
    Date = as.Date(character()),
    DayOfWeek = character(),
    Occupancy = numeric(),
    ADR = numeric(),
    RevPAR = numeric()
  )
  
  cat("Created empty dataset to prevent app crashes\n")
}

cat("\n🎯 DATA PROCESSING STATUS:\n")
cat("✅ Updated data_manip.R with corrected column mapping\n")
cat("✅ 2024+ files now use ADR columns 24-30 (was 23-29)\n") 
cat("✅ This fixes the 1-day shift error in ADR and RevPAR\n")
cat("\nIf you need to reprocess data: source('reprocess_data.R')\n")

# List of required packages
required_packages <- c(
  "shiny",
  "shinydashboard", 
  "DT",
  "plotly",
  "dplyr",
  "readxl",
  "lubridate",
  "ggplot2",
  "tidyverse"
)

# Function to check and install packages
install_if_missing <- function(packages) {
  # Get list of installed packages
  installed_packages <- rownames(installed.packages())
  
  # Find missing packages
  missing_packages <- packages[!packages %in% installed_packages]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("This may take a few minutes...\n")
    
    # Install missing packages
    install.packages(missing_packages, dependencies = TRUE)
    
    cat("Package installation complete!\n")
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Check and install missing packages
install_if_missing(required_packages)

# Load all required packages
cat("Loading required packages...\n")
for (package in required_packages) {
  library(package, character.only = TRUE)
  cat("✓", package, "loaded\n")
}

cat("All packages loaded successfully!\n")

# Source the CORRECTED data manipulation script
cat("Sourcing CORRECTED data manipulation script...\n")
source("data_manip.R")

# Function to get the latest CORRECTED data file
get_latest_corrected_data <- function() {
  
  finaldata_dir <- "../finaldata"
  
  if(!dir.exists(finaldata_dir)) {
    cat("No finaldata directory found\n")
    return(NULL)
  }
  
  # Look for CORRECTED files first, then any processed files
  csv_files <- list.files(
    finaldata_dir, 
    pattern = "charlottesville_str_daily.*\\.csv$", 
    full.names = TRUE
  )
  
  if(length(csv_files) == 0) {
    cat("No processed data files found\n")
    return(NULL)
  }
  
  # Prioritize CORRECTED files
  corrected_files <- csv_files[str_detect(basename(csv_files), "CORRECTED")]
  if(length(corrected_files) > 0) {
    latest_file <- corrected_files[which.max(file.mtime(corrected_files))]
    cat("Loading CORRECTED data:", basename(latest_file), "\n")
  } else {
    # Fall back to most recent file
    latest_file <- csv_files[which.max(file.mtime(csv_files))]
    cat("⚠️ Loading non-corrected data:", basename(latest_file), "\n")
    cat("🔧 Run reprocess_data.R to generate corrected dataset\n")
  }
  
  # Read and return the data
  data <- read_csv(latest_file, show_col_types = FALSE)
  
  cat("Loaded", nrow(data), "records from processed data\n")
  cat("Date range:", min(data$Date), "to", max(data$Date), "\n")
  
  return(data)
}

# Load and process the CORRECTED data automatically
cat("=== LOADING CORRECTED DATA IN GLOBAL.R ===\n")

# Try to load existing corrected data first
hotel_data <- get_latest_corrected_data()

# If no corrected data exists, offer to process it
if(is.null(hotel_data)) {
  cat("⚠️ No corrected data found. Processing with corrected mapping...\n")
  hotel_data <- refresh_data_CORRECTED()
}

if(!is.null(hotel_data) && nrow(hotel_data) > 0) {
  
  cat("=== DATA LOADING SUCCESSFUL ===\n")
  cat("Total records loaded:", nrow(hotel_data), "\n")
  cat("Date range:", as.character(min(hotel_data$Date)), "to", as.character(max(hotel_data$Date)), "\n")
  
  # Verify data quality by checking May 18, 2024
  may18_check <- hotel_data %>% filter(Date == "2024-05-18")
  if(nrow(may18_check) > 0) {
    cat("\n🎯 DATA QUALITY CHECK (May 18, 2024 Saturday):\n")
    cat("   Occupancy:", round(may18_check$Occupancy, 2), "%\n")
    cat("   ADR: $", round(may18_check$ADR, 2), "\n")
    cat("   RevPAR: $", round(may18_check$RevPAR, 2), "\n")
    
    # Check if this looks like corrected data
    if(may18_check$ADR > 400) {
      cat("✅ Data appears to be CORRECTED (Saturday ADR > $400)\n")
    } else {
      cat("⚠️ Data may be UNCORRECTED (Saturday ADR < $400)\n")
      cat("🔧 Consider running reprocess_data.R to get corrected data\n")
    }
  }
  
  # Show yearly breakdown
  yearly_breakdown <- hotel_data %>%
    group_by(Year = year(Date)) %>%
    summarise(Records = n(), .groups = "drop") %>%
    arrange(Year)
  
  cat("Records by year:\n")
  for(i in 1:nrow(yearly_breakdown)) {
    cat(" ", yearly_breakdown$Year[i], ":", yearly_breakdown$Records[i], "records\n")
  }
  
  # Show recent data if available
  recent_data <- hotel_data %>%
    filter(Date >= as.Date("2025-01-01")) %>%
    group_by(Month = floor_date(Date, "month")) %>%
    summarise(Records = n(), .groups = "drop") %>%
    arrange(Month)
  
  if(nrow(recent_data) > 0) {
    cat("2025 monthly breakdown:\n")
    for(i in 1:nrow(recent_data)) {
      cat(" ", format(recent_data$Month[i], "%Y-%m"), ":", recent_data$Records[i], "records\n")
    }
  }
  
  # Create summary data for quick access
  day_summary <- hotel_data %>%
    group_by(DayOfWeek, DayAbbrev) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Avg_RevPAR)
  
  # Seasonal summary
  season_summary <- hotel_data %>%
    group_by(Season, DayType) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # Monthly performance
  monthly_summary <- hotel_data %>%
    group_by(Month, Season) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Season, Avg_RevPAR)
  
  # Holiday summary for quick access
  holiday_summary <- hotel_data %>%
    filter(Holiday != "Regular Day") %>%
    group_by(Holiday) %>%
    summarise(
      Count = n(),
      Avg_Occupancy = round(mean(Occupancy, na.rm = TRUE), 1),
      Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
      Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Avg_RevPAR)
  
  cat("Summary tables created successfully!\n")
  cat("=== CORRECTED GLOBAL.R INITIALIZATION COMPLETE ===\n")
  
} else {
  cat("=== DATA LOADING FAILED ===\n")
  cat("No data could be loaded. Please check:\n")
  cat("1. Your ../data directory exists\n")
  cat("2. It contains Excel files with 'Week Ending' in the filename\n")
  cat("3. The files contain Charlottesville data\n")
  cat("4. The file format matches expected column structure\n")
  
  # Set hotel_data to empty tibble to prevent errors
  hotel_data <- tibble(
    Date = as.Date(character()),
    DayOfWeek = character(),
    Occupancy = numeric(),
    ADR = numeric(),
    RevPAR = numeric()
  )
  
  cat("Created empty dataset to prevent app crashes\n")
}

# Function to refresh with corrected data
refresh_data_if_needed_CORRECTED <- function() {
  cat("=== REFRESHING WITH CORRECTED COLUMN MAPPING ===\n")
  return(refresh_data_CORRECTED())
}

cat("\n🎯 TO USE CORRECTED DATA:\n")
cat("1. Replace data_manip.R with data_manip_CORRECTED.R\n")
cat("2. Replace global.R with this updated version\n") 
cat("3. Run reprocess_data.R to generate corrected dataset\n")
cat("4. Restart your Shiny dashboard to use corrected data\n")