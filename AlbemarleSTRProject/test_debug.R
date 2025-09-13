# test_debug.R - Run this FIRST to test your data loading

library(tidyverse)
library(readxl) 
library(lubridate)

# Source your functions
source("data_manip.R")

# Test 1: Check file discovery
cat("=== TESTING FILE DISCOVERY ===\n")
str_files <- find_str_files("../data")
cat("Files found:", length(str_files), "\n")

if(length(str_files) > 0) {
  # Show some sample files
  recent_files <- str_files[grepl("2025", str_files)]
  cat("2025 files:", length(recent_files), "\n")
  
  if(length(recent_files) > 0) {
    cat("Sample 2025 file:", basename(recent_files[1]), "\n")
    
    # Test 2: Try processing one file
    cat("\n=== TESTING FILE PROCESSING ===\n")
    test_result <- extract_charlottesville_daily_data(recent_files[1])
    
    if(!is.null(test_result)) {
      cat("SUCCESS: File processed\n")
      cat("Records:", nrow(test_result), "\n")
      cat("Date range:", min(test_result$Date), "to", max(test_result$Date), "\n")
      print(head(test_result %>% select(Date, DayOfWeek, Occupancy, ADR, RevPAR)))
    } else {
      cat("FAILED: Could not process file\n")
    }
  }
} else {
  cat("No STR files found! Check your directory structure.\n")
}

# Test 3: Full data loading
cat("\n=== TESTING FULL DATA LOADING ===\n")
full_data <- refresh_data_if_needed()

if(!is.null(full_data)) {
  cat("SUCCESS: Full data loaded\n")
  cat("Total records:", nrow(full_data), "\n")
  cat("Date range:", min(full_data$Date), "to", max(full_data$Date), "\n")
  
  # Check 2025 data specifically
  data_2025 <- full_data %>% filter(year(Date) == 2025)
  if(nrow(data_2025) > 0) {
    cat("2025 records:", nrow(data_2025), "\n")
    monthly_2025 <- data_2025 %>%
      group_by(Month = floor_date(Date, "month")) %>%
      summarise(Records = n()) %>%
      arrange(Month)
    cat("2025 monthly breakdown:\n")
    print(monthly_2025)
  } else {
    cat("No 2025 data found\n")
  }
} else {
  cat("FAILED: Could not load data\n")
}

