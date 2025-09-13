# validate_data_corrected.R - Works from AlbemarleSTRProject directory

library(tidyverse)
library(readxl)
library(lubridate)

# Function to extract specific date from raw file
check_raw_value <- function(file_path, target_date) {
  tryCatch({
    # Read the raw file
    data <- read_excel(file_path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
    
    # Extract week ending date from filename
    file_name <- basename(file_path)
    date_string <- str_extract(file_name, "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}")
    week_ending <- mdy(date_string)
    
    # Calculate if target date is in this week
    week_start <- week_ending - 6
    if(target_date >= week_start && target_date <= week_ending) {
      # Find day index (1 = Sunday, 7 = Saturday)
      day_index <- as.numeric(format(target_date, "%w")) + 1
      
      # Find Charlottesville row
      cville_row <- which(str_detect(tolower(as.character(data[[1]])), "charlottesville"))[1]
      
      if(!is.na(cville_row)) {
        # Extract values (using detected structure)
        # This is simplified - you'd use the detection logic from main script
        occ_val <- as.numeric(data[cville_row, day_index + 1])
        adr_val <- as.numeric(data[cville_row, day_index + 23])  # Adjust based on format
        revpar_val <- as.numeric(data[cville_row, day_index + 45])  # Adjust based on format
        
        return(list(
          date = target_date,
          file = file_name,
          occupancy = occ_val,
          adr = adr_val,
          revpar = revpar_val
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# Function to validate processed data
validate_processed_data <- function(processed_data, raw_files_dir = "data") {
  cat("=== VALIDATING PROCESSED DATA ===\n\n")
  
  # Select random sample of dates to check
  sample_dates <- processed_data %>%
    sample_n(min(20, nrow(.))) %>%
    pull(Date)
  
  # Get all raw files
  raw_files <- list.files(raw_files_dir, pattern = "Week Ending.*\\.xlsx", 
                          full.names = TRUE, recursive = TRUE)
  
  cat("Found", length(raw_files), "raw Excel files to check against\n\n")
  
  validation_results <- list()
  
  for(check_date in sample_dates) {
    cat("Checking date:", as.character(check_date), "\n")
    
    # Get processed values
    processed_row <- processed_data %>% 
      filter(Date == check_date) %>%
      slice(1)
    
    # Find and check raw file
    for(raw_file in raw_files) {
      raw_values <- check_raw_value(raw_file, check_date)
      
      if(!is.null(raw_values)) {
        # Compare values
        occ_match <- abs(processed_row$Occupancy - raw_values$occupancy) < 1
        adr_match <- abs(processed_row$ADR - raw_values$adr) < 1
        revpar_match <- abs(processed_row$RevPAR - raw_values$revpar) < 1
        
        result <- data.frame(
          Date = check_date,
          Day = processed_row$DayOfWeek,
          Processed_Occ = round(processed_row$Occupancy, 2),
          Raw_Occ = round(raw_values$occupancy, 2),
          Occ_Match = occ_match,
          Processed_ADR = round(processed_row$ADR, 2),
          Raw_ADR = round(raw_values$adr, 2),
          ADR_Match = adr_match,
          Processed_RevPAR = round(processed_row$RevPAR, 2),
          Raw_RevPAR = round(raw_values$revpar, 2),
          RevPAR_Match = revpar_match,
          Source_File = raw_values$file
        )
        
        validation_results[[length(validation_results) + 1]] <- result
        
        if(!occ_match || !adr_match || !revpar_match) {
          cat("  ⚠️ MISMATCH FOUND!\n")
          cat("    Occupancy - Processed:", round(processed_row$Occupancy, 2), 
              "Raw:", round(raw_values$occupancy, 2), "\n")
          cat("    ADR - Processed:", round(processed_row$ADR, 2), 
              "Raw:", round(raw_values$adr, 2), "\n")
          cat("    RevPAR - Processed:", round(processed_row$RevPAR, 2), 
              "Raw:", round(raw_values$revpar, 2), "\n")
        } else {
          cat("  ✅ Values match\n")
        }
        break
      }
    }
  }
  
  # Combine results
  if(length(validation_results) > 0) {
    validation_df <- bind_rows(validation_results)
    
    # Summary
    cat("\n=== VALIDATION SUMMARY ===\n")
    cat("Dates checked:", nrow(validation_df), "\n")
    cat("Occupancy matches:", sum(validation_df$Occ_Match), "/", nrow(validation_df), "\n")
    cat("ADR matches:", sum(validation_df$ADR_Match), "/", nrow(validation_df), "\n")
    cat("RevPAR matches:", sum(validation_df$RevPAR_Match), "/", nrow(validation_df), "\n")
    
    # Calculate match percentage
    match_pct <- mean(validation_df$Occ_Match & validation_df$ADR_Match & validation_df$RevPAR_Match) * 100
    
    if(match_pct == 100) {
      cat("\n✅ PERFECT MATCH! All values align correctly.\n")
    } else if(match_pct >= 90) {
      cat("\n⚠️ GOOD MATCH: ", round(match_pct, 1), "% of values align correctly.\n")
    } else {
      cat("\n❌ POOR MATCH: Only ", round(match_pct, 1), "% of values align correctly.\n")
      cat("Data alignment issues need to be addressed.\n")
    }
    
    # Save validation report
    write_csv(validation_df, paste0("validation_report_", Sys.Date(), ".csv"))
    cat("\nValidation report saved to: validation_report_", Sys.Date(), ".csv\n")
    
    return(validation_df)
  } else {
    cat("No validation results generated.\n")
    return(NULL)
  }
}

# Main validation function
run_validation <- function() {
  cat("Loading processed data...\n")
  cat("Current directory:", getwd(), "\n")
  
  # Check for processed data in finaldata directory
  if(file.exists("finaldata/charlottesville_str_daily_FIXED_2025-08-19.csv")) {
    processed_data <- read_csv("finaldata/charlottesville_str_daily_FIXED_2025-08-19.csv", 
                               show_col_types = FALSE)
  } else {
    # Try to find any processed CSV in finaldata
    csv_files <- list.files("finaldata", pattern = "*.csv", full.names = TRUE)
    if(length(csv_files) > 0) {
      cat("Using file:", basename(csv_files[1]), "\n")
      processed_data <- read_csv(csv_files[1], show_col_types = FALSE)
    } else {
      stop("No processed data found in finaldata directory")
    }
  }
  
  cat("Loaded", nrow(processed_data), "records\n\n")
  
  # Run validation
  validation_results <- validate_processed_data(processed_data)
  
  return(validation_results)
}

# If running interactively, execute the validation
if(interactive()) {
  cat("Starting validation process...\n\n")
  validation_results <- run_validation()
  
  if(!is.null(validation_results)) {
    cat("\n=== SAMPLE RESULTS ===\n")
    print(head(validation_results, 5))
  }
}