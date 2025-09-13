# diagnose_columns_fixed.R - Fixed version that handles temp files

library(tidyverse)
library(readxl)
library(lubridate)

# Function to analyze a single file's structure
analyze_file_structure <- function(file_path) {
  
  # Skip temporary files
  if(str_detect(basename(file_path), "^~\\$")) {
    return(NULL)
  }
  
  cat("\n========================================\n")
  cat("Analyzing:", basename(file_path), "\n")
  cat("========================================\n")
  
  tryCatch({
    # Read the file
    data <- read_excel(file_path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
    
    # Find Charlottesville row
    cville_row <- NULL
    for(i in 1:min(20, nrow(data))) {
      if(any(str_detect(tolower(as.character(data[i,])), "charlottesville"))) {
        cville_row <- i
        break
      }
    }
    
    if(is.null(cville_row)) {
      cat("ERROR: Could not find Charlottesville row\n")
      return(NULL)
    }
    
    cat("Charlottesville found at row:", cville_row, "\n\n")
    
    # Look for header rows with day names
    header_row <- NULL
    for(i in 1:min(10, nrow(data))) {
      row_text <- tolower(paste(as.character(data[i,]), collapse = " "))
      if(str_detect(row_text, "sun") && str_detect(row_text, "mon")) {
        header_row <- i
        cat("Header row found at row:", header_row, "\n")
        break
      }
    }
    
    # Extract Charlottesville data
    cville_data <- data[cville_row,]
    
    # Display first 60 columns of Charlottesville data
    cat("\nCharlottesville row data (first 60 columns):\n")
    cat("Columns with values:\n")
    for(i in 1:min(60, ncol(cville_data))) {
      val <- as.character(cville_data[[i]])
      if(!is.na(val) && val != "") {
        # Try to convert to numeric
        num_val <- suppressWarnings(as.numeric(val))
        if(!is.na(num_val)) {
          cat(sprintf("Col %2d: %.2f\n", i, num_val))
        } else {
          cat(sprintf("Col %2d: %s\n", i, val))
        }
      }
    }
    
    # Try to identify metric sections
    cat("\n--- ATTEMPTING TO IDENTIFY METRIC SECTIONS ---\n")
    
    # Look for occupancy values (should be between 0-100)
    cat("\nPotential OCCUPANCY columns (values 0-100):\n")
    occ_cols <- c()
    for(i in 2:min(20, ncol(cville_data))) {
      val <- suppressWarnings(as.numeric(cville_data[[i]]))
      if(!is.na(val) && val >= 0 && val <= 100) {
        cat("  Col", i, ":", round(val, 2), "\n")
        occ_cols <- c(occ_cols, i)
      }
    }
    
    # Look for ADR values (typically 50-500)
    cat("\nPotential ADR columns (values 50-500):\n")
    adr_cols <- c()
    for(i in 2:min(60, ncol(cville_data))) {
      val <- suppressWarnings(as.numeric(cville_data[[i]]))
      if(!is.na(val) && val >= 50 && val <= 500) {
        cat("  Col", i, ":", round(val, 2), "\n")
        adr_cols <- c(adr_cols, i)
      }
    }
    
    # Look for RevPAR values (typically 10-400)
    cat("\nPotential RevPAR columns (values 10-400):\n")
    revpar_cols <- c()
    for(i in 2:min(60, ncol(cville_data))) {
      val <- suppressWarnings(as.numeric(cville_data[[i]]))
      if(!is.na(val) && val >= 10 && val <= 400) {
        cat("  Col", i, ":", round(val, 2), "\n")
        revpar_cols <- c(revpar_cols, i)
      }
    }
    
    # Identify likely column groups
    if(length(occ_cols) >= 7) {
      cat("\n📊 LIKELY OCCUPANCY COLUMNS: ", paste(occ_cols[1:7], collapse=", "), "\n")
    }
    
    # Find ADR columns that are ~20-30 columns after occupancy
    if(length(adr_cols) > 0 && length(occ_cols) > 0) {
      adr_start <- adr_cols[adr_cols > (occ_cols[1] + 15)][1]
      if(!is.na(adr_start)) {
        cat("📊 LIKELY ADR START: Column", adr_start, "\n")
      }
    }
    
    # Extract date from filename
    date_string <- str_extract(basename(file_path), "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}")
    week_ending <- mdy(date_string)
    
    cat("\n--- FILE METADATA ---\n")
    cat("Week ending:", as.character(week_ending), "\n")
    cat("Year:", year(week_ending), "\n")
    cat("Total columns:", ncol(cville_data), "\n")
    cat("Total rows:", nrow(data), "\n")
    
    return(list(
      file = basename(file_path),
      year = year(week_ending),
      week_ending = week_ending,
      cville_row = cville_row,
      header_row = header_row,
      total_cols = ncol(cville_data),
      occ_cols_found = length(occ_cols),
      adr_cols_found = length(adr_cols),
      revpar_cols_found = length(revpar_cols)
    ))
    
  }, error = function(e) {
    cat("ERROR processing file:", e$message, "\n")
    return(NULL)
  })
}

# Function to compare multiple files
compare_file_structures <- function(num_files = 5) {
  cat("=== COMPARING FILE STRUCTURES ===\n")
  
  # Get sample files from different years (excluding temp files)
  all_files <- list.files("data", pattern = "Week Ending.*\\.xlsx$", 
                          full.names = TRUE, recursive = TRUE)
  
  # Filter out temporary files
  all_files <- all_files[!str_detect(basename(all_files), "^~\\$")]
  
  if(length(all_files) == 0) {
    stop("No Excel files found in data directory")
  }
  
  cat("Found", length(all_files), "valid Excel files\n\n")
  
  # Select files from different years if possible
  selected_files <- c()
  
  # Try to get one from each year
  for(year in c("2022", "2023", "2024", "2025")) {
    year_files <- all_files[str_detect(all_files, year)]
    if(length(year_files) > 0) {
      selected_files <- c(selected_files, year_files[1])
    }
  }
  
  # If we don't have enough, just take the first few
  if(length(selected_files) < num_files) {
    selected_files <- all_files[1:min(num_files, length(all_files))]
  }
  
  # Analyze each file
  results <- list()
  for(file in selected_files) {
    result <- analyze_file_structure(file)
    if(!is.null(result)) {
      results[[length(results) + 1]] <- result
    }
  }
  
  # Summary
  if(length(results) > 0) {
    cat("\n\n=== STRUCTURE COMPARISON SUMMARY ===\n")
    summary_df <- bind_rows(results)
    print(summary_df)
  }
  
  return(results)
}

# Function to do a deep dive on one specific file
deep_analyze_file <- function(file_path) {
  
  if(str_detect(basename(file_path), "^~\\$")) {
    cat("Skipping temporary file\n")
    return(NULL)
  }
  
  cat("\n=== DEEP ANALYSIS ===\n")
  cat("File:", basename(file_path), "\n\n")
  
  # Read the file
  data <- read_excel(file_path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  
  # Show first 10 rows to understand structure
  cat("First 10 rows, columns 1-10:\n")
  print(data[1:min(10, nrow(data)), 1:min(10, ncol(data))])
  
  # Find Charlottesville row
  cville_row <- which(str_detect(tolower(as.character(data[[1]])), "charlottesville"))[1]
  
  if(!is.na(cville_row)) {
    cat("\nCharlottesville data (columns 1-60):\n")
    cville_data <- data[cville_row,]
    
    for(i in 1:min(60, ncol(cville_data))) {
      val <- cville_data[[i]]
      if(!is.na(val) && as.character(val) != "") {
        cat(sprintf("Col %2d: %s\n", i, as.character(val)))
      }
    }
    
    # Try to identify patterns
    cat("\n=== PATTERN DETECTION ===\n")
    
    # Find consecutive numeric values that could be 7 days
    numeric_runs <- list()
    run_start <- NULL
    
    for(i in 2:min(60, ncol(cville_data))) {
      val <- suppressWarnings(as.numeric(cville_data[[i]]))
      
      if(!is.na(val)) {
        if(is.null(run_start)) {
          run_start <- i
        }
      } else {
        if(!is.null(run_start) && (i - run_start) >= 7) {
          numeric_runs[[length(numeric_runs) + 1]] <- list(
            start = run_start,
            end = i - 1,
            length = i - run_start
          )
        }
        run_start <- NULL
      }
    }
    
    cat("\nFound", length(numeric_runs), "runs of 7+ consecutive numeric values:\n")
    for(run in numeric_runs) {
      cat("  Columns", run$start, "-", run$end, "(", run$length, "values)\n")
      
      # Show the values
      for(j in run$start:min(run$start + 6, run$end)) {
        val <- as.numeric(cville_data[[j]])
        cat("    Col", j, ":", round(val, 2), "\n")
      }
    }
  }
}

# Quick test function
test_specific_file <- function(year = "2024") {
  all_files <- list.files("data", pattern = paste0("Week Ending.*", year, ".*\\.xlsx$"), 
                          full.names = TRUE, recursive = TRUE)
  
  # Filter out temp files
  all_files <- all_files[!str_detect(basename(all_files), "^~\\$")]
  
  if(length(all_files) > 0) {
    cat("Testing file from", year, ":\n")
    deep_analyze_file(all_files[1])
  } else {
    cat("No files found for year", year, "\n")
  }
}

# Main execution
cat("STR FILE STRUCTURE DIAGNOSTIC TOOL\n")
cat("===================================\n\n")

# Compare structures across years
cat("Comparing file structures across years...\n\n")
results <- compare_file_structures(4)

cat("\n\n")
cat("To do a deep analysis of a specific file, run:\n")
cat("  deep_analyze_file('path/to/your/file.xlsx')\n")
cat("\nTo test a specific year, run:\n")
cat("  test_specific_file('2024')\n")