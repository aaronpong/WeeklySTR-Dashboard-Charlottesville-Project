# data_manip.R - IMPROVED VERSION with Dynamic Format Detection
# This version automatically detects column positions for each file

# Load required libraries
library(tidyverse)
library(readxl)
library(lubridate)

# Function to find all STR Excel files
find_str_files <- function(base_dir = NULL) {
  cat("=== FINDING STR FILES ===\n")
  
  if(is.null(base_dir)) {
    possible_paths <- c(
      "data",
      "../data",
      "../../data",
      "./data",
      "."
    )
  } else {
    possible_paths <- c(base_dir)
  }
  
  for(path in possible_paths) {
    cat("Trying path:", path, "\n")
    
    if(dir.exists(path)) {
      cat("✓ Found directory:", path, "\n")
      
      all_files <- list.files(
        path, 
        pattern = "\\.xlsx?$", 
        full.names = TRUE,
        recursive = TRUE
      )
      
      str_files <- all_files[str_detect(basename(all_files), "Week Ending") & 
                               !str_detect(basename(all_files), "^~\\$")]
      
      if(length(str_files) > 0) {
        cat("✓ Found", length(str_files), "STR files in", path, "\n")
        return(str_files)
      }
    }
  }
  
  cat("No STR files found. Please check your data directory.\n")
  return(c())
}

# Function to detect column positions dynamically
detect_column_structure <- function(data, charlottesville_row_index) {
  cat("Detecting column structure...\n")
  
  # Get header row (usually row 3 or 4)
  header_row <- NULL
  for(i in 1:min(5, nrow(data))) {
    row_text <- tolower(paste(as.character(data[i,]), collapse = " "))
    if(str_detect(row_text, "sun|mon|tue|wed|thu|fri|sat")) {
      header_row <- i
      break
    }
  }
  
  if(is.null(header_row)) {
    cat("Could not find header row with day names\n")
    return(NULL)
  }
  
  # Extract headers
  headers <- as.character(data[header_row,])
  
  # Find day columns (looking for Sun, Mon, Tue, etc.)
  day_patterns <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")
  day_cols <- c()
  
  for(day in day_patterns) {
    col_idx <- which(str_detect(tolower(headers), paste0("^", day)))
    if(length(col_idx) > 0) {
      day_cols <- c(day_cols, col_idx[1])
    }
  }
  
  # If we can't find day columns in headers, use default positions
  if(length(day_cols) != 7) {
    cat("Using default day column positions\n")
    day_cols <- 2:8
  }
  
  # Find metric sections by looking for keywords
  occ_section_start <- which(str_detect(tolower(headers), "occ|occupancy"))[1]
  adr_section_start <- which(str_detect(tolower(headers), "adr|average daily rate"))[1]
  revpar_section_start <- which(str_detect(tolower(headers), "revpar|revenue per"))[1]
  
  # Calculate column positions based on section starts
  structure <- list()
  
  if(!is.na(occ_section_start)) {
    # First occurrence section is typically at columns 2-8
    structure$occupancy_cols <- day_cols
    
    # ADR section typically starts after a gap from occupancy
    if(!is.na(adr_section_start)) {
      # ADR columns follow the same pattern as occupancy but offset
      offset <- adr_section_start - occ_section_start
      structure$adr_cols <- day_cols + offset
    } else {
      # Fallback: ADR usually starts around column 23-24
      structure$adr_cols <- day_cols + 22
    }
    
    # RevPAR section
    if(!is.na(revpar_section_start)) {
      offset <- revpar_section_start - occ_section_start
      structure$revpar_cols <- day_cols + offset
    } else {
      # Fallback: RevPAR usually starts around column 45-46
      structure$revpar_cols <- day_cols + 44
    }
  } else {
    # Use intelligent defaults based on typical STR report structure
    structure$occupancy_cols <- 2:8
    structure$adr_cols <- 24:30
    structure$revpar_cols <- 46:52
  }
  
  # Validate the structure by checking if we have numeric data
  charlottesville_data <- data[charlottesville_row_index,]
  
  # Test if the detected columns have numeric values
  test_occ <- mean(sapply(structure$occupancy_cols, function(i) {
    if(i <= ncol(data)) {
      val <- as.numeric(charlottesville_data[[i]])
      return(!is.na(val) && val >= 0 && val <= 100)
    }
    return(FALSE)
  }))
  
  test_adr <- mean(sapply(structure$adr_cols, function(i) {
    if(i <= ncol(data)) {
      val <- as.numeric(charlottesville_data[[i]])
      return(!is.na(val) && val > 0 && val < 1000)
    }
    return(FALSE)
  }))
  
  if(test_occ < 0.5 || test_adr < 0.5) {
    cat("Detection validation failed, using alternative mapping\n")
    # Try alternative column mapping
    structure$occupancy_cols <- 2:8
    structure$adr_cols <- 12:18
    structure$revpar_cols <- 22:28
  }
  
  cat("Detected columns - Occ:", paste(structure$occupancy_cols, collapse=","), 
      " ADR:", paste(structure$adr_cols, collapse=","),
      " RevPAR:", paste(structure$revpar_cols, collapse=","), "\n")
  
  return(structure)
}

# Improved function to extract Charlottesville data
extract_charlottesville_daily_data <- function(file_path) {
  
  tryCatch({
    file_name <- basename(file_path)
    cat("\nProcessing file:", file_name, "\n")
    
    # Extract date from filename
    date_string <- str_extract(file_name, "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}")
    
    if(is.na(date_string)) {
      cat("Could not extract date from filename\n")
      return(NULL)
    }
    
    week_ending_date <- mdy(date_string)
    
    if(is.na(week_ending_date)) {
      cat("Could not parse date:", date_string, "\n")
      return(NULL)
    }
    
    file_year <- year(week_ending_date)
    cat("Week ending date:", as.character(week_ending_date), "Year:", file_year, "\n")
    
    # Read the Excel file
    suppressMessages({
      data <- read_excel(file_path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
    })
    
    if(is.null(data) || nrow(data) == 0) {
      cat("No data read from file\n")
      return(NULL)
    }
    
    # Find the Charlottesville row
    charlottesville_patterns <- c("charlottesville", "charlottesvil", "cville", "cvb")
    charlottesville_row_index <- NULL
    
    for(pattern in charlottesville_patterns) {
      charlottesville_row_index <- which(str_detect(tolower(as.character(data[[1]])), pattern))
      if(length(charlottesville_row_index) > 0) {
        cat("Found Charlottesville data at row", charlottesville_row_index[1], "\n")
        break
      }
    }
    
    if(length(charlottesville_row_index) == 0) {
      cat("Could not find Charlottesville row\n")
      return(NULL)
    }
    
    charlottesville_row <- data[charlottesville_row_index[1], ]
    
    # Dynamically detect column structure
    col_structure <- detect_column_structure(data, charlottesville_row_index[1])
    
    if(is.null(col_structure)) {
      cat("Could not detect column structure\n")
      return(NULL)
    }
    
    # Extract data using detected structure
    occupancy_cols <- col_structure$occupancy_cols
    adr_cols <- col_structure$adr_cols
    revpar_cols <- col_structure$revpar_cols
    
    # Day names and date calculation
    day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    day_abbrev <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    
    # Calculate dates for the week (Sunday to Saturday)
    week_start_date <- week_ending_date - 6  # Sunday of the week
    
    # Create daily records
    daily_records <- list()
    
    for(i in 1:7) {
      current_date <- week_start_date + (i - 1)
      
      # Extract metrics with validation
      occupancy_val <- if(occupancy_cols[i] <= ncol(data)) {
        val <- as.numeric(charlottesville_row[[occupancy_cols[i]]])
        
        if(!is.na(val) && val >= 0) {
          # Handle both percentage and decimal formats
          if(val <= 1) {
            val * 100
          } else {
            val
          }
        } else {
          NA
        }
      } else {
        NA
      }
      
      adr_val <- if(adr_cols[i] <= ncol(data)) {
        val <- as.numeric(charlottesville_row[[adr_cols[i]]])
        if(!is.na(val) && val > 0 && val < 2000) {
          val
        } else {
          NA
        }
      } else {
        NA
      }
      
      revpar_val <- if(revpar_cols[i] <= ncol(data)) {
        val <- as.numeric(charlottesville_row[[revpar_cols[i]]])
        if(!is.na(val) && val >= 0 && val < 2000) {
          val
        } else {
          NA
        }
      } else {
        NA
      }
      
      # Calculate RevPAR if missing
      if(is.na(revpar_val) && !is.na(occupancy_val) && !is.na(adr_val)) {
        revpar_val <- (occupancy_val / 100) * adr_val
      }
      
      # Validation check
      if(!is.na(occupancy_val) && !is.na(adr_val) && !is.na(revpar_val)) {
        expected_revpar <- (occupancy_val / 100) * adr_val
        if(abs(revpar_val - expected_revpar) > 5) {
          cat("Warning: RevPAR mismatch for", format(current_date, "%Y-%m-%d"), 
              "- Expected:", round(expected_revpar, 2), "Got:", round(revpar_val, 2), "\n")
        }
      }
      
      daily_record <- tibble(
        Market = as.character(charlottesville_row[[1]]),
        Date = current_date,
        DayOfWeek = day_names[i],
        DayAbbrev = day_abbrev[i],
        WeekEnding = week_ending_date,
        FileName = file_name,
        FilePath = file_path,
        Occupancy = occupancy_val,
        ADR = adr_val,
        RevPAR = revpar_val,
        DayIndex = i,
        FileYear = file_year
      )
      
      daily_records[[i]] <- daily_record
    }
    
    week_data <- bind_rows(daily_records)
    
    # Validation summary
    valid_records <- sum(!is.na(week_data$Occupancy) & !is.na(week_data$ADR) & !is.na(week_data$RevPAR))
    cat("Created", nrow(week_data), "records,", valid_records, "with complete data\n")
    
    # Display sample for verification
    if(valid_records > 0) {
      sample_record <- week_data[!is.na(week_data$RevPAR), ][1, ]
      cat("Sample - Date:", as.character(sample_record$Date), 
          "Day:", sample_record$DayOfWeek, 
          "Occ:", round(sample_record$Occupancy, 1), "%",
          "ADR: $", round(sample_record$ADR, 2), 
          "RevPAR: $", round(sample_record$RevPAR, 2), "\n")
    }
    
    return(week_data)
    
  }, error = function(e) {
    cat("Error processing file:", file_path, "\n")
    cat("Error message:", e$message, "\n")
    return(NULL)
  })
}

# Main processing function
process_charlottesville_data <- function() {
  
  cat("=== STARTING IMPROVED DATA PROCESSING ===\n")
  
  str_files <- find_str_files()
  
  if(length(str_files) == 0) {
    stop("No STR files found. Please check your data directory structure.")
  }
  
  cat("Processing", length(str_files), "STR files...\n")
  
  # Process all files
  all_data <- list()
  successful_files <- 0
  failed_files <- 0
  
  for(i in 1:length(str_files)) {
    cat("\n--- Processing file", i, "of", length(str_files), "---")
    
    file_data <- extract_charlottesville_daily_data(str_files[i])
    
    if(!is.null(file_data) && nrow(file_data) > 0) {
      all_data[[i]] <- file_data
      successful_files <- successful_files + 1
    } else {
      failed_files <- failed_files + 1
      cat("Failed to process:", basename(str_files[i]), "\n")
    }
  }
  
  cat("\n\n=== PROCESSING SUMMARY ===\n")
  cat("Successful files:", successful_files, "\n")
  cat("Failed files:", failed_files, "\n")
  
  if(successful_files == 0) {
    stop("No files could be processed successfully")
  }
  
  # Combine all data
  consolidated_data <- bind_rows(all_data)
  
  cat("Total raw records:", nrow(consolidated_data), "\n")
  
  # Add temporal variables and classifications
  final_data <- consolidated_data %>%
    arrange(Date) %>%
    mutate(
      Month = month(Date, label = TRUE),
      Year = year(Date),
      Quarter = quarter(Date),
      WeekOfYear = week(Date),
      MonthDay = paste(month(Date), day(Date), sep = "-"),
      
      Season = case_when(
        Month %in% c("Mar", "Apr", "May") ~ "Spring",
        Month %in% c("Jun", "Jul", "Aug") ~ "Summer", 
        Month %in% c("Sep", "Oct", "Nov") ~ "Fall",
        TRUE ~ "Winter"
      ),
      
      DayType = case_when(
        DayAbbrev %in% c("Fri", "Sat") ~ "Weekend",
        TRUE ~ "Weekday"
      ),
      
      DayCategory = case_when(
        DayAbbrev == "Sun" ~ "Sunday",
        DayAbbrev %in% c("Mon", "Tue", "Wed", "Thu") ~ "Midweek",
        DayAbbrev == "Fri" ~ "Friday", 
        DayAbbrev == "Sat" ~ "Saturday"
      ),
      
      Holiday = case_when(
        MonthDay == "1-1" ~ "New Year's Day",
        MonthDay == "12-31" ~ "New Year's Eve",
        MonthDay == "7-4" ~ "Independence Day",
        MonthDay == "12-25" ~ "Christmas Day",
        MonthDay == "12-24" ~ "Christmas Eve",
        MonthDay == "11-11" ~ "Veterans Day",
        Month == "May" & DayAbbrev == "Mon" & day(Date) >= 25 ~ "Memorial Day",
        Month == "Sep" & DayAbbrev == "Mon" & day(Date) <= 7 ~ "Labor Day",
        Month == "Nov" & DayAbbrev == "Thu" & day(Date) >= 22 & day(Date) <= 28 ~ "Thanksgiving",
        Month == "Nov" & DayAbbrev == "Fri" & day(Date) >= 23 & day(Date) <= 29 ~ "Black Friday",
        TRUE ~ "Regular Day"
      ),
      
      ThreeDayWeekend = case_when(
        # Monday holidays (Memorial Day, Labor Day)
        Holiday %in% c("Memorial Day", "Labor Day") ~ "3-Day Weekend (Monday Holiday)",
        
        # Friday holidays that create 3-day weekends
        Holiday != "Regular Day" & DayAbbrev == "Fri" ~ "3-Day Weekend (Friday Holiday)",
        
        # Sunday before Monday holiday (using lead with default)
        DayAbbrev == "Sun" & 
          lead(Holiday, 1, default = "Regular Day") %in% c("Memorial Day", "Labor Day") ~ "3-Day Weekend (Sunday before Monday Holiday)",
        
        # Saturday before Monday holiday (using lead with default)  
        DayAbbrev == "Sat" & 
          lead(Holiday, 2, default = "Regular Day") %in% c("Memorial Day", "Labor Day") ~ "3-Day Weekend (Saturday before Monday Holiday)",
        
        # Saturday after Friday holiday (using lag with default)
        DayAbbrev == "Sat" & 
          lag(Holiday, 1, default = "Regular Day") != "Regular Day" & 
          lag(DayAbbrev, 1, default = "") == "Fri" ~ "3-Day Weekend (Saturday after Friday Holiday)",
        
        # Sunday after Friday holiday (using lag with default)
        DayAbbrev == "Sun" & 
          lag(Holiday, 2, default = "Regular Day") != "Regular Day" & 
          lag(DayAbbrev, 2, default = "") == "Fri" ~ "3-Day Weekend (Sunday after Friday Holiday)",
        
        TRUE ~ "Regular Weekend"
      )
    ) %>%
    filter(!is.na(Date)) %>%
    filter(!(is.na(Occupancy) & is.na(ADR) & is.na(RevPAR)))
  
  cat("Records after processing:", nrow(final_data), "\n")
  
  # Data quality report
  if(nrow(final_data) > 0) {
    date_range <- range(final_data$Date, na.rm = TRUE)
    
    cat("\n=== DATA QUALITY REPORT ===\n")
    cat("Date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
    
    # Check completeness by year
    yearly_summary <- final_data %>%
      group_by(Year) %>%
      summarise(
        Records = n(),
        Complete_Records = sum(!is.na(Occupancy) & !is.na(ADR) & !is.na(RevPAR)),
        Avg_Occupancy = mean(Occupancy, na.rm = TRUE),
        Avg_ADR = mean(ADR, na.rm = TRUE),
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Year)
    
    cat("\nYearly Summary:\n")
    print(yearly_summary)
    
    # Check for data anomalies
    anomalies <- final_data %>%
      filter(
        Occupancy > 100 | Occupancy < 0 |
          ADR > 1000 | ADR < 10 |
          RevPAR > 1000 | RevPAR < 0
      )
    
    if(nrow(anomalies) > 0) {
      cat("\n⚠️ Found", nrow(anomalies), "records with potential anomalies\n")
    } else {
      cat("\n✅ No data anomalies detected\n")
    }
    
    # NEW (FIXED) 
    # Verify day alignment
    sample_weeks <- final_data %>%
      group_by(WeekEnding) %>%
      summarise(DaysInWeek = n(), .groups = "drop") %>%
      filter(DaysInWeek != 7)
    
    if(nrow(sample_weeks) == 0) {
      cat("\n✅ Day alignment check passed (all weeks have 7 days)\n")
    } else {
      cat("\n⚠️ Day alignment issues found in", nrow(sample_weeks), "weeks\n")
      cat("Weeks with issues:\n")
      print(sample_weeks)
    }
  }
  
  # Create output directory if needed (in parent directory)
  finaldata_path <- "../finaldata"
  if(!dir.exists(finaldata_path)) {
    dir.create(finaldata_path, recursive = TRUE)
  }
  
  # Save the final dataset
  output_file <- file.path(finaldata_path, paste0("charlottesville_str_daily_IMPROVED_", Sys.Date(), ".csv"))
  write_csv(final_data, output_file)
  
  cat("\nData saved to:", output_file, "\n")
  cat("=== DATA PROCESSING COMPLETE ===\n")
  
  return(final_data)
}

# Function to validate against known values
validate_data_quality <- function(data) {
  cat("\n=== DATA VALIDATION ===\n")
  
  # You can add specific date/value combinations to check
  # For example, if you know specific values from the raw files
  test_cases <- list(
    list(date = "2024-07-06", expected_occ = 95, expected_adr = 450),
    list(date = "2024-01-01", expected_occ = 30, expected_adr = 150)
    # Add more test cases as needed
  )
  
  for(test in test_cases) {
    record <- data %>% filter(Date == test$date)
    if(nrow(record) > 0) {
      cat("Testing", test$date, ":\n")
      cat("  Occupancy: Got", round(record$Occupancy, 1), "Expected ~", test$expected_occ, "\n")
      cat("  ADR: Got $", round(record$ADR, 0), "Expected ~ $", test$expected_adr, "\n")
    }
  }
}

# Helper function to get the latest data
get_latest_data <- function() {
  finaldata_dir <- "../finaldata"
  
  if(!dir.exists(finaldata_dir)) {
    return(NULL)
  }
  
  csv_files <- list.files(
    finaldata_dir, 
    pattern = "charlottesville_str_daily.*\\.csv$", 
    full.names = TRUE
  )
  
  if(length(csv_files) == 0) {
    return(NULL)
  }
  
  # Prioritize IMPROVED files
  improved_files <- csv_files[str_detect(basename(csv_files), "IMPROVED")]
  if(length(improved_files) > 0) {
    latest_file <- improved_files[which.max(file.mtime(improved_files))]
  } else {
    latest_file <- csv_files[which.max(file.mtime(csv_files))]
  }
  
  cat("Loading data from:", basename(latest_file), "\n")
  data <- read_csv(latest_file, show_col_types = FALSE)
  
  return(data)
}

# Function to refresh data if needed
refresh_data_if_needed <- function() {
  cat("=== CHECKING IF DATA REFRESH IS NEEDED ===\n")
  return(process_charlottesville_data())
}

cat("=== IMPROVED DATA_MANIP.R LOADED ===\n")
cat("Key improvements:\n")
cat("✅ Dynamic column detection for each file\n")
cat("✅ Automatic format adaptation\n")
cat("✅ Enhanced validation and error checking\n")
cat("✅ Data quality reporting\n")
cat("\nRun process_charlottesville_data() to process all files\n")

