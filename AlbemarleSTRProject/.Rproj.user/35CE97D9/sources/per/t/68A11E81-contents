# test_data_quality.R - Comprehensive testing suite for STR data

library(tidyverse)
library(lubridate)
library(testthat)

# Function to run all data quality tests
run_data_quality_tests <- function(data) {
  
  cat("=== RUNNING DATA QUALITY TESTS ===\n\n")
  
  results <- list()
  
  # Test 1: Date Continuity
  cat("Test 1: Checking date continuity...\n")
  date_gaps <- data %>%
    arrange(Date) %>%
    mutate(gap = as.numeric(Date - lag(Date))) %>%
    filter(gap > 1) %>%
    select(Date, gap)
  
  if(nrow(date_gaps) > 0) {
    cat("  ⚠️ Found", nrow(date_gaps), "date gaps\n")
    results$date_gaps <- date_gaps
  } else {
    cat("  ✅ No date gaps found\n")
    results$date_gaps <- NULL
  }
  
  # Test 2: Day of Week Alignment
  cat("\nTest 2: Verifying day of week alignment...\n")
  day_mismatches <- data %>%
    mutate(
      expected_day = weekdays(Date),
      match = (DayOfWeek == expected_day)
    ) %>%
    filter(!match)
  
  if(nrow(day_mismatches) > 0) {
    cat("  ❌ Found", nrow(day_mismatches), "day of week mismatches\n")
    cat("  This indicates serious date alignment issues!\n")
    results$day_mismatches <- day_mismatches %>% 
      select(Date, DayOfWeek, expected_day)
  } else {
    cat("  ✅ All days of week align correctly\n")
    results$day_mismatches <- NULL
  }
  
  # Test 3: Value Range Validation
  cat("\nTest 3: Checking value ranges...\n")
  out_of_range <- data %>%
    filter(
      Occupancy < 0 | Occupancy > 100 |
        ADR < 0 | ADR > 2000 |
        RevPAR < 0 | RevPAR > 2000
    )
  
  if(nrow(out_of_range) > 0) {
    cat("  ⚠️ Found", nrow(out_of_range), "records with out-of-range values\n")
    results$out_of_range <- out_of_range
  } else {
    cat("  ✅ All values within expected ranges\n")
    results$out_of_range <- NULL
  }
  
  # Test 4: RevPAR Calculation Check
  cat("\nTest 4: Verifying RevPAR calculations...\n")
  revpar_issues <- data %>%
    filter(!is.na(Occupancy) & !is.na(ADR) & !is.na(RevPAR)) %>%
    mutate(
      calculated_revpar = (Occupancy / 100) * ADR,
      difference = abs(RevPAR - calculated_revpar),
      pct_difference = (difference / RevPAR) * 100
    ) %>%
    filter(difference > 1)  # Allow $1 tolerance
  
  if(nrow(revpar_issues) > 0) {
    cat("  ⚠️ Found", nrow(revpar_issues), "records with RevPAR calculation issues\n")
    cat("  Average difference: $", round(mean(revpar_issues$difference), 2), "\n")
    results$revpar_issues <- revpar_issues %>%
      select(Date, DayOfWeek, Occupancy, ADR, RevPAR, calculated_revpar, difference)
  } else {
    cat("  ✅ All RevPAR calculations are consistent\n")
    results$revpar_issues <- NULL
  }
  
  # Test 5: Duplicate Detection
  cat("\nTest 5: Checking for duplicates...\n")
  duplicates <- data %>%
    group_by(Date) %>%
    filter(n() > 1) %>%
    arrange(Date)
  
  if(nrow(duplicates) > 0) {
    cat("  ⚠️ Found", nrow(duplicates), "duplicate records\n")
    results$duplicates <- duplicates
  } else {
    cat("  ✅ No duplicate dates found\n")
    results$duplicates <- NULL
  }
  
  # Test 6: Seasonal Pattern Check
  cat("\nTest 6: Checking seasonal patterns...\n")
  seasonal_stats <- data %>%
    group_by(Season, DayType) %>%
    summarise(
      avg_occupancy = mean(Occupancy, na.rm = TRUE),
      avg_adr = mean(ADR, na.rm = TRUE),
      avg_revpar = mean(RevPAR, na.rm = TRUE),
      record_count = n(),
      .groups = "drop"
    )
  
  # Check if winter really is the worst season
  winter_revpar <- seasonal_stats %>% 
    filter(Season == "Winter") %>% 
    pull(avg_revpar) %>% 
    mean()
  
  other_seasons_revpar <- seasonal_stats %>% 
    filter(Season != "Winter") %>% 
    pull(avg_revpar) %>% 
    mean()
  
  if(winter_revpar < other_seasons_revpar) {
    cat("  ✅ Winter shows lowest RevPAR as expected\n")
  } else {
    cat("  ⚠️ Winter RevPAR pattern unexpected\n")
  }
  
  results$seasonal_stats <- seasonal_stats
  
  # Test 7: Weekend vs Weekday Pattern
  cat("\nTest 7: Checking weekend vs weekday patterns...\n")
  day_type_stats <- data %>%
    group_by(DayType) %>%
    summarise(
      avg_occupancy = mean(Occupancy, na.rm = TRUE),
      avg_adr = mean(ADR, na.rm = TRUE),
      avg_revpar = mean(RevPAR, na.rm = TRUE),
      .groups = "drop"
    )
  
  weekend_revpar <- day_type_stats %>% 
    filter(DayType == "Weekend") %>% 
    pull(avg_revpar)
  
  weekday_revpar <- day_type_stats %>% 
    filter(DayType == "Weekday") %>% 
    pull(avg_revpar)
  
  if(weekend_revpar > weekday_revpar) {
    cat("  ✅ Weekends show higher RevPAR as expected\n")
  } else {
    cat("  ⚠️ Weekend/weekday pattern unexpected\n")
  }
  
  results$day_type_stats <- day_type_stats
  
  # Test 8: Sunday Performance Check
  cat("\nTest 8: Checking Sunday performance...\n")
  sunday_stats <- data %>%
    filter(DayOfWeek == "Sunday") %>%
    summarise(
      avg_occupancy = mean(Occupancy, na.rm = TRUE),
      avg_adr = mean(ADR, na.rm = TRUE),
      avg_revpar = mean(RevPAR, na.rm = TRUE),
      record_count = n()
    )
  
  other_days_revpar <- data %>%
    filter(DayOfWeek != "Sunday") %>%
    summarise(avg_revpar = mean(RevPAR, na.rm = TRUE)) %>%
    pull(avg_revpar)
  
  if(sunday_stats$avg_revpar < other_days_revpar) {
    cat("  ✅ Sunday shows lower RevPAR as expected\n")
  } else {
    cat("  ⚠️ Sunday performance pattern unexpected\n")
  }
  
  results$sunday_stats <- sunday_stats
  
  # Generate Summary Report
  cat("\n=== TEST SUMMARY ===\n")
  
  issues_found <- sum(
    !is.null(results$date_gaps),
    !is.null(results$day_mismatches),
    !is.null(results$out_of_range),
    !is.null(results$revpar_issues),
    !is.null(results$duplicates)
  )
  
  if(issues_found == 0) {
    cat("✅ ALL TESTS PASSED! Data quality looks excellent.\n")
  } else {
    cat("⚠️ Found issues in", issues_found, "out of 8 test categories.\n")
    cat("Review the detailed results for more information.\n")
  }
  
  return(results)
}

# Function to generate detailed report
generate_quality_report <- function(test_results, output_file = NULL) {
  
  if(is.null(output_file)) {
    output_file <- paste0("data_quality_report_", Sys.Date(), ".txt")
  }
  
  sink(output_file)
  
  cat("STR DATA QUALITY REPORT\n")
  cat("Generated:", Sys.Date(), "\n")
  cat("=" , strrep("=", 50), "\n\n")
  
  # Print each test result
  if(!is.null(test_results$day_mismatches)) {
    cat("DAY OF WEEK MISMATCHES (CRITICAL):\n")
    print(test_results$day_mismatches)
    cat("\n")
  }
  
  if(!is.null(test_results$revpar_issues)) {
    cat("REVPAR CALCULATION ISSUES:\n")
    print(head(test_results$revpar_issues, 10))
    cat("\n")
  }
  
  if(!is.null(test_results$date_gaps)) {
    cat("DATE GAPS:\n")
    print(test_results$date_gaps)
    cat("\n")
  }
  
  cat("SEASONAL STATISTICS:\n")
  print(test_results$seasonal_stats)
  cat("\n")
  
  cat("DAY TYPE STATISTICS:\n")
  print(test_results$day_type_stats)
  cat("\n")
  
  sink()
  
  cat("Report saved to:", output_file, "\n")
}

# Main execution
if(interactive()) {
  # Load the data
  cat("Loading data...\n")
  hotel_data <- read_csv("../finaldata/charlottesville_str_daily_IMPROVED_2025-08-19.csv", 
                         show_col_types = FALSE)
  
  # Run tests
  test_results <- run_data_quality_tests(hotel_data)
  
  # Generate report
  generate_quality_report(test_results)
}