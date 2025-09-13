# reprocess_data.R - Script to reprocess data with corrected column mapping

library(tidyverse)
library(readxl) 
library(lubridate)

cat("=== REPROCESSING WITH CORRECTED DATA_MANIP.R ===\n")
cat("Current directory:", getwd(), "\n")

# Check if the corrected data_manip.R file exists
if(file.exists("app/data_manip.R")) {
  cat("\n✓ Found data_manip.R in current directory\n")
  
  # Source the corrected functions
  source("app/data_manip.R")
  
  cat("\n=== RUNNING CORRECTED DATA PROCESSING ===\n")
  cat("🔧 Using corrected column mapping for 2024+ files\n")
  
  # Run the corrected processing
  tryCatch({
    corrected_data <- refresh_data_if_needed()
    
    if(!is.null(corrected_data) && nrow(corrected_data) > 0) {
      cat("\n🎉 SUCCESS! Data reprocessed with corrected column mapping\n")
      cat("Total records:", nrow(corrected_data), "\n")
      
      # Test the May 18, 2024 record
      may18_test <- corrected_data %>% filter(Date == "2024-05-18")
      if(nrow(may18_test) > 0) {
        cat("\n🎯 Verification (May 18, 2024 Saturday):\n")
        cat("   Occupancy:", round(may18_test$Occupancy, 2), "%\n")
        cat("   ADR: $", round(may18_test$ADR, 2), "\n")
        cat("   RevPAR: $", round(may18_test$RevPAR, 2), "\n")
        
        # Check if the correction worked
        if(may18_test$ADR > 400) {
          cat("✅ SUCCESS: Data appears corrected (Saturday ADR > $400)!\n")
          cat("📊 The column mapping fix is working correctly!\n")
        } else {
          cat("⚠️ Warning: ADR still seems low for Saturday ($", round(may18_test$ADR, 2), ")\n")
          cat("Expected around $473.67 for this date\n")
        }
      }
      
      # Show weekend performance improvement
      weekend_summary <- corrected_data %>%
        filter(DayOfWeek %in% c("Friday", "Saturday"), Year >= 2024) %>%
        group_by(DayOfWeek) %>%
        summarise(
          Count = n(),
          Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
          Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      if(nrow(weekend_summary) > 0) {
        cat("\n📈 2024+ Weekend Performance (Corrected):\n")
        for(i in 1:nrow(weekend_summary)) {
          cat("   ", weekend_summary$DayOfWeek[i], ": ADR=$", weekend_summary$Avg_ADR[i], 
              ", RevPAR=$", weekend_summary$Avg_RevPAR[i], 
              " (", weekend_summary$Count[i], " records)\n")
        }
      }
      
      # Show where the file was saved
      cat("\n📁 Corrected data saved to finaldata directory\n")
      cat("✅ You can now restart your Shiny dashboard to use the corrected data\n")
      
    } else {
      cat("❌ Error: No data was processed\n")
    }
    
  }, error = function(e) {
    cat("❌ Error during processing:", e$message, "\n")
    cat("\n🔧 TROUBLESHOOTING:\n")
    cat("1. Check that your data/ directory exists and contains Excel files\n")
    cat("2. Make sure the Excel files have 'Week Ending' in their names\n")  
    cat("3. Verify the files contain Charlottesville data\n")
    cat("4. Check file paths in the error message above\n")
  })
  
} else {
  cat("\n❌ data_manip.R not found in current directory\n")
  cat("\n🔧 NEXT STEPS:\n")
  cat("1. Make sure you've replaced your data_manip.R file with the corrected version\n")
  cat("2. Current directory: ", getwd(), "\n")
  cat("3. Files in this directory:\n")
  
  # Show user what files are available
  files <- list.files(".", pattern = "\\.(R|r)$")
  if(length(files) > 0) {
    for(file in files) {
      cat("   -", file, "\n")
    }
  } else {
    cat("   No R files found\n")
  }
}

cat("\n=== REPROCESSING COMPLETE ===\n")

