# WeeklySTR Charlottesville Dashboard

An interactive R Shiny dashboard for comprehensive analysis of short-term rental and hotel performance data in the Charlottesville, Virginia tourism market. This project was developed during a Marketing Data Science internship with Albemarle County to support data-driven decision making in the hospitality industry.

## Project Overview

This dashboard processes multi-year weekly hotel performance reports and provides interactive visualizations for revenue optimization, seasonal analysis, and market trend identification. The system handles varying Excel formats through dynamic column detection algorithms and ensures data accuracy across historical datasets.

## Data Privacy & Confidentiality

### ⚠️ Important Note About Data
The actual STR performance data used in this analysis contains **proprietary business information** from Albemarle County's tourism market and is **not included** in this public repository for confidentiality reasons.

### 🔍 What You Can See
This repository demonstrates:
- ✅ **Advanced R programming** and data processing skills
- ✅ **Interactive dashboard development** with Shiny
- ✅ **Statistical analysis methods** and visualization techniques  
- ✅ **Real-world business insights** and recommendations
- ✅ **Professional project documentation** and code organization
- ✅ **Proper data privacy practices** with .gitignore implementation

**The code, methodology, and technical implementation are fully visible while protecting sensitive business data.**

### 📊 Data Structure
The dashboard analyzes STR data with this structure:
```r
# Weekly STR Performance Data Structure
str_data <- tibble(
  date = as.Date("2023-01-01"),     # Weekly reporting date
  occupancy_pct = 75.5,             # Occupancy percentage  
  adr = 120.00,                     # Average Daily Rate ($)
  revpar = 90.60,                   # Revenue per Available Room ($)
  season = "Winter",                # Seasonal classification
  day_of_week = "Sunday"            # Day of week
)
```

## Key Features

### Data Processing
- Automated STR analytics pipeline using R and tidyverse
- Dynamic column detection for handling varying Excel report formats
- Data validation processes to detect and correct historical alignment issues
- Reproducible analytical workflows for ongoing performance monitoring

### Interactive Analysis Modules
- **Seasonal Performance Analysis**: Comprehensive breakdown by seasons with RevPAR, occupancy, and ADR metrics
- **Day-of-Week Patterns**: Identification of weekday vs weekend performance trends
- **Holiday Impact Assessment**: Analysis of national holidays and special events on hotel performance
- **Conference Day Optimization**: Peak performance identification for Tuesday, Wednesday, and Thursday
- **Weekend Performance Tracking**: 3-day holiday weekend analysis and complete weekend comparisons

### Visualizations
- Time series analysis with seasonal decomposition
- Performance distribution by day of week
- Holiday vs regular day comparisons
- Revenue trend analysis over time
- Interactive charts with hover tooltips and filtering capabilities

## Technologies Used

- **R**: Core programming language for data analysis
- **Shiny**: Web application framework for interactive dashboard
- **tidyverse**: Data manipulation and transformation
- **ggplot2**: Static data visualization
- **plotly**: Interactive plotting and charts
- **lubridate**: Date and time handling
- **DT**: Interactive data tables
- **shinydashboard**: Dashboard layout and styling

## Key Insights Discovered

### Performance Patterns
- **Best Performing Season**: Fall (RevPAR: $115.08)
- **Optimization Opportunity**: Winter weekends show significant potential for improvement (RevPAR: $70)
- **Peak Conference Days**: Thursday shows highest performance ($84.02 RevPAR)
- **Consistent Patterns**: Sunday is consistently the worst performing day across all seasons

### Seasonal Trends
- Fall demonstrates strongest overall performance with 69 records and high RevPAR
- Winter presents the biggest opportunity for revenue optimization
- Spring and summer show moderate but stable performance levels
- 3-day holiday weekends perform differently across seasons

### Day-of-Week Analysis
- Saturday shows highest RevPAR ($142.97) but represents only 4.3% of bottom quartile performance
- Weekdays (Monday-Friday) show consistent performance in the $63-84 range
- Weekend premiums are most pronounced during certain seasons

## How to Run

### With Your Own Data:
1. Clone this repository
2. Place your STR Excel files in `data/Weekly STR/` directory
3. Ensure your data matches the expected structure shown above
4. Open `AlbemarleSTRProject.Rproj` in RStudio
5. Install required packages:
   ```r
   install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                      "tidyverse", "lubridate", "readxl"))
   ```
6. Run `START_HERE.R` to launch the dashboard

### Code Review:
- All R scripts and dashboard code are available for examination
- Documentation and comments explain the data processing methodology
- Visualization code demonstrates advanced ggplot2 and plotly techniques

## Project Structure

```
WeeklySTR-Dashboard-Charlottesville-Project/
├── app/                          # Shiny application files
├── .gitignore                    # Privacy protection for sensitive files
├── ScreenShots so Far/           # Dashboard screenshots and visualizations
├── AlbemarleSTRProject.Rproj     # RStudio project file
├── START_HERE.R                  # Launch script
├── test_debug.R                  # Development and testing scripts
├── INSTRUCTIONS_R Setup and Dashboard Guide.pdf  # Setup documentation
└── README.md                     # This file

# Protected/Private (not in repository):
# ├── data/Weekly STR/            # Actual STR performance data
# ├── finaldata/                  # Processed data files
```

## Screenshots

See the `ScreenShots so Far/` folder for dashboard previews and visualizations showing the types of analysis and insights generated.

## Technical Achievements

This project demonstrates proficiency in:
- **Data Engineering**: Automated processing of inconsistent Excel formats
- **Statistical Analysis**: Time series analysis, seasonal decomposition, performance metrics
- **Interactive Visualization**: Responsive Shiny dashboards with complex filtering
- **Business Intelligence**: Translation of technical analysis into actionable business insights
- **Professional Development**: Proper version control, documentation, and data privacy practices

## Future Enhancements

- Integration with real-time data feeds
- Predictive modeling for demand forecasting
- Competitive benchmarking features
- Mobile-responsive design improvements
- Advanced statistical analysis modules

## Contact

Aaron Pongsugree  
571-420-5725 | aaronpong21@gmail.com

## Acknowledgments

Special thanks to Albemarle County for providing the internship opportunity and access to STR performance data for this analysis. This project demonstrates professional data handling practices while showcasing technical capabilities in data science and dashboard development.
