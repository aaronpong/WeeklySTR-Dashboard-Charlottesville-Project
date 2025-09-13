# WeeklySTR Charlottesville Dashboard

An interactive R Shiny dashboard for comprehensive analysis of short-term rental and hotel performance data in the Charlottesville, Virginia tourism market. This project was developed during a Marketing Data Science internship with Albemarle County to support data-driven decision making in the hospitality industry.

## Project Overview

This dashboard processes multi-year weekly hotel performance reports and provides interactive visualizations for revenue optimization, seasonal analysis, and market trend identification. The system handles varying Excel formats through dynamic column detection algorithms and ensures data accuracy across historical datasets.

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

1. Clone this repository
2. Open `AlbemarleSTRProject.Rproj` in RStudio
3. Install required packages:
   ```r
   install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                      "tidyverse", "lubridate", "readxl"))
   ```
4. Run `START_HERE.R` to launch the dashboard

## Project Structure

```
WeeklySTR-Dashboard-Charlottesville-Project/
├── app/                     # Shiny application files
├── data/Weekly STR/         # STR performance data
├── finaldata/               # Processed data files
├── ScreenShots so Far/      # Dashboard screenshots
├── AlbemarleSTRProject.Rproj # RStudio project file
├── START_HERE.R             # Launch script
└── README.md               # This file
```

## Data Requirements

The dashboard expects weekly STR performance data with the following key metrics:
- Date/Week information
- Occupancy percentages
- Average Daily Rate (ADR)
- Revenue per Available Room (RevPAR)
- Hotel/property identifiers
- Seasonal categorization

## Screenshots

See the `ScreenShots so Far/` folder for dashboard previews and visualizations.

## Future Enhancements

- Integration with real-time data feeds
- Predictive modeling for demand forecasting
- Competitive benchmarking features
- Mobile-responsive design improvements
- Advanced statistical analysis modules

## Contributing

This project was developed as part of an internship program. For questions or suggestions regarding the methodology or implementation, please reach out through the contact information below.

## Contact

Aaron Pongsugree  
571-420-5725 | aaronpong21@gmail.com

## Acknowledgments

Special thanks to Albemarle County for providing the internship opportunity and access to STR performance data for this analysis.
