# ui.R 

dashboardPage(
  
  # Dashboard Header
  dashboardHeader(
    title = "Charlottesville STR Analytics Dashboard"
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Day Performance", tabName = "daily", icon = icon("calendar")),
      menuItem("Seasonal Analysis", tabName = "seasonal", icon = icon("leaf")),
      menuItem("Holiday Analysis", tabName = "holidays", icon = icon("gift")),
      menuItem("Detailed Data", tabName = "data", icon = icon("table")),
      menuItem("Research Analysis", tabName = "research", icon = icon("chart-line"))
    ),
    
    # Filters section
    hr(),
    h4("Filters", style = "margin-left: 15px;"),
    
    # Date range filter - FIXED VERSION
    div(style = "margin: 15px;",
        dateRangeInput("date_range", 
                       "Date Range:",
                       start = as.Date("2022-01-01"),
                       end = as.Date("2025-12-31"))
    ),
    
    # Season filter
    div(style = "margin: 15px;",
        checkboxGroupInput("season_filter", 
                           "Seasons:",
                           choices = c("Spring", "Summer", "Fall", "Winter"),
                           selected = c("Spring", "Summer", "Fall", "Winter"))
    ),
    
    # Day type filter
    div(style = "margin: 15px;",
        checkboxGroupInput("day_type_filter", 
                           "Day Types:",
                           choices = c("Weekend", "Weekday"),
                           selected = c("Weekend", "Weekday"))
    ),
    hr(),
    h4("Data Definitions", style = "margin-left: 15px;"),
    
    # Season definitions
    div(style = "margin: 15px;",
        h5("Seasons:"),
        tags$ul(style = "font-size: 12px; margin-left: 10px;",
                tags$li("Spring: March 1 - May 31"),
                tags$li("Summer: June 1 - August 31"), 
                tags$li("Fall: September 1 - November 30"),
                tags$li("Winter: December 1 - February 28/29")
        )
    ),
    # 3-Day Weekend Reference
    div(style = "margin: 15px;",
        h5("3-Day Holiday Weekends by Season:"),
        tags$ul(style = "font-size: 12px; margin-left: 10px;",
                tags$li(tags$b("Spring:"), "Memorial Day (last Monday in May) - creates Sat-Sun-Mon weekend"),
                tags$li(tags$b("Summer:"), "Independence Day (July 4) - creates 3-day weekend when it falls on Friday or Monday"),
                tags$li(tags$b("Fall:"), "Labor Day (first Monday in September) - creates Sat-Sun-Mon weekend"),
                tags$li(tags$b("Winter:"), "New Year's Day (January 1) - creates 3-day weekend when it falls on Friday or Monday")
        ),
        tags$p(style = "font-size: 11px; font-style: italic; margin-left: 10px;", 
               "Note: Veterans Day (November 11) can also create 3-day weekends in Fall when it falls on Friday or Monday")
    ),   
    # Holiday definitions  
    div(style = "margin: 15px;",
        h5("Holidays Analyzed:"),
        tags$ul(style = "font-size: 12px; margin-left: 10px;",
                tags$li("New Year's Day/Eve"),
                tags$li("Memorial Day (Last Mon in May)"),
                tags$li("Independence Day (July 4)"),
                tags$li("Labor Day (First Mon in Sep)"),
                tags$li("Thanksgiving + Black Friday"),
                tags$li("Christmas Eve/Day"),
                tags$li("Veterans Day (November 11)")
        )
    ),
    # Refresh data button
    div(style = "margin: 15px;",
        actionButton("refresh_data", "Refresh Data", 
                     icon = icon("refresh"),
                     style = "color: white; background-color: #3c8dbc;")
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    
    # JavaScript for Research Questions Tab
    tags$script(HTML("
      $(document).ready(function() {
        function updateButtonStates(activeButton) {
          $('#q1_btn, #q2_btn, #q3_btn, #q4_btn, #q5_btn, #q6_btn')
            .removeClass('btn-primary').addClass('btn-default');
          
          if(activeButton) {
            $('#' + activeButton).removeClass('btn-default').addClass('btn-primary');
          }
          
          var questionTexts = {
            'q1_btn': 'Analyzing: Worst Day of Week',
            'q2_btn': 'Analyzing: Slow Spring/Fall Weeks', 
            'q3_btn': 'Analyzing: 3-Day Holiday Weekends',
            'q4_btn': 'Analyzing: Worst National Holidays',
            'q5_btn': 'Analyzing: Worst Weekends',
            'q6_btn': 'Analyzing: Best Conference Days'
          };
          
          var indicatorText = questionTexts[activeButton] || 'Click a question above to view analysis';
          $('#current_question_indicator h6').text(indicatorText);
        }
        
        $('#q1_btn').click(function() { updateButtonStates('q1_btn'); });
        $('#q2_btn').click(function() { updateButtonStates('q2_btn'); });
        $('#q3_btn').click(function() { updateButtonStates('q3_btn'); });
        $('#q4_btn').click(function() { updateButtonStates('q4_btn'); });
        $('#q5_btn').click(function() { updateButtonStates('q5_btn'); });
        $('#q6_btn').click(function() { updateButtonStates('q6_btn'); });
      });
    ")),
    
    # CSS Styling
    tags$style(HTML("
      .btn-primary {
        background-color: #337ab7 !important;
        border-color: #2e6da4 !important;
        color: white !important;
      }
      
      .research-btn {
        margin-bottom: 8px; 
        text-align: left;
        transition: all 0.3s ease;
      }
    ")),
    
    tabItems(
      
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                # Value boxes for key metrics
                valueBoxOutput("total_records"),
                valueBoxOutput("date_range_box"),
                valueBoxOutput("avg_revpar")
              ),
              
              fluidRow(
                box(
                  title = "RevPAR Over Time", status = "primary", solidHeader = TRUE,
                  width = 8, height = 450,
                  plotlyOutput("revpar_timeline", height = "380px")
                ),
                box(
                  title = "Key Insights", status = "info", solidHeader = TRUE,
                  width = 4, height = 400,
                  h4("Worst Performing Day:"),
                  textOutput("worst_day"),
                  br(),
                  h4("Best Season:"),
                  textOutput("best_season"),
                  br(),
                  h4("Conference Peak:"),
                  textOutput("conference_peak")
                )
              ),
              
              fluidRow(
                box(
                  title = "Performance by Day of Week", status = "primary", solidHeader = TRUE,
                  width = 12, height = 450,
                  plotlyOutput("day_of_week_plot", height = "380px")
                )
              )
      ),
      
      # Daily Performance Tab
      tabItem(tabName = "daily",
              fluidRow(
                box(
                  title = "Day of Week Performance", status = "primary", solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("daily_boxplot")
                ),
                box(
                  title = "Day Performance Summary", status = "info", solidHeader = TRUE,
                  width = 4,
                  DT::dataTableOutput("day_summary_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Weekday vs Weekend by Season", status = "primary", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("weekday_weekend_plot")
                ),
                box(
                  title = "Sunday Performance Analysis", status = "warning", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("sunday_analysis_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Bottom 25% Performance Analysis", status = "danger", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("bottom_quartile_table")
                )
              )
      ),
      
      # Seasonal Analysis Tab  
      tabItem(tabName = "seasonal",
              fluidRow(
                box(
                  title = "Monthly Performance", status = "primary", solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("monthly_performance")
                ),
                box(
                  title = "Seasonal Summary", status = "info", solidHeader = TRUE,
                  width = 4,
                  DT::dataTableOutput("seasonal_summary_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Seasonal Trends by Metric", status = "primary", solidHeader = TRUE,
                  width = 12,
                  selectInput("metric_choice", "Choose Metric:",
                              choices = c("RevPAR", "Occupancy", "ADR"),
                              selected = "RevPAR"),
                  plotlyOutput("seasonal_trends")
                )
              )
      ),
      
      # Holiday Analysis Tab
      tabItem(tabName = "holidays",
              fluidRow(
                box(
                  title = "Worst Individual Days (Bottom 20)", status = "danger", solidHeader = TRUE,
                  width = 8,
                  DT::dataTableOutput("worst_days_table")
                ),
                box(
                  title = "Holiday Performance Summary", status = "warning", solidHeader = TRUE,
                  width = 4,
                  DT::dataTableOutput("holiday_summary_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "3-Day Weekend Sunday Performance", status = "primary", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("three_day_weekend_plot")
                ),
                box(
                  title = "Holiday vs Regular Day Performance", status = "info", solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("holiday_comparison_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "New Year Period Analysis", status = "warning", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("new_year_analysis_table")
                )
              )
      ),
      
      # Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Filtered Hotel Data", status = "primary", solidHeader = TRUE,
                  width = 12,
                  div(style = "margin-bottom: 10px;",
                      downloadButton("download_data", "Download Filtered Data", 
                                     class = "btn-primary")
                  ),
                  DT::dataTableOutput("data_table")
                )
              )
      ),
      
      # Research Analysis Tab
      tabItem(tabName = "research",
              
              fluidRow(
                # Question Navigation Sidebar
                column(3,
                       box(
                         title = "Research Questions", status = "primary", solidHeader = TRUE,
                         width = NULL,
                         
                         # Question Selection Buttons
                         div(style = "margin-bottom: 10px;",
                             h5("Select a question to explore:")
                         ),
                         
                         actionButton("q1_btn", "Q1: Worst Day of Week", 
                                      class = "btn-block btn-default", 
                                      style = "margin-bottom: 8px; text-align: left;"),
                         
                         actionButton("q2_btn", "Q2: Slow Spring/Fall Weeks", 
                                      class = "btn-block btn-default", 
                                      style = "margin-bottom: 8px; text-align: left;"),
                         
                         actionButton("q3_btn", "Q3: 3-Day Holiday Weekends", 
                                      class = "btn-block btn-default", 
                                      style = "margin-bottom: 8px; text-align: left;"),
                         
                         actionButton("q4_btn", "Q4: Worst National Holidays", 
                                      class = "btn-block btn-default", 
                                      style = "margin-bottom: 8px; text-align: left;"),
                         
                         actionButton("q5_btn", "Q5: Worst Weekends", 
                                      class = "btn-block btn-default", 
                                      style = "margin-bottom: 8px; text-align: left;"),
                         
                         actionButton("q6_btn", "Q6: Best Conference Days", 
                                      class = "btn-block btn-default", 
                                      style = "margin-bottom: 8px; text-align: left;"),
                         
                         hr(),
                         
                         # Current Question Indicator
                         div(id = "current_question_indicator",
                             h6("Click a question above to view analysis", 
                                style = "color: #666; font-style: italic;")
                         )
                       )
                ),
                
                # Main Content Area
                column(9,
                       # Question Content (conditionally rendered)
                       conditionalPanel(
                         condition = "output.selected_question == 'q1'",
                         box(
                           title = "Q1: Do we have a day of the week that is consistently the worst?", 
                           status = "danger", solidHeader = TRUE, width = NULL,
                           
                           div(style = "background-color: #f4f4f4; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
                               h4(style = "color: #d73927; margin-top: 0;", "✓ ANSWER: Yes, Sunday is consistently your worst day"),
                               
                           ),
                           
                           # Supporting Visualization
                           plotlyOutput("q1_visualization", height = "400px"),
                           
                           # Actionable Recommendations
                           div(style = "background-color: #dff0d8; padding: 15px; margin-top: 15px; border-radius: 5px;",
                               h5(style = "color: #3c763d; margin-top: 0;", "💡 ACTIONABLE RECOMMENDATIONS:"),
                               tags$ul(
                                 tags$li("Develop Sunday arrival packages with extended stay incentives"),
                                 tags$li("Target Sunday-Monday promotional rates for leisure travelers"),
                                 tags$li("Consider Sunday as your 'soft opening' day for maintenance/training")
                               )
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.selected_question == 'q2'",
                         box(
                           title = "Q2: Are there slow weeks during spring and fall, especially midweek?", 
                           status = "warning", solidHeader = TRUE, width = NULL,
                           
                           div(style = "background-color: #fcf8e3; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
                               h4(style = "color: #8a6d3b; margin-top: 0;", "✓ ANSWER: Yes, November and March weekdays are slowest"),
                               
                           ),
                           
                           plotlyOutput("q2_visualization", height = "400px"),
                           
                           div(style = "background-color: #dff0d8; padding: 15px; margin-top: 15px; border-radius: 5px;",
                               h5(style = "color: #3c763d; margin-top: 0;", "💡 ACTIONABLE RECOMMENDATIONS:"),
                               tags$ul(
                                 tags$li("Launch targeted corporate packages for March and November"),
                                 tags$li("Focus digital marketing spend on these slower months"),
                                 tags$li("Offer midweek meeting packages with AV equipment included"),
                                 tags$li("Partner with local businesses for conference hosting")
                               )
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.selected_question == 'q3'",
                         box(
                           title = "Q3: How do 3-day holiday weekends perform? Sunday occupancy?", 
                           status = "info", solidHeader = TRUE, width = NULL,
                           
                           div(style = "background-color: #d9edf7; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
                               h4(style = "color: #31708f; margin-top: 0;", "✓ ANSWER: Mixed results - depends on season"),
                               
                           ),
                           
                           plotlyOutput("q3_visualization", height = "400px"),
                           
                           div(style = "background-color: #dff0d8; padding: 15px; margin-top: 15px; border-radius: 5px;",
                               h5(style = "color: #3c763d; margin-top: 0;", "💡 ACTIONABLE RECOMMENDATIONS:"),
                               tags$ul(
                                 tags$li("Maximize summer 3-day weekend pricing - demand is there"),
                                 tags$li("Create winter holiday packages to combat poor performance"),
                                 tags$li("Market extended stay deals for 3-day weekend arrivals"),
                                 tags$li("Partner with local attractions for winter holiday activities")
                               )
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.selected_question == 'q4'",
                         box(
                           title = "Q4: Which national holidays perform worst?", 
                           status = "danger", solidHeader = TRUE, width = NULL,
                           
                           div(style = "background-color: #f4f4f4; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
                               h4(style = "color: #d73927; margin-top: 0;", "✓ ANSWER: Christmas holidays consistently perform worst"),
                              
                           ),
                           
                           plotlyOutput("q4_visualization", height = "400px"),
                           
                           div(style = "background-color: #dff0d8; padding: 15px; margin-top: 15px; border-radius: 5px;",
                               h5(style = "color: #3c763d; margin-top: 0;", "💡 ACTIONABLE RECOMMENDATIONS:"),
                               tags$ul(
                                 tags$li("Create New Year's Eve party packages with local venues"),
                                 tags$li("Offer January detox/wellness packages post-holidays"),
                                 tags$li("Consider closing or limited service during worst performing days"),
                                 tags$li("Partner with Richmond hotels to capture overflow during their peak")
                               )
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.selected_question == 'q5'",
                         box(
                           title = "Q5: Which weekends are worst in the year?", 
                           status = "danger", solidHeader = TRUE, width = NULL,
                           
                           div(style = "background-color: #f4f4f4; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
                               h4(style = "color: #d73927; margin-top: 0;", "✓ ANSWER: Winter weekends dramatically worst"),
                               
                           ),
                           
                           plotlyOutput("q5_visualization", height = "400px"),
                           
                           div(style = "background-color: #dff0d8; padding: 15px; margin-top: 15px; border-radius: 5px;",
                               h5(style = "color: #3c763d; margin-top: 0;", "💡 ACTIONABLE RECOMMENDATIONS:"),
                               tags$ul(
                                 tags$li("HUGE OPPORTUNITY: Winter weekend strategy needed"),
                                 tags$li("Create winter escape packages with local wineries"),
                                 tags$li("Market to Richmond/DC residents for weekend getaways"),
                                 tags$li("Partner with UVA for winter events and basketball games"),
                                 tags$li("Consider spa packages or indoor activity partnerships")
                               )
                           )
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.selected_question == 'q6'",
                         box(
                           title = "Q6: What weekdays and months perform best for conferences?", 
                           status = "success", solidHeader = TRUE, width = NULL,
                           
                           div(style = "background-color: #dff0d8; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
                               h4(style = "color: #3c763d; margin-top: 0;", "✓ ANSWER: Wednesday peak, October/April seasons"),
                           ),
                           
                           plotlyOutput("q6_visualization", height = "400px"),
                           
                           div(style = "background-color: #d1ecf1; padding: 15px; margin-top: 15px; border-radius: 5px;",
                               h5(style = "color: #0c5460; margin-top: 0;", "💡 ACTIONABLE RECOMMENDATIONS:"),
                               tags$ul(
                                 tags$li("Premium pricing for Wednesday group bookings"),
                                 tags$li("Target corporate sales efforts in October and April"),
                                 tags$li("Develop Tuesday-Thursday conference packages"),
                                 tags$li("Study UVA academic calendar for correlation opportunities"),
                                 tags$li("Create conference arrival packages (Monday-Tuesday)")
                               )
                           )
                         )
                       ),
                       
                       # Default view when no question is selected
                       conditionalPanel(
                         condition = "output.selected_question == '' || output.selected_question == null",
                         box(
                           title = "Research Questions Analysis", status = "primary", solidHeader = TRUE, width = NULL,
                           div(style = "text-align: center; padding: 40px;",
                               icon("chart-line", style = "font-size: 48px; color: #337ab7; margin-bottom: 20px;"),
                               h3("Select a Research Question"),
                               p("Click on any question in the sidebar to view detailed analysis and actionable recommendations."),
                               hr(),
                               h4("Available Questions:"),
                               tags$ol(
                                 tags$li("Worst performing day of the week"),
                                 tags$li("Slow periods during spring and fall"),
                                 tags$li("3-day holiday weekend performance"),
                                 tags$li("Worst performing national holidays"),
                                 tags$li("Worst performing weekends"),
                                 tags$li("Best conference days and months")
                               )
                           )
                         )
                       )
                )
              )
      )
    )
  )
)