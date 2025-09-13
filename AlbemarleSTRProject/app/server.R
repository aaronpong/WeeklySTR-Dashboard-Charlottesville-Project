# server.R - Complete Server Logic for Charlottesville STR Dashboard

function(input, output, session) {
  
  # Update date range when app starts based on actual data
  observe({
    if(!is.null(hotel_data) && nrow(hotel_data) > 0) {
      actual_start <- min(hotel_data$Date, na.rm = TRUE)
      actual_end <- max(hotel_data$Date, na.rm = TRUE)
      
      updateDateRangeInput(session, "date_range",
                           start = actual_start,
                           end = actual_end)
      
      # Debug output
      cat("Updated date range to:", as.character(actual_start), "through", as.character(actual_end), "\n")
      cat("Total records available:", nrow(hotel_data), "\n")
      
      # Show data summary
      yearly_summary <- hotel_data %>%
        group_by(Year = year(Date)) %>%
        summarise(Records = n(), .groups = "drop") %>%
        arrange(Year)
      
      cat("Data by year:\n")
      for(i in 1:nrow(yearly_summary)) {
        cat(" ", yearly_summary$Year[i], ":", yearly_summary$Records[i], "records\n")
      }
    } else {
      cat("No hotel data available for date range update\n")
    }
  })
  
  # ===== REACTIVE DATA =====
  
  # Reactive data based on filters
  filtered_data <- reactive({
    
    if(is.null(hotel_data)) {
      cat("hotel_data is NULL in filtered_data reactive\n")
      return(NULL)
    }
    
    if(nrow(hotel_data) == 0) {
      cat("hotel_data has 0 rows in filtered_data reactive\n")
      return(NULL)
    }
    
    data <- hotel_data %>%
      filter(
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        Season %in% input$season_filter,
        DayType %in% input$day_type_filter
      )
    
    cat("Filtered data: ", nrow(data), " records from ", 
        as.character(input$date_range[1]), " to ", as.character(input$date_range[2]), "\n")
    
    return(data)
  })
  
  # Refresh data when button is clicked
  observeEvent(input$refresh_data, {
    showNotification("Refreshing data...", type = "message")
    
    # Reload data
    hotel_data <<- refresh_data_if_needed()
    
    # Update summaries
    if(!is.null(hotel_data)) {
      showNotification(paste("Data refreshed successfully!", nrow(hotel_data), "records loaded"), type = "message")
      
      # Update date range again
      actual_start <- min(hotel_data$Date, na.rm = TRUE)
      actual_end <- max(hotel_data$Date, na.rm = TRUE)
      updateDateRangeInput(session, "date_range", start = actual_start, end = actual_end)
      
    } else {
      showNotification("Error refreshing data!", type = "error")
    }
  })
  
  # ===== VALUE BOXES =====
  
  output$total_records <- renderValueBox({
    valueBox(
      value = if(!is.null(filtered_data())) nrow(filtered_data()) else 0,
      subtitle = "Total Records",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$date_range_box <- renderValueBox({
    if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      start_date <- format(min(filtered_data()$Date), "%m/%d/%y")
      end_date <- format(max(filtered_data()$Date), "%m/%d/%y")
      date_range <- paste0(start_date, " to ", end_date)
    } else {
      date_range <- "No data"
    }
    
    valueBox(
      value = date_range,
      subtitle = "Date Range",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$avg_revpar <- renderValueBox({
    avg_revpar <- if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      paste0("$", round(mean(filtered_data()$RevPAR, na.rm = TRUE), 2))
    } else {
      "N/A"
    }
    
    valueBox(
      value = avg_revpar,
      subtitle = "Average RevPAR",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  # ===== INSIGHTS TEXT OUTPUTS =====
  
  output$worst_day <- renderText({
    if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      worst <- filtered_data() %>%
        group_by(DayOfWeek) %>%
        summarise(Avg_RevPAR = mean(RevPAR, na.rm = TRUE)) %>%
        arrange(Avg_RevPAR) %>%
        slice(1)
      paste0(worst$DayOfWeek, " ($", round(worst$Avg_RevPAR, 2), ")")
    } else {
      "No data available"
    }
  })
  
  output$best_season <- renderText({
    if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      best <- filtered_data() %>%
        group_by(Season) %>%
        summarise(Avg_RevPAR = mean(RevPAR, na.rm = TRUE)) %>%
        arrange(desc(Avg_RevPAR)) %>%
        slice(1)
      paste0(best$Season, " ($", round(best$Avg_RevPAR, 2), ")")
    } else {
      "No data available"
    }
  })
  
  output$conference_peak <- renderText({
    if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      peak <- filtered_data() %>%
        filter(DayType == "Weekday") %>%
        group_by(DayOfWeek) %>%
        summarise(Avg_RevPAR = mean(RevPAR, na.rm = TRUE)) %>%
        arrange(desc(Avg_RevPAR)) %>%
        slice(1)
      paste0(peak$DayOfWeek, " ($", round(peak$Avg_RevPAR, 2), ")")
    } else {
      "No data available"
    }
  })
  
  # ===== PLOTS =====
  
  # Helper function for empty plots
  plotly_empty <- function() {
    plotly::plot_ly() %>%
      plotly::add_annotations(
        text = "No data available for current filters",
        showarrow = FALSE,
        font = list(size = 16)
      ) %>%
      plotly::layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  }
  
  # Day of Week Plot - FIXED VERSION
  output$day_of_week_plot <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    
    p <- filtered_data() %>%
      mutate(DayOfWeek = factor(DayOfWeek, levels = day_order)) %>%
      group_by(DayOfWeek, DayType) %>%
      summarise(
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        Avg_Occupancy = mean(Occupancy, na.rm = TRUE),
        Avg_ADR = mean(ADR, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = DayOfWeek, y = Avg_RevPAR, fill = DayType)) +
      geom_col(position = "dodge") +
      labs(
        title = "Average RevPAR by Day of Week",
        x = "Day of Week",
        y = "Average RevPAR ($)",
        fill = "Day Type"
      ) +
      theme_minimal() +
      theme(
        plot.margin = margin(10, 10, 40, 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p) %>%
      layout(
        margin = list(l = 50, r = 50, b = 80, t = 50),
        autosize = TRUE,
        xaxis = list(
          showticklabels = TRUE,
          tickangle = -45
        ),
        yaxis = list(
          showticklabels = TRUE
        )
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d")
      )
  })
  
  # RevPAR Timeline Plot
  
  output$revpar_timeline <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Define consistent colors
    season_colors <- c(
      "Fall" = "#F8766D",      # Red  
      "Spring" = "#7CAE00",    # Green
      "Summer" = "#00BFC4",    # Cyan
      "Winter" = "#C77CFF"     # Purple
    )
    
    # Create clean scatter plot
    p <- filtered_data() %>%
      arrange(Date) %>%
      ggplot(aes(x = Date, y = RevPAR, color = Season)) +
      geom_point(alpha = 0.7, size = 1.5) +
      scale_color_manual(values = season_colors) +
      labs(
        title = "RevPAR Over Time",
        x = "Date",
        y = "RevPAR ($)",
        color = "Season"
      ) +
      theme_minimal() +
      theme(
        plot.margin = margin(10, 10, 40, 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
    
    # Convert to plotly
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(
        margin = list(l = 50, r = 100, b = 80, t = 50),
        autosize = TRUE,
        xaxis = list(
          showticklabels = TRUE,
          tickangle = -45
        ),
        yaxis = list(
          showticklabels = TRUE
        ),
        legend = list(
          title = list(text = "Season"),
          orientation = "v",
          x = 1.02,
          y = 1
        )
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d")
      )
  })
  
  # Daily Performance Boxplot
  output$daily_boxplot <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    day_order <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    
    p <- filtered_data() %>%
      mutate(DayAbbrev = factor(DayAbbrev, levels = day_order)) %>%
      ggplot(aes(x = DayAbbrev, y = RevPAR, fill = DayType)) +
      geom_boxplot() +
      labs(
        title = "RevPAR Distribution by Day of Week",
        x = "Day of Week",
        y = "RevPAR ($)",
        fill = "Day Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Day Summary Table
  output$day_summary_table <- DT::renderDataTable({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(data.frame())
    }
    
    day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    
    filtered_data() %>%
      group_by(DayOfWeek) %>%
      summarise(
        Records = n(),
        Avg_Occ = round(mean(Occupancy, na.rm = TRUE), 1),
        Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
        Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(DayOfWeek = factor(DayOfWeek, levels = day_order)) %>%
      arrange(DayOfWeek)
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  # Weekday vs Weekend Plot
  output$weekday_weekend_plot <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    p <- filtered_data() %>%
      group_by(Season, DayType) %>%
      summarise(Avg_RevPAR = mean(RevPAR, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = Season, y = Avg_RevPAR, fill = DayType)) +
      geom_col(position = "dodge") +
      labs(
        title = "Weekday vs Weekend Performance by Season",
        x = "Season",
        y = "Average RevPAR ($)",
        fill = "Day Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Sunday Analysis Plot
  output$sunday_analysis_plot <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    p <- filtered_data() %>%
      filter(DayOfWeek == "Sunday") %>%
      group_by(Season) %>%
      summarise(
        Avg_Occupancy = mean(Occupancy, na.rm = TRUE),
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = Season, y = Avg_Occupancy)) +
      geom_col(fill = "lightblue") +
      labs(
        title = "Sunday Occupancy by Season",
        x = "Season",
        y = "Average Occupancy (%)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Bottom 25% Analysis Table
  output$bottom_quartile_table <- DT::renderDataTable({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(data.frame())
    }
    
    # Calculate quartiles
    occupancy_q25 <- quantile(filtered_data()$Occupancy, 0.25, na.rm = TRUE)
    adr_q25 <- quantile(filtered_data()$ADR, 0.25, na.rm = TRUE)
    revpar_q25 <- quantile(filtered_data()$RevPAR, 0.25, na.rm = TRUE)
    
    bottom_quartile <- filtered_data() %>%
      group_by(DayOfWeek) %>%
      summarise(
        Total_Records = n(),
        Bottom25_Occ = sum(Occupancy <= occupancy_q25),
        Bottom25_ADR = sum(ADR <= adr_q25),
        Bottom25_RevPAR = sum(RevPAR <= revpar_q25),
        .groups = "drop"
      ) %>%
      mutate(
        Pct_Bottom25_Occ = round(Bottom25_Occ / Total_Records * 100, 1),
        Pct_Bottom25_ADR = round(Bottom25_ADR / Total_Records * 100, 1),
        Pct_Bottom25_RevPAR = round(Bottom25_RevPAR / Total_Records * 100, 1)
      ) %>%
      select(DayOfWeek, Total_Records, Pct_Bottom25_Occ, Pct_Bottom25_ADR, Pct_Bottom25_RevPAR) %>%
      arrange(desc(Pct_Bottom25_RevPAR))
    
    bottom_quartile
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  # Monthly Performance Plot
  output$monthly_performance <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    p <- filtered_data() %>%
      mutate(Month = factor(Month, levels = month_order)) %>%
      group_by(Month, Season) %>%
      summarise(Avg_RevPAR = mean(RevPAR, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = Month, y = Avg_RevPAR, fill = Season)) +
      geom_col() +
      labs(
        title = "Monthly RevPAR Performance",
        x = "Month",
        y = "Average RevPAR ($)",
        fill = "Season"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Seasonal Summary Table
  output$seasonal_summary_table <- DT::renderDataTable({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(data.frame())
    }
    
    filtered_data() %>%
      group_by(Season) %>%
      summarise(
        Records = n(),
        Avg_Occ = round(mean(Occupancy, na.rm = TRUE), 1),
        Avg_ADR = round(mean(ADR, na.rm = TRUE), 2),
        Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(Avg_RevPAR))
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  # Seasonal Trends Plot
  output$seasonal_trends <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    metric_col <- switch(input$metric_choice,
                         "RevPAR" = "RevPAR",
                         "Occupancy" = "Occupancy", 
                         "ADR" = "ADR")
    
    p <- filtered_data() %>%
      ggplot(aes_string(x = "Date", y = metric_col, color = "Season")) +
      geom_point(alpha = 0.4) +
      geom_smooth(se = FALSE) +
      labs(
        title = paste(input$metric_choice, "Trends by Season"),
        x = "Date",
        y = input$metric_choice,
        color = "Season"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===== HOLIDAY ANALYSES =====
  
  # Worst Individual Days Table
  output$worst_days_table <- DT::renderDataTable({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(data.frame())
    }
    
    filtered_data() %>%
      arrange(RevPAR) %>%
      select(Date, DayOfWeek, Season, Month, Holiday, Occupancy, ADR, RevPAR) %>%
      head(20) %>%
      mutate(
        Occupancy = paste0(round(Occupancy, 1), "%"),
        ADR = paste0("$", round(ADR, 2)),
        RevPAR = paste0("$", round(RevPAR, 2))
      )
  }, options = list(pageLength = 20, dom = 't'), rownames = FALSE)
  
  # Holiday Summary Table
  output$holiday_summary_table <- DT::renderDataTable({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(data.frame())
    }
    
    holiday_summary <- filtered_data() %>%
      filter(Holiday != "Regular Day") %>%
      group_by(Holiday) %>%
      summarise(
        Count = n(),
        Avg_Occ = round(mean(Occupancy, na.rm = TRUE), 1),
        Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
        Min_RevPAR = round(min(RevPAR, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(Avg_RevPAR)
    
    holiday_summary
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  # 3-Day Weekend Plot
  output$three_day_weekend_plot <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    weekend_data <- filtered_data() %>%
      filter(DayOfWeek == "Sunday") %>%
      mutate(
        WeekendType = case_when(
          str_detect(ThreeDayWeekend, "3-Day") ~ "3-Day Weekend",
          TRUE ~ "Regular Sunday"
        )
      ) %>%
      group_by(Season, WeekendType) %>%
      summarise(
        Count = n(),
        Avg_Occupancy = mean(Occupancy, na.rm = TRUE),
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Count >= 2)
    
    if(nrow(weekend_data) == 0) {
      return(plotly_empty())
    }
    
    p <- weekend_data %>%
      ggplot(aes(x = Season, y = Avg_Occupancy, fill = WeekendType)) +
      geom_col(position = "dodge") +
      labs(
        title = "Sunday Occupancy: 3-Day Weekends vs Regular Sundays",
        x = "Season",
        y = "Average Occupancy (%)",
        fill = "Weekend Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Holiday Comparison Plot
  output$holiday_comparison_plot <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    comparison_data <- filtered_data() %>%
      mutate(
        DayCategory = case_when(
          Holiday != "Regular Day" ~ "Holiday",
          TRUE ~ "Regular Day"
        )
      ) %>%
      group_by(DayCategory, Season) %>%
      summarise(
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- comparison_data %>%
      ggplot(aes(x = Season, y = Avg_RevPAR, fill = DayCategory)) +
      geom_col(position = "dodge") +
      labs(
        title = "Holiday vs Regular Day Performance by Season",
        x = "Season",
        y = "Average RevPAR ($)",
        fill = "Day Type"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # New Year Analysis Table
  output$new_year_analysis_table <- DT::renderDataTable({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(data.frame())
    }
    
    new_year_data <- filtered_data() %>%
      filter(Holiday %in% c("New Year's Eve", "New Year's Day")) %>%
      arrange(RevPAR) %>%
      select(Date, DayOfWeek, Holiday, Occupancy, ADR, RevPAR) %>%
      mutate(
        Occupancy = paste0(round(Occupancy, 1), "%"),
        ADR = paste0("$", round(ADR, 2)),
        RevPAR = paste0("$", round(RevPAR, 2))
      )
    
    new_year_data
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  # Data Table
  output$data_table <- DT::renderDataTable({
    if(is.null(filtered_data())) {
      return(data.frame())
    }
    
    filtered_data() %>%
      select(Date, DayOfWeek, Season, Month, Occupancy, ADR, RevPAR, DayType) %>%
      arrange(desc(Date))
  }, options = list(pageLength = 25, scrollX = TRUE))
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("charlottesville_str_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # ===== RESEARCH QUESTIONS TAB LOGIC =====
  
  # Research Questions Tab Logic
  values <- reactiveValues(selected_question = "")
  
  # Button click handlers
  observeEvent(input$q1_btn, { values$selected_question <- "q1" })
  observeEvent(input$q2_btn, { values$selected_question <- "q2" })
  observeEvent(input$q3_btn, { values$selected_question <- "q3" })
  observeEvent(input$q4_btn, { values$selected_question <- "q4" })
  observeEvent(input$q5_btn, { values$selected_question <- "q5" })
  observeEvent(input$q6_btn, { values$selected_question <- "q6" })
  
  # Pass selected question to client for conditional panels
  output$selected_question <- reactive({ values$selected_question })
  outputOptions(output, "selected_question", suspendWhenHidden = FALSE)
  
  # Q1: Worst Day of Week Visualization
  output$q1_visualization <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Create day summary with emphasis on Sunday being worst
    day_summary <- filtered_data() %>%
      group_by(DayOfWeek) %>%
      summarise(
        Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
        Records = n(),
        Bottom25_Count = sum(RevPAR <= quantile(filtered_data()$RevPAR, 0.25, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        Bottom25_Pct = round(Bottom25_Count / Records * 100, 1),
        DayOfWeek = factor(DayOfWeek, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
        IsWorst = DayOfWeek == "Sunday"
      )
    
    p <- ggplot(day_summary, aes(x = DayOfWeek, y = Avg_RevPAR, fill = IsWorst)) +
      geom_col() +
      geom_text(aes(label = paste0("$", Avg_RevPAR, "\n(", Bottom25_Pct, "% bottom 25%)")), 
                vjust = -0.5, size = 3) +
      scale_fill_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c")) +
      labs(title = "Sunday is Consistently the Worst Performing Day",
           subtitle = "RevPAR and Bottom 25% frequency shown",
           x = "Day of Week", y = "Average RevPAR ($)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, max(day_summary$Avg_RevPAR) * 1.15)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(l = 50, r = 50, b = 80, t = 50))
  })
  
  # Q2: Slow Spring/Fall Weeks Visualization
  output$q2_visualization <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Focus on weekday performance by month for spring/fall
    month_weekday_data <- filtered_data() %>%
      filter(DayType == "Weekday", Season %in% c("Spring", "Fall")) %>%
      group_by(Month, Season) %>%
      summarise(
        Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
        Records = n(),
        .groups = "drop"
      ) %>%
      mutate(
        Month = factor(Month, levels = c("Mar", "Apr", "May", "Sep", "Oct", "Nov")),
        IsSlowest = Month %in% c("Mar", "Nov")
      )
    
    p <- ggplot(month_weekday_data, aes(x = Month, y = Avg_RevPAR, fill = IsSlowest)) +
      geom_col() +
      geom_text(aes(label = paste0("$", Avg_RevPAR)), vjust = -0.5) +
      scale_fill_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#e67e22")) +
      facet_wrap(~Season, scales = "free_x") +
      labs(title = "March and November Weekdays Are the Slowest",
           subtitle = "Spring and Fall weekday performance comparison",
           x = "Month", y = "Average RevPAR ($)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(margin = list(l = 50, r = 50, b = 80, t = 50))
  })
  
  # Q3: 3-Day Weekend Performance Visualization
  output$q3_visualization <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Get all 3-day weekend data (all days, not just Sundays)
    three_day_weekend_data <- filtered_data() %>%
      filter(str_detect(ThreeDayWeekend, "3-Day Weekend")) %>%
      group_by(Season) %>%
      summarise(
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        Avg_Occupancy = mean(Occupancy, na.rm = TRUE),
        Avg_ADR = mean(ADR, na.rm = TRUE),
        Total_Days = n(),
        Unique_Weekends = n_distinct(WeekEnding),
        .groups = "drop"
      ) %>%
      mutate(
        # Add performance ranking
        Performance_Rank = rank(-Avg_RevPAR),
        # Color by performance level
        Performance_Level = case_when(
          Avg_RevPAR > 80 ~ "Strong",
          Avg_RevPAR > 60 ~ "Moderate",
          TRUE ~ "Weak"
        )
      )
    
    # Also get regular weekend data for comparison context
    regular_weekend_data <- filtered_data() %>%
      filter(DayOfWeek %in% c("Friday", "Saturday", "Sunday"), 
             !str_detect(ThreeDayWeekend, "3-Day Weekend")) %>%
      group_by(Season) %>%
      summarise(
        Regular_Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Combine for comparison
    comparison_data <- three_day_weekend_data %>%
      left_join(regular_weekend_data, by = "Season") %>%
      mutate(
        Performance_vs_Regular = Avg_RevPAR - Regular_Avg_RevPAR,
        Better_than_Regular = Performance_vs_Regular > 0
      )
    
    if(nrow(comparison_data) == 0) {
      return(plotly_empty())
    }
    
    # Create the chart
    p <- ggplot(comparison_data, aes(x = reorder(Season, -Avg_RevPAR), y = Avg_RevPAR, fill = Performance_Level)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0("$", round(Avg_RevPAR, 0), "\n(", Unique_Weekends, " weekends, ", Total_Days, " days)")), 
                vjust = -0.5, size = 3.2, fontface = "bold") +
      scale_fill_manual(
        values = c("Strong" = "#27ae60", "Moderate" = "#f39c12", "Weak" = "#e74c3c"),
        name = "Performance Level"
      ) +
      labs(
        title = "3-Day Holiday Weekend Performance by Season",
        subtitle = "Average RevPAR across all 3-day weekend days (Fri-Sat-Sun or Sat-Sun-Mon)",
        x = "Season", 
        y = "Average RevPAR ($)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        legend.position = "right"
      ) +
      ylim(0, max(comparison_data$Avg_RevPAR, na.rm = TRUE) * 1.2)
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        margin = list(l = 50, r = 100, b = 80, t = 50)
      )
  })
  
  # Q4: Worst Holidays Visualization - IMPROVED
  output$q4_visualization <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Filter to ONLY national holidays and calculate averages
    holiday_performance <- filtered_data() %>%
      filter(Holiday != "Regular Day") %>%  # Only actual holidays
      group_by(Holiday) %>%
      summarise(
        Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        Count = n(),
        Min_RevPAR = min(RevPAR, na.rm = TRUE),
        Max_RevPAR = max(RevPAR, na.rm = TRUE),
        # Add day of week info
        Days_of_Week = paste(unique(substr(weekdays(filtered_data()$Date[filtered_data()$Holiday == first(Holiday)]), 1, 3)), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(Avg_RevPAR) %>%  # Worst to best (ascending)
      mutate(
        IsNewYear = str_detect(Holiday, "New Year"),
        # Better categories
        HolidayCategory = case_when(
          str_detect(Holiday, "New Year") ~ "New Year Period",
          str_detect(Holiday, "Christmas") ~ "Christmas Period", 
          Holiday %in% c("Memorial Day", "Labor Day", "Independence Day") ~ "Summer Holidays",
          TRUE ~ "Other Holidays"
        ),
        # Create display labels with sample size
        HolidayLabel = paste0(Holiday, "\n(", Count, " days)")
      )
    
    if(nrow(holiday_performance) == 0) {
      return(plotly_empty())
    }
    
    # Custom color palette
    colors <- c(
      "New Year Period" = "#e74c3c",      # Red
      "Christmas Period" = "#9b59b6",     # Purple  
      "Summer Holidays" = "#27ae60",      # Green
      "Other Holidays" = "#3498db"        # Blue
    )
    
    p <- ggplot(holiday_performance, aes(x = reorder(HolidayLabel, Avg_RevPAR), y = Avg_RevPAR, fill = HolidayCategory)) +
      geom_col() +
      geom_text(aes(label = paste0("$", round(Avg_RevPAR, 0))), hjust = -0.1, size = 3.5) +
      scale_fill_manual(values = colors) +
      coord_flip() +
      labs(
        title = "National Holiday Performance: Average RevPAR Comparison",
        subtitle = "Ranked from worst to best performing holidays",
        x = "Holiday", 
        y = "Average RevPAR ($)", 
        fill = "Holiday Category"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      ) +
      # Add range indicators showing best/worst year for each holiday
      geom_errorbar(aes(ymin = Min_RevPAR, ymax = Max_RevPAR), 
                    width = 0.3, alpha = 0.5, color = "gray30")
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        margin = list(l = 180, r = 120, b = 80, t = 80),
        showlegend = TRUE
      )
  })
  
  # Q5: Worst Weekends Visualization - COMPLETE WEEKENDS
  output$q5_visualization <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Calculate weekend performance (Friday + Saturday combined)
    weekend_performance <- filtered_data() %>%
      filter(DayOfWeek %in% c("Friday", "Saturday")) %>%
      # Group by weekend pairs (same week ending date)
      group_by(Season, WeekEnding) %>%
      summarise(
        Weekend_Avg_RevPAR = mean(RevPAR, na.rm = TRUE),
        Weekend_Avg_Occupancy = mean(Occupancy, na.rm = TRUE),
        Weekend_Avg_ADR = mean(ADR, na.rm = TRUE),
        Days_in_Weekend = n(), # Should be 2 for complete weekends
        .groups = "drop"
      ) %>%
      # Only include complete weekends (both Friday and Saturday data)
      filter(Days_in_Weekend == 2) %>%
      # Calculate seasonal averages
      group_by(Season) %>%
      summarise(
        Avg_Weekend_RevPAR = mean(Weekend_Avg_RevPAR, na.rm = TRUE),
        Avg_Weekend_Occupancy = mean(Weekend_Avg_Occupancy, na.rm = TRUE),
        Avg_Weekend_ADR = mean(Weekend_Avg_ADR, na.rm = TRUE),
        Complete_Weekends = n(),
        .groups = "drop"
      ) %>%
      mutate(
        IsWorstSeason = Season == "Winter",
        # Add performance context
        Performance_Level = case_when(
          Avg_Weekend_RevPAR > 150 ~ "Strong",
          Avg_Weekend_RevPAR > 100 ~ "Moderate", 
          TRUE ~ "Weak"
        )
      )
    
    if(nrow(weekend_performance) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(weekend_performance, aes(x = reorder(Season, -Avg_Weekend_RevPAR), y = Avg_Weekend_RevPAR, fill = IsWorstSeason)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0("$", round(Avg_Weekend_RevPAR, 0), "\n(", Complete_Weekends, " weekends)")), 
                vjust = -0.5, size = 3.5, fontface = "bold") +
      scale_fill_manual(
        values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
        labels = c("FALSE" = "Other Seasons", "TRUE" = "Worst Season")
      ) +
      labs(
        title = "Complete Weekend Performance by Season",
        subtitle = "Average RevPAR for Friday + Saturday combined",
        x = "Season", 
        y = "Average Weekend RevPAR ($)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        legend.position = "none"
      ) +
      # Add opportunity annotation for winter
      annotate("text", x = which(weekend_performance$Season == "Winter"), 
               y = max(weekend_performance$Avg_Weekend_RevPAR) * 0.85, 
               label = "HUGE\nOPPORTUNITY", 
               color = "red", fontface = "bold", size = 4) +
      ylim(0, max(weekend_performance$Avg_Weekend_RevPAR) * 1.2)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        margin = list(l = 50, r = 50, b = 100, t = 50),
        showlegend = FALSE
      )
  })
  
  # Q6: Best Conference Days Visualization
  output$q6_visualization <- renderPlotly({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      return(plotly_empty())
    }
    
    # Conference corridor analysis (Tue-Wed-Thu) by month
    conference_data <- filtered_data() %>%
      filter(DayOfWeek %in% c("Tuesday", "Wednesday", "Thursday")) %>%
      group_by(Month, DayOfWeek) %>%
      summarise(
        Avg_RevPAR = round(mean(RevPAR, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(
        Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
        IsWednesday = DayOfWeek == "Wednesday",
        IsPrimeMonth = Month %in% c("Oct", "Apr")
      )
    
    p <- ggplot(conference_data, aes(x = Month, y = Avg_RevPAR, fill = DayOfWeek)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = ifelse(DayOfWeek == "Wednesday", paste0("$", Avg_RevPAR), "")), 
                position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
      labs(title = "Wednesday Peak + October/April Conference Seasons",
           subtitle = "Conference corridor (Tue-Wed-Thu) performance by month",
           x = "Month", y = "Average RevPAR ($)", fill = "Conference Day") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      annotate("rect", xmin = 3.7, xmax = 4.3, ymin = 90, ymax = 110, 
               alpha = 0.3, fill = "green") +
      annotate("rect", xmin = 9.7, xmax = 10.3, ymin = 100, ymax = 120, 
               alpha = 0.3, fill = "green")
    
    ggplotly(p) %>%
      layout(margin = list(l = 50, r = 50, b = 80, t = 50))
  })
  
}
