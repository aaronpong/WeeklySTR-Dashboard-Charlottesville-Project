# app.R - Main Shiny Application File for Charlottesville STR Dashboard
# Load global environment (libraries, data, functions)
source("global.R")

# Source UI and Server components
source("ui.R")
source("server.R")

# Create and run the Shiny application
shinyApp(ui = ui, server = server)