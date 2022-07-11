library(shiny)
source("app_ui.R", local = TRUE)
source("app_server.R", local = TRUE)

# Run Shiny app from app_ui() in app_ui.R and app_server() in app_server.R
shinyApp(ui = app_ui, server = app_server)