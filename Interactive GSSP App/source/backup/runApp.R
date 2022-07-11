library(shiny)
# library(rjson)
# setwd("/Users/blc/rspace/DTKB-AppsTest/Interactive GSSP App/source")
print(getwd())
source("./Interactive GSSP App/source/appUI.R", local = TRUE)
source("./Interactive GSSP App/source/appServer.R", local = TRUE)

# Create a Shiny app from appUI() (appUI.R) and appServer() (appServer.R)
shinyApp(ui = appUI, server = appServer)