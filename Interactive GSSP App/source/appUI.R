library(shiny)
library(shinydashboard)
library(leaflet)
library(threejs)
library(htmlwidgets)
source("global.R", local = TRUE)

# Create a Shiny app UI
appUI <- shinyUI({
  
  dashboardPage(
    dashboardHeader(title = "Interactive GSSP App"),
    
    # Sidebar navigation
    dashboardSidebar(
      sidebarMenu(
        # GSSP Marker map
        menuItem(
          "GSSP Map", 
          tabName = "map", 
          icon = icon("map")
        ),
        # GPlates Reconstruction map
        menuItem(
          "GPlates Map",
          tabName = "gplates",
          icon = icon("globe")
        ),

        # interactive globe
        menuItem(
          "Globe",
          tabName = "globe",
          icon = icon("globe")
        ),
        
        # About page
        menuItem(
          "About", 
          tabName = "about", 
          icon = icon("info")
        )
      )
    ),
    
    # Page contents
    dashboardBody(
      tabItems(
        # GSSP Marker Map
        tabItem(
          tabName = "map",
          # leafletOutput(outputId = "leaflet", height = 900)
          # editor: Jiyin
          globeOutput(outputId = "globejs", height = 900)
        ),
        # GPlates Reconstruction Map
        tabItem(
          tabName = "gplates",
            selectInput(
              inputId = "gplatesconcept",
              label = "Geographical Concept",
              choices = conceptsWithCoords$concept
            ),
            tags$head(
              tags$style(HTML(".leaflet-container { background: #aad3df; }"))
            ),
            leafletOutput(outputId = "gplatesleaflet", height = 900)
        ),
        
        # GSSP Interactive Globe
        tabItem(
          tabName = "globe",
          # uiOutput(outputId = "gplateDemo", height = 900),
          includeHTML("/Users/blc/rspace/DTKB-AppsTest/JS/gplates-demo.html")
          # fluidPage(
          #   includeHTML("/Users/blc/rspace/DTKB-AppsTest/JS/gplates-demo.html")
          # )
        ),

        # About page
        # Contains link to Github repository with source code
        tabItem(
          tabName = "about",
          h1("About"),
          h5("The source code and a document outlining the UI and server of this app are available at the link below."),
          tags$a(href="https://github.com/lotkey/DTKB-Apps", "https://github.com/lotkey/DTKB-Apps")
        )
      )
    )
  )
})