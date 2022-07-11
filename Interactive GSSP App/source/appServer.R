library(shiny)
library(leaflet)
library(threejs)
library(htmlwidgets)
source("map.R", local = TRUE)
source("global.R", local = TRUE)
source("gPlates.R", local = TRUE)

# Shiny app server
appServer <- function(input, output, session) {
  
  # Plot the GSSP Marker map
  #   using the map() function defined in map.R
  output$globejs <- renderGlobe({
    map()
  })
  
  # output$gplateDemo <- renderUI({
  #   fluidPage(
  #     includeHTML("/Users/blc/rspace/DTKB-AppsTest/JS/gplates-demo.html")
  #     # includeCSS("/Users/blc/rspace/DTKB-AppsTest/JS/gplates-demo.html")
  #   )
  # })


  
  # Plot the GPlates Reconstruction map
  output$gplatesleaflet <- renderLeaflet({
    
    # Gather input
    #   using a variable conceptsWithCoords from global.R
    index <- match(input$gplatesconcept, conceptsWithCoords$concept)
    concept <- conceptsWithCoords$concept[index]
    time <- conceptsWithCoords$time[index]
    long <- conceptsWithCoords$longitude[index]
    lat <- conceptsWithCoords$latitude[index]
    
    # Reconstruction models only support specific ranges of time
    # Select model based on time
    if (time <= 1100) {
      if (time <= 750) {
        getLeafletMap(concept, time, long, lat, "PALEOMAP")
      }
      else {
        getLeafletMap(concept, time, long, lat, "RODINIA2013")
      }
    }
    else { # No coastline reconstruction
      coords <- substr(coordContents, nchar("{\"type\":\"MultiPoint\",\"coordinates\":[[") + 1, nchar(coordContents) - 3)
      coords <- strsplit(coords, ",")[[1]]
      longitude <- as.numeric(coords[1])
      latitude <- as.numeric(coords[2])
      
      # Make "custom" icon with golden spike image
      gsspIcon <- makeIcon(
        iconUrl = "http://www2.cs.uidaho.edu/~max/image/gssp.png",
        iconWidth = 20, iconHeight = 32,
        iconAnchorX = 10, iconAnchorY = 32  
      )
      
      # Create popup text with HTML formatting
      popup <- paste("<b><i>", concept, "</b></i><br><b>Longitude: </b>", longitude, "?<br><b>Latitude: </b>", latitude, "?<br><b>Age: </b>", time, " Ma", sep = "")
      
      # Create a leaflet map with no JSON for reconstructed coastlines
      #   but with a marker and a popup window
      leaflet() %>%
        addMarkers(lng = long, lat = lat, icon = gsspIcon, popup = popup)
    }
    
  })
}