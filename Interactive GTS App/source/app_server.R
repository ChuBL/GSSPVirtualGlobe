library(DeepTimeKB)
library(gbRd)
library(igraph)
library(shiny)
library(shinythemes)
library(leaflet)
source("topology.R", local = TRUE)
source("gts_knowledge.R", local = TRUE)

app_server <- function(input, output, session){
  
  # ===================================
  # GTS Knowledge
  
  # -----------------------------------
  # Help Pages
  # Uses helpPageTextHTML() function in gts_knowledge.R to render a help page as HTML text
  
  # gts.listRegion()
  output$listRegionHelp <- renderText({
    helpPageTextHTML("gts.listRegion")
  })
  
  # gts.list()
  output$listHelp <- renderText({
    helpPageTextHTML("gts.list")
  })
  
  # gts()
  output$gtsHelp <- renderText({
    helpPageTextHTML("gts")
  })
  
  # gts.level()
  output$gtsLevelHelp <- renderText({
    helpPageTextHTML("gts.level")
  })
  
  # gts.point()
  output$gtsPointHelp <- renderText({
    helpPageTextHTML("gts.point")
  })
  
  # gts.range()
  output$gtsRangeHelp <- renderText({
    helpPageTextHTML("gts.range")
  })
  
  # gts.within()
  output$gtsWithinHelp <- renderText({
    helpPageTextHTML("gts.within")
  })
  
  # gts.hierarchy()
  output$gtsHierarchyHelp <- renderText({
    helpPageTextHTML("gts.hierarchy")
  })
  
  # -----------------------------------
  # Demo Pages
  # Many use dfToString(), a function defined in gts_knowledge.R
  
  # gts.listRegion() demo
  # Runs listRegionText() function
  # listRegionText() is a function defined below
  # It only updates when input$runListRegion button is pressed
  output$listRegionOutput <- renderText({
    listRegionText()
  })
  
  # gts.listRegion() demo
  # Only runs when input$runListRegion button is pressed
  listRegionText <- eventReactive(input$runListRegion, {
    # How gts.listRegion() is called depends on the arguments
    #   provided by the user
    
    # Optional arguments are left empty by user
    if (input$listRegionPrefix == "" && input$listRegionGraph == ""){
      dfToString(gts.listRegion())
    }
    # User only specifies graph argument
    else if (input$listRegionPrefix == "" && input$listRegionGraph != ""){
      dfToString(gts.listRegion(graph = input$listRegionGraph))
    }
    # User only specifies prefix argument
    else if (input$listRegionPrefix != "" && input$listRegionGraph == ""){
      dfToString(gts.listRegion(graph = input$listRegionGraph))
    }
    # User specifies both optional arguments
    else {
      dfToString(gts.listRegion(prefix = input$listRegionPrefix, graph = input$listRegionGraph))
    }
  })
  
  # gts.list() demo
  # Calls gtsList() function in gts_knowledge.R
  output$listOutput <- renderText({
    gtsList(input$listRegion, input$listLevel)
  })
  
  # gts() demo
  # Runs gtsText() function
  # gtsText() is a function defined below
  # It only updates when input$runGts button is pressed
  output$gtsOutput <- renderText({
    gtsText()
  })
  
  # gts() demo
  # Only runs when input$runGts button is pressed
  gtsText <- eventReactive(input$runGts, {
    # How gts.listRegion() is called depends on the arguments
    #   provided by the user
    
    # Optional arguments are left empty by user
    if (input$gtsPrefix == "" && input$gtsGraph == "") {
      output <- gts(geoConcept = input$gtsConcept)
    }
    # User only specifies graph argument
    else if (input$gtsPrefix == "" && input$gtsGraph != "") {
      output <- gts(geoConcept = input$gtsConcept, graph = input$gtsGraph)
    }
    # User only specifies prefix argument
    else if (input$gtsPrefix != "" && input$gtsGraph == "") {
      output <- gts(geoConcept = input$gtsConcept, prefix = input$gtsPrefix)
    }
    # User specifies both optional arguments
    else {
      output <- gts(geoConcept = input$gtsConcept, prefix = input$gtsPrefix, graph = input$gtsGraph)
    }
    dfToString(output)
  })
  
  # gts.level() demo
  # Calls gts.level() with user input
  output$gtsLevelOutput <- renderText({
    dfToString(gts.level(input$gtsLevelConcept))
  })
  
  # gts.point() demo
  # Runs gts.point() with user input
  output$gtsPointOutput <- renderText({
    dfToString(gts.point(input$gtsPointYears))
  })
  
  # gts.range() demo
  # Runs gts.range() with user input (some optional)
  output$gtsRangeOutput <- renderText({
    # The arguments used in gts.range() depend on the user input
    
    # User can specify a region
    if (input$gtsRangeShowRegion) {
      # If the region is "International" one of the arguments is the ISC version
      if (input$gtsRangeRegion == "International") {
        df <- gts.range(geoConcept = input$gtsRangeGeoConcept, region = input$gtsRangeRegion, iscVersion = input$gtsRangeIscVersion)
      }
      # Otherwise, the ISC version is not needed
      else {
        df <- gts.range(geoConcept = input$gtsRangeGeoConcept, region = input$gtsRangeRegion)
      }
    }
    # The user can opt to not specify a region
    else {
      df <- gts.range(geoConcept = input$gtsRangeGeoConcept)
    }
    dfToString(df)
  })
  
  # gts.within() demo
  # Runs gts.within() with user input
  output$gtsWithinOutput <- renderText({
    dfToString(gts.within(geotime1 = input$gtsWithinTime1, geotime2 = input$gtsWithinTime2))
  })
  
  # gts.hierarchy() demo
  # Runs gts.hierarchy with user input
  output$gtsHierarchyOutput <- renderText({
    dfToString(gts.hierarchy(input$gtsHierarchyConcept))
  })
  
  # ===================================
  # GSSP
  
  # Render the output of gssp.map()
  output$gsspPlot <- renderPlot({
    gssp.map(input$isc)
  })
  
  # ===================================
  # Topology
  
  # Render the output of topoText() to the text box
  output$topology <- renderText({
    topoText()
  })
  
  # Set text when the button is pressed
  topoText <- eventReactive(input$runTopo, {
    getTopo(input$region1, input$region2,
            input$geoConcept1, input$geoConcept2,
            input$iscVersion1, input$iscVersion2)
  })
  
  # Change the dropdown menus for geological concepts based on the chosen regions
  observe({
    updateSelectInput(session, "geoConcept1", choices = sort(gts.list(input$region1)))
    updateSelectInput(session, "geoConcept2", choices = sort(gts.list(input$region2)))
  })
}