library(shiny)
library(shinythemes)
library(DeepTimeKB)
library(leaflet)
source("gts_knowledge.R", local = TRUE)
source("topology.R", local = TRUE)

# Create the UI for the app
app_ui <- shinyUI({
  # All regions/vocabulary schemes
  regions <- sort(c("International", "West Europe", "Tethyan", "N-E Siberia", "South China",
              "Russia Platform", "New Zealand", "North China", "North America", "Kazakhstan",
              "Japan", "Iberian-Morocco", "California", "Britain", "Boreal", "Baltoscania",
              "East Avalonian", "Australia"))
  # Calls allGeoConcepts() from gts_knowledge.R
  conceptsWithLevels <- allGeoConcepts()
  # Calls allGeoConceptsNoLevels() from gts_knowledge.R
  concepts <- allGeoConceptsNoLevels()
  # All ISC Versions
  iscVersions <- substr(gts.iscSchemes(), 55, 61)
  # All levels
  levels <- c("Eon", "Era", "Period", "Epoch", "Age")
  # Set UI theme to a dark theme
  fluidPage(theme = shinytheme("cyborg"),
    # Navigation Bar with multiple pages
    navbarPage(
      "Interactive GTS App",
      
      # =================================================================
      # Panel 1: GTS Knowledge ==========================================
      # Demos and documentation for functions in the DeepTimeKB package
      
      tabPanel("GTS Knowledge",
        navbarPage(
          title = "",
          
          # gts.listRegion() function page
          tabPanel("gts.listRegion()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.listRegion()
              tabPanel("Demo",
                textInput(inputId = "listRegionPrefix",
                  label = "Prefix (optional)"        
                ),
                textInput(inputId = "listRegionGraph",
                  label = "Graph (optional)"          
                ),
                actionButton(
                  inputId = "runListRegion",
                  label = "Run"
                ),
                # Output for gts.listRegion()
                verbatimTextOutput(outputId = "listRegionOutput"),
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "listRegionHelp")    
              )
            )
          ),
          
          # gts.list() function page
          tabPanel("gts.list()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.list()
              tabPanel("Demo",
                selectInput(
                  inputId = "listRegion",
                  label = "Region/Vocabulary Scheme",
                  choices = regions
                ),
                selectInput(
                  inputId = "listLevel",
                  label = "Concept Level",
                  choices = levels
                ),
                # Output for gts.list()
                verbatimTextOutput(outputId = "listOutput"),                         
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "listHelp")
              )
            )
          ),
          
          # gts() function page
          tabPanel("gts()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts() 
              tabPanel("Demo",
                selectInput(
                  inputId = "gtsConcept",
                  label = "Geographical Concept",
                  choices = conceptsWithLevels
                ),
                textInput(inputId = "gtsPrefix",
                          label = "Prefix (optional)"        
                ),
                textInput(inputId = "gtsGraph",
                          label = "Graph (optional)"          
                ),
                actionButton(
                  inputId = "runGts",
                  label = "Run"
                ),
                # Output for gts()
                verbatimTextOutput(outputId = "gtsOutput")
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "gtsHelp")         
              )
            )        
          ),
          
          # gts.level() function page
          tabPanel("gts.level()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.level()
              tabPanel("Demo",
                selectInput(
                  inputId = "gtsLevelConcept",
                  label = "Geographical Concept",
                  choices = concepts
                ),         
                # Output for gts.level()
                verbatimTextOutput(outputId = "gtsLevelOutput")
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "gtsLevelHelp")         
              )
            )
          ),
          
          # gts.point() function page
          tabPanel("gts.point()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.point()
              tabPanel("Demo",
                numericInput(
                  inputId = "gtsPointYears",
                  label = "Geological time in million years BP",
                  min = 0,
                  step = 1,
                  value = 200
                ),
                # Output for gts.point()
                verbatimTextOutput(outputId = "gtsPointOutput")
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "gtsPointHelp")         
              )
            )
          ),
          
          # gts.range() function page
          tabPanel("gts.range()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.range()
              tabPanel("Demo",
                selectInput(
                  inputId = "gtsRangeGeoConcept",
                  label = "Geoligical Concept",
                  choices = conceptsWithLevels
                ),
                checkboxInput(inputId = "gtsRangeShowRegion", label = "Select region"),
                # Conditional panel only shows up when the user opts to specify a region
                conditionalPanel(
                  condition = "input.gtsRangeShowRegion",
                  selectInput(
                    inputId = "gtsRangeRegion",
                    label = "Region",
                    choices = regions
                  ),
                ),
                # Conditional panel only shows when the user selects the "International" region
                # User must specify an ISC version
                conditionalPanel(
                  condition = "input.gtsRangeRegion == \"International\" && input.gtsRangeShowRegion",
                  selectInput(
                    inputId = "gtsRangeIscVersion",
                    label = "ISC Version",
                    choices = iscVersions
                  )
                ),
                # Output for gts.range()
                verbatimTextOutput(outputId = "gtsRangeOutput")
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "gtsRangeHelp")         
              )
            )
          ),
          
          # gts.within() function page
          tabPanel("gts.within()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.within()
              tabPanel("Demo",
                numericInput(
                  inputId = "gtsWithinTime1",
                  label = "Geological time in million years BP",
                  min = 0,
                  step = 1,
                  value = 200
                ),
                numericInput(
                  inputId = "gtsWithinTime2",
                  label = "Geological time in million years BP",
                  min = 0,
                  step = 1,
                  value = 0
                ),
                # Output for gts.within()
                verbatimTextOutput(outputId = "gtsWithinOutput")
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "gtsWithinHelp")         
              )
            )
          ),
          
          # gts.hierarchy() function page
          tabPanel("gts.hierarchy()",
            navbarPage(title = "",
              # Demo part
              # User can change parameters used to call gts.hierarchy()
              tabPanel("Demo",
                selectInput(
                  label = "Geoligical time concept",
                  inputId = "gtsHierarchyConcept",
                  choices = conceptsWithLevels
                ),
                # Output for gts.hierarchy()
                verbatimTextOutput(outputId = "gtsHierarchyOutput")
              ),
              # Documentation part
              # Just for showing R help page
              tabPanel("Documentation",
                htmlOutput(outputId = "gtsHierarchyHelp")         
              )
            )
          )
          
        )
      ),
      
      
      # =================================================================
      # Panel 2: GSSP Plot ==============================================
      # Function demo for gssp.map()
      
      tabPanel("GSSP Map",
        # User provides ISC version
        sidebarPanel(
          selectInput(inputId = "isc",
          label = "ISC Version:",
          choices = iscVersions),
        ),
        mainPanel(
          h1("GSSP Map"),
          h4("Plot the locations of all Global Boundary Stratotype Section and Points (GSSPs) on the world map according to a specific ISC scheme."),
          h6("Main functions used: gts.iscSchemes(), gssp.map()"),
        ),
        # Output plot for gssp.map()
        plotOutput(outputId = "gsspPlot")
      ),
      
      
      # =================================================================
      # Panel 3: GTS Topology ===========================================
      # User selects parameters to run gts.topo() with
      tabPanel("Topology", 
        sidebarPanel(
          # Select first region
          selectInput(
            inputId = "region1",
            label = "Region",
            choices = regions,
            selected = "International"
          ),
          # Select first region's ISC version if the region is "International"
          conditionalPanel(
            condition = "input.region1 == \"International\"",
            selectInput(
              inputId = "iscVersion1",
              label = "ISC Version",
              choices = iscVersions
            ),
          ),
          # Select first region's geological concept
          selectInput(
            inputId = "geoConcept1",
            label = "Geological time concept 1",
            choices = sort(gts.list("International"))
          ),
        ),
        sidebarPanel(
          # Select second region
          selectInput(
            inputId = "region2",
            label = "Region",
            choices = regions,
            selected = "International"
          ),
          # Select second region's ISC version if the region is "International"
          conditionalPanel(
            condition = "input.region2 == \"International\"",
            selectInput(
              inputId = "iscVersion2",
              label = "ISC Version",
              choices = iscVersions
            ),
          ),
          # Select second region's geological concept
          selectInput(
            inputId = "geoConcept2",
            label = "Geological time concept 2",
            choices = sort(gts.list("international"))
          ),
        ),
        mainPanel(
          # Run button
          actionButton(
            inputId = "runTopo",
            label = "Run"
          ),
          # Titles
          h1("GTS Topology"),
          h4("Get the topology of two geological time concepts."),
          # Text output
          verbatimTextOutput(outputId = "topology")
        )
      ),
      
      # =================================================================
      # Panel 4: Settings ===============================================
      tabPanel("Settings",
        # Change the theme of the UI  
        shinythemes::themeSelector()    
      ),
      
      # =================================================================
      # Panel 5: Source Code ============================================
      tabPanel("Source Code",
        h5("The source code and a document outlining the UI and server of this app are available at the link below."),
        tags$a(href="https://github.com/lotkey/DTKB-Apps", "https://github.com/lotkey/DTKB-Apps")           
      )
    )
  )
})