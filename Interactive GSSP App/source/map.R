#library(leaflet)
library(SPARQL)
library(threejs)
library(stringr)
source("saveSpikes.R", local = TRUE)
source("stringFunctions.R", local = TRUE)



# Return a map with markers at each golden spike and popup information
map <- function() {
  # Set the default name of the json file for store spikes data
  spikesJsonDataFileName = "spikesData.json"
  
  # Check the existance of the spikes data json file
  if(checkDataFile(spikesJsonDataFileName)){
    # if exists, load directly
    queryResultDF <- loadSpikesJson(spikesJsonDataFileName)
  }else{
    # if not, process the query and save a local json copy of the processed query result for loading next time
    result <- queryGSSPResult()
    processedResult <- processResult(result)
    saveSpikesJson(processedResult, spikesJsonDataFileName)
    queryResultDF <- processedResult
  }
  
  # Process the results and store the result in a dataframe
  # queryResultDF <- processResult(result)
  
  # Isolate the longitude and latitude into a dataframe
  longlatdata <- str_split_fixed(queryResultDF$longlat, " ", 2)
  longlatdf <- data.frame(longlatdata)
  
  
  # # Make gssp icon for the leaflet map
  # gsspIcon <- makeIcon(
  #   iconUrl = "http://www2.cs.uidaho.edu/~max/image/gssp.png",
  #   iconWidth = 20, iconHeight = 32,
    
  #   # Use the bottom center point of the icon for the anchor  
  #   iconAnchorX = 10, iconAnchorY = 32  
  # )
  
  # editor: Jiyin
  (earth <- system.file("images/world.jpg",  package="threejs")) # Get the image of globe
  
  m <- globejs(img=earth, lat=longlatdf[,2], long=longlatdf[,1], # arcs=test_df,
               #arcsHeight=0.3, arcsLwd=2, arcsColor="#ffff00", arcsOpacity=0.15,
               atmosphere=TRUE, height=800, width = 800)
  return(m)
}

queryGSSPResult <- function(){
  # Endpoint to send SPARQL queries to
  endpoint <- "http://virtuoso.nkn.uidaho.edu:8890/sparql/"
  
  # List of SPARQL prefixes for the queries
  prefixes <- "
      prefix tssc: <http://deeptimekb.org/tssc#> 
      prefix tsnc: <http://deeptimekb.org/tsnc#> 
      prefix tswe: <http://deeptimekb.org/tswe#> 
      prefix tsbr: <http://deeptimekb.org/tsbr#>
      prefix tsba: <http://deeptimekb.org/tsba#> 
      prefix tsjp: <http://deeptimekb.org/tsjp#> 
      prefix tsau: <http://deeptimekb.org/tsau#>                   
      prefix tsnc: <http://deeptimekb.org/tsnc#> 
      prefix dc: <http://purl.org/dc/elements/1.1/> 
      prefix dcterms: <http://purl.org/dc/terms/> 
      prefix foaf: <http://xmlns.com/foaf/0.1/> 
      prefix geo: <http://www.opengis.net/ont/geosparql#> 
      prefix gts: <http://resource.geosciml.org/ontology/timescale/gts#> 
      prefix isc: <http://resource.geosciml.org/classifier/ics/ischart/> 
      prefix owl: <http://www.w3.org/2002/07/owl#> 
      prefix rank: <http://resource.geosciml.org/ontology/timescale/rank/> 
      prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
      prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> 
      prefix samfl: <http://def.seegrid.csiro.au/ontology/om/sam-lite#> 
      prefix sf: <http://www.opengis.net/ont/sf#> 
      prefix skos: <http://www.w3.org/2004/02/skos/core#> 
      prefix sosa: <http://www.w3.org/ns/sosa/> 
      prefix thors: <http://resource.geosciml.org/ontology/timescale/thors#> 
      prefix time: <http://www.w3.org/2006/time#> 
      prefix ts: <http://resource.geosciml.org/vocabulary/timescale/> 
      prefix vann: <http://purl.org/vocab/vann/> 
      prefix void: <http://rdfs.org/ns/void#> 
      prefix xkos: <http://rdf-vocabulary.ddialliance.org/xkos#> 
      prefix xsd: <http://www.w3.org/2001/XMLSchema#>
    "
  
  # Query that will return only the most recent international scheme
  schemes <- paste0(
    prefixes, '
      SELECT DISTINCT ?schemeURI ?label
      WHERE {
        GRAPH <http://deeptimekb.org/iscallnew> {
          ?schemeURI a skos:ConceptScheme ;
            rdfs:label ?label .
          FILTER regex(?label, "International", "i")
        }      
      }
      ORDER BY DESC (?schemeURI)
      LIMIT 1
    '
  )
  
  # Run the query and store the scheme
  scheme <- SPARQL(endpoint, schemes)$results$schemeURI[1]
  
  # Query to get all golden spikes in the most recent international scheme
  # This query returns each spike's label, coordinates, and every other predicate/property pair
  # Each field is cast as a string in order to take care of some URI formatting
  query <- paste0(prefixes, '
    SELECT DISTINCT (str(?spLabel) AS ?label) (str(?spCoordinates) AS ?coordinates) (str(?predicate) AS ?pred) (str(?property) AS ?prop)
    WHERE {
      GRAPH <http://deeptimekb.org/iscallnew> {   
        ?bdry  a gts:GeochronologicBoundary ;
          dc:description [
            gts:stratotype ?baseSp ;
            skos:inScheme ', scheme, '
          ] .
          
          ?baseSp samfl:shape ?spLocation ;  
            rdfs:label ?spLabel ;
            gts:ratifiedGSSP ?tf ; 
            ?predicate ?property .
          FILTER(regex(str(?tf), "true", "i"))
        ?spLocation geo:asWKT ?spCoordinates .
      }
    }'
  )
  
  # Run the query and store the results
  result <- SPARQL(endpoint, query)$results
  
  return(result)
}

# Process the dataframe returned from the SPARQL query in the map() function
processResult <- function(result) {
  
  # Trim all concepts and longitude/latitude pairs
  concept <- str_trim(substr(result$label, 25, nchar(result$label)))
  longlat <- substr(result$coordinates, 7, nchar(result$coordinates) - 1)
  # Create a dataframe from the concepts and longitude/latitude pairs and change the column names
  df <- unique(data.frame(concept, longlat))
  colnames(df) <- c("concept", "longlat")
  # Create a new vector of strings for the popup text with one string for each concept
  info <- paste("<b><i>", df$concept, "</i></b>", sep = "")
  lastConcept <- 1
  
  for (i in 1:nrow(result)) { # Loop through each row of the dataframe
    
    # Gather the concept and predicate from the row 
    newConcept <- str_trim(substr(result$label[i], 25, nchar(result$label[i])))
    index <- match(newConcept, df$concept)
    predicate <- trimURI(result$pred[i])
    
    if (predicate == "See Also") { # If the predicate is "See Also" then the property is a link
      # Store an HTML link version of the property
      property <- result$prop[i] 
      property <- HTMLlink(property)
    }
    else { # Otherwise...
      # Process the property as if it's a URI
      property <- trimURI(result$prop[i])
    }
    
    # Append the information to it's corresponding concept's row
    info[index] <- paste(info[index], "<br><b>", predicate, "</b>: <i>", property, "</i>", sep = "")
    
  }
  
  # Add this popup text to the dataframe in a new column
  df['text'] <- info
  # saveSpikesJson(df, "spikesData.json")
  # df2 <- loadSpikesJson("spikesData.json")
  return(df)
  
}

# setwd("/Users/blc/rspace/DTKB-AppsTest/Interactive GSSP App/source")
print(getwd())
# map()
