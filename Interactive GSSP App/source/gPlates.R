library(leaflet)
library(SPARQL)
library(readr)
library(geojsonio)
library(stringr)
source("stringFunctions.R", local = TRUE)

# Returns a dataframe of GSSP concepts, coordinates, and age
conceptsWithCoordinates <- function() {
  
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
  # This query returns each spike's label, coordinates, and age
  # Some fields are cast as strings in order to take care of some URI formatting
  query <- paste0(prefixes, '
    SELECT DISTINCT (str(?spLabel) AS ?label) (str(?spCoordinates) AS ?coordinates) ?timeValue
    WHERE {
      GRAPH <http://deeptimekb.org/iscallnew> {   
        ?bdry  a gts:GeochronologicBoundary ;
          dc:description [
            gts:stratotype ?baseSp ;
            skos:inScheme ', scheme, '
          ] ; 
          time:inTemporalPosition ?tempPosition .
          
        ?tempPosition 
          dc:description [
            time:numericPosition ?timeValue ;
            skos:inScheme ', scheme, '
          ] .
          
          ?baseSp samfl:shape ?spLocation ;  
            rdfs:label ?spLabel ;
            gts:ratifiedGSSP ?tf .
          FILTER(regex(str(?tf), "true", "i"))
        ?spLocation geo:asWKT ?spCoordinates .
      }
    }'
  )
  
  # Run the query and process it into a dataframe
  result <- SPARQL(endpoint, query)$results
  return(getConceptsAndCoordinates(result))
}

# Process the result from the SPARQL query in conceptsWithCoordinates()
getConceptsAndCoordinates <- function(result) {
  
  # Create new vectors
  longitude <- vector()
  latitude <- vector()
  time <- vector()
  # Substring the longitude/latitude and concept strings
  longlat <- substr(result$coordinates, 7, nchar(result$coordinates) - 1)
  concepts <- str_trim(substr(result$label, 25, nchar(result$label)))
  
  for (i in 1:length(longlat)) { # Loop through each element
    # Split the longitude/latidude string into a list of strings
    split <- str_split(longlat[i], " ")[[1]]
    # Cast the longitude and latitude strings as numbers
    longitude <- append(longitude, as.numeric(split[1]))
    latitude <- append(latitude, as.numeric(split[2]))
    # Cast the time string as a number
    time <- append(time, as.numeric(result$time[i]))
  }
  
  # Create a dataframe and set the column names
  df <- data.frame(concepts, longitude, latitude, time)
  colnames(df) <- c("concept", "longitude", "latitude", "time")
  print(df)
  return(df[order(df$time),])
  
}

# Returns a leaflet map with reconstructed coastlines and a marker
getLeafletMap <- function(concept, time, longitude, latitude, reconstructionModel) {
  
  # Create URLs and temperary filename
  filename <- "temp.json"
  coordUrl <- paste("https://gws.gplates.org/reconstruct/reconstruct_points/?points=", longitude, ",", latitude, "&time=", time, "&model=", reconstructionModel, sep = "")
  coastlineUrl <- paste("https://gws.gplates.org/reconstruct/coastlines/?time=", time, "&model=", reconstructionModel, sep = "")
  
  # Open the temporary file
  fileConn <- file(filename)
  # Read contents from the URLs
  coordContents <- read_file(coordUrl)
  coastlineContents <- read_file(coastlineUrl)
  # Write the coastline JSON to a file and reread it as a spatial object, then delete the temporary file
  writeLines(coastlineContents, fileConn)
  close(fileConn)
  json <- geojson_read(filename, what = "sp")
  file.remove(filename)
  
  # Parse the coordinates from the multipoint JSON
  coords <- substr(coordContents, nchar("{\"type\":\"MultiPoint\",\"coordinates\":[[") + 1, nchar(coordContents) - 3)
  coords <- strsplit(coords, ",")[[1]]
  longitude <- as.numeric(coords[1])
  latitude <- as.numeric(coords[2])
  
  # Create a GSSP icon
  gsspIcon <- makeIcon(
    iconUrl = "http://www2.cs.uidaho.edu/~max/image/gssp.png",
    iconWidth = 20, iconHeight = 32,
    iconAnchorX = 10, iconAnchorY = 32  
  )
  
  # Construct a popup with basic information (name, longitude/latitude, age)
  popup <- paste("<b><i>", concept, "</b></i><br><b>Longitude: </b>", longitude, "<br><b>Latitude: </b>", latitude, "<br><b>Age: </b>", time, " Ma", sep = "")
  
  # Construct a leaflet map with markers and popups and return it
  l <- leaflet(json) %>%
    addPolygons(stroke = TRUE, weight = .5, opacity = 1, fillOpacity = 1, fillColor = "#f2efe9", color = "#bfaaba") %>%
    addMarkers(lng = longitude, lat = latitude, icon = gsspIcon, popup = popup)
  return(l)
  
}