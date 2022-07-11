library(readr)

# Function taking arguments for gts.list() and returning a string
# If the result of gts.list() is empty then the returned string is more descriptive
# Otherwise the string is just the output of gts.list()
gtsList <- function(region, level) {
  string <- paste(gts.list(region = region, level = level), collapse = "\n")
  if (string == "") string <- paste("No ", level, "(s) in ", region, " scheme.", sep = "")
  return(string)
}

# Function taking a list and returning a concatenated string of the list
# Optional argument separator is pasted in between each element
listToString <- function(l, separator = "\n") {
  string <- ""
  for (i in 1:nrow(l)) {
    if (i == 1) string <- paste(string, l[i], sep = "")
    else string <- paste(string, l[i], sep = separator)
  }
  return(string)
}

# Takes a list of strings and pads the shorter strings with whitespace to even the lengths
# This is for formatting purposes
strListToConstantLength <- function(strlist) {
  newList = vector()
  maxLen = 0
  
  # Find the length of the longest string
  for (str in strlist) {
    if (nchar(str) > maxLen) maxLen = nchar(str)
  }
  
  # Pad all strings under that length with whitespace
  for (str in strlist) {
    # Pad the string
    if (nchar(str) < maxLen) {
      newStr <- str
      for (i in 1:(maxLen - nchar(str))) {
        newStr <- paste(newStr, " ", sep = "")
      }
      newList <- append(newList, newStr)
    }
    # No padding necessary
    else {
      newList <- append(newList, str)
    }
  }
  
  return(newList)
}

# Function taking a dataframe and returning a string
# This string is formatted and includes the column names
dfToString <- function(df, colSeparator = "\t", rowSeparator = "\n") {
  # Empty dataframe, return "NA"
  if (ncol(df) == 0 || nrow(df) == 0) return("NA")
  
  # Create a new dataframe
  df2 <- data.frame(matrix(NA, ncol = 1, nrow = nrow(df) + 1))[-1]
  # Set the column names
  columnNames <- colnames(df)
  # Pad each column with whitespace for formatting purposes and add column names
  for (i in 1:ncol(df)) {
    df2[columnNames[i]] <- strListToConstantLength(c(columnNames[i], df[[i]]))
  }
  
  # Make a new string
  string <- ""
  # Loop through each element and concatenate it
  for (i in 1:nrow(df2)) {
    for (j in 1:ncol(df2)) {
      if (j == 1) string <- paste(string, df2[i, j], sep = "")
      else string <- paste(string, df2[i, j], sep = colSeparator)
    }
    string <- paste(string, rowSeparator, sep = "")
  }
  return(string)
}

# Function taking a function name and returning the HTML for the help page
helpPageTextHTML <- function(func) {
  temp <- Rd2HTML(Rd_fun(func), out = tempfile("docs"))
  content <- read_file(temp)
  file.remove(temp)
  return(content)
}

# Function that returns a list of all geographical time concepts
allGeoConcepts <- function() {
  regions <- c("International", "West Europe", "Tethyan", "N-E Siberia", "South China",
                "Russia Platform", "New Zealand", "North China", "North America", "Kazakhstan",
                "Japan", "Iberian-Morocco", "California", "Britain", "Boreal", "Baltoscania",
                "East Avalonian", "Australia")
  concepts = vector()
  # Loop through each region and add the concepts from that region
  for (region in regions) {
    for (concept in gts.list(region)) {
      if (!concept %in% concepts) concepts <- append(concepts, concept)
    }
  }
  # Sort the concepts alphabetically
  return(sort(concepts))
}

# Function that returns a list of all geographical time concepts without the level (period, epoch, etc.)
allGeoConceptsNoLevels <- function() {
  regions <- c("International", "West Europe", "Tethyan", "N-E Siberia", "South China",
               "Russia Platform", "New Zealand", "North China", "North America", "Kazakhstan",
               "Japan", "Iberian-Morocco", "California", "Britain", "Boreal", "Baltoscania",
               "East Avalonian", "Australia")
  concepts = vector()
  # Loop through each region and add the concepts from that region
  for (region in regions) {
    for (concept in gts.list(region)) {
      if (!concept %in% concepts) {
        # Split the string at each space
        conceptNoLevel <- strsplit(concept, " ")[[1]][1]
        # Add the first string from that split
        concepts <- append(concepts, conceptNoLevel)
      }
    }
  }
  return(sort(concepts))
}