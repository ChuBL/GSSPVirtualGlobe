# Trims URI from camel case into multiple words
# Ex: trimURI("http://www.w3.org/2004/02/skos/core#broaderTransitive") -> "Broader Transitive"
# Ex: trimURI("notauri") -> "notauri"
trimURI <- function(uri) {
  if (is.url(uri)) { # if the string is a URI, process it
    
    # split the string at each occurrence of '#' and store the last of the splits in y
    x <- strsplit(uri, '#') 
    y <- x[[length(x)]][length(x[[length(x)]])]
    
    # split the string at each occurrence of '/' and store the last of the splits in y
    x <- strsplit(y, '/')
    y <- x[[length(x)]][length(x[[length(x)]])]
    
    # split y from camel case and return it
    return(camelSplit(y))
    
  }
  
  return(uri) # if not a URI, no processing is required
}

# Split a string that is in camel case
# Ex: camelSplit("camelCase") -> "Camel Case"
camelSplit <- function(string) {
  # Create a new character vector
  newString <- vector()
  # Split the string into an array of characters
  split <- strsplit(string, "")[[1]]
  # Capitalize the first character and store it in the new string
  newString <- paste(newString, toupper(split[1]), sep = "")

  for (i in 2:length(split)) { # Loop through the remaining characters in the supplied string
    if (is.upper(split[i]) && is.lower(split[i - 1])){ # If the current character is capital and the previous is lowercase...
      # Append a space and the current character to the new string
      newString <- paste(newString, " ", split[i], sep = "")
    }
    else { # Otherwise...
      # Just append the current character to the new string
      newString <- paste(newString, split[i], sep = "")
    }
  }
  return(newString)
}

# Checks whether or not a string is a URL
# Ex: is.url("www.deeptimkb.org") -> TRUE
# Ex: is.url("notaurl") -> FALSE
is.url <- function(x) {
  # Return whether or not the string contains "www.", "http:", or "https:" using a regular expression
  return(grepl("www.|http:|https:", x))
}

# Check whether a supplied string is alphabetical or not
# Ex: is.alpha("alphabetical") -> TRUE
# Ex: is.alpha("not!") -> FALSE
# Ex: is.alpha("7") -> FALSE
is.alpha <- function(x) {
  # Return whether or not the string consists of only alphabetical characters using a regular expression
  return(grepl("^[A-Za-z]*$", x))
}

# Check whether a supplied string is alphabetical and uppercase or not
# Ex: is.upper("UPPER") -> TRUE
# Ex: is.upper("lower") -> FALSE
# Ex: is.upper("7") -> FALSE
is.upper <- function(char) {
  # Return whether or not the string consists of only uppercase alphabetical characters using a regular expression
  return(grepl("^[A-Z]*$", char))
}

# Check whether a supplied string is alphabetical and lowercase or not
# Ex: is.upper("lower") -> TRUE
# Ex: is.upper("UPPER") -> FALSE
# Ex: is.upper("7") -> FALSE
is.lower <- function(char) {
  # Return whether or not the string consists of only lowercase alphabetical characters using a regular expression
  return(grepl("^[a-z]*$", char))
}

# Creates an HTML link with a supplied link
# Optional argument charLimit denotes how long a link has to be before shortening how it is. Defaults to 43, useful for longer links
# Ex: HTMLlink("www.deeptimekb.org") -> "<a href ="www.deeptimekb.org"> deeptimekb.org </a>"
# Ex: HTMLlink("www.deeptimekb.org", charLimit = 10) -> "<a href ="www.deeptimekb.org"> deeptim... </a>"
HTMLlink <- function(link, charLimit = 43) {
  # If the length of the link is shorter than the character limit than it does not need to be shortened
  if (nchar(link) < charLimit) {
    htmllink <- paste("<a href =\"", link, "\">", link, "</a>", sep = "")
  }
  else { # Otherwise, it must be shortened
    # Grab a substring of the string 3 characters shorter than the limit
    shortlink <- substr(link, 0, charLimit - 3)
    # Append an ellipses
    shortlink <- paste(shortlink, "...", sep = "")
    # "Linkify" it
    htmllink <- paste("<a href =\"", link, "\">", shortlink, "</a>", sep = "")
  }
  return(htmllink)
}