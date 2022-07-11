library(DeepTimeKB)

# Return a descriptive string from the result of gts.topo()
getTopo <- function(region1, region2, geoConcept1, geoConcept2, iscVersion1, iscVersion2) {
  
  # Get gts.topo() and gts.range() outputs depending on the input
  # Several input fields are optional
  if (region1 == "International" && region2 == "International") {
    topoResult <- gts.topo(geoConcept1 = geoConcept1,
                           geoConcept2 = geoConcept2,
                           region1 = region1,
                           region2 = region2,
                           iscVersion1 = iscVersion1,
                           iscVersion2 = iscVersion2)
    
    rangeResult1 <- gts.range(geoConcept = geoConcept1,
                              region = region1,
                              iscVersion = iscVersion1)
    
    rangeResult2 <- gts.range(geoConcept = geoConcept2,
                              region = region2,
                              iscVersion = iscVersion2)
    
  }
  else if (region1 != "International" && region2 == "International") {
    topoResult <- gts.topo(geoConcept1 = geoConcept1,
                           geoConcept2 = geoConcept2,
                           region1 = region1,
                           region2 = region2,
                           iscVersion2 = iscVersion2)
    
    rangeResult1 <- gts.range(geoConcept = geoConcept1,
                              region = region1)
    
    rangeResult2 <- gts.range(geoConcept = geoConcept2,
                              region = region2,
                              iscVersion = iscVersion2)
    
  }
  else if (region1 == "International" && region2 != "International") {
    topoResult <- gts.topo(geoConcept1 = geoConcept1,
                           geoConcept2 = geoConcept2,
                           region1 = region1,
                           region2 = region2,
                           iscVersion1 = iscVersion1)
    
    rangeResult1 <- gts.range(geoConcept = geoConcept1,
                              region = region1,
                              iscVersion = iscVersion1)
    
    rangeResult2 <- gts.range(geoConcept = geoConcept2,
                              region = region2)
    
  }
  else {
    topoResult <- gts.topo(geoConcept1 = geoConcept1,
                           geoConcept2 = geoConcept2,
                           region1 = region1,
                           region2 = region2)
    
    rangeResult1 <- gts.range(geoConcept = geoConcept1,
                              region = region1)
    
    rangeResult2 <- gts.range(geoConcept = geoConcept2,
                              region = region2)
    
  }
  
  # Set range1 to a string displaying the beginning and end time values
  range1 <- paste(geoConcept1, ": ", rangeResult1$begTimeValue[1], " - ", rangeResult1$endTimeValue[1], sep = "")
  # Set range2 to a string displaying the beginning and end time values
  range2 <- paste(geoConcept2, ": ", rangeResult2$begTimeValue[1], " - ", rangeResult2$endTimeValue[1], sep = "")  
  # Set topo to a string displaying two regions and their relationship
  topo <- paste(geoConcept1, topoResult, geoConcept2, sep = " ")
  
  return(paste(range1, range2, topo, sep = "\n"))
}