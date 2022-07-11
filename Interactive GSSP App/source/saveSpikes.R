library(jsonlite)
library(rstudioapi)
library(rjson)

getDataPath <- function(){
  # return the path of data directory, would return the absolute path of "./data"
  curPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
  dataPath <- paste(curPath, .Platform$file.sep, "data", sep='')
  return(dataPath)
}

checkDataFile <- function(fileName){
  # check if the file exists in the path of "./data"
  dataPath <- getDataPath()
  filePath <- paste(dataPath, .Platform$file.sep, fileName, sep='')
  if(!file.exists((filePath))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

loadSpikesJson <- function(fileName){
  # load the data file in "./data"
  dataPath <- getDataPath()
  filePath <- paste(dataPath, .Platform$file.sep, fileName, sep='')
  result <- fromJSON(file = filePath)
  return(result)
}

saveSpikesJson <- function(rObject, filename){
  # save the data file in "./data"
  dataPath <- getDataPath()
  filePath <- paste(dataPath, .Platform$file.sep, filename, sep='')
  jsonFile <- toJSON(rObject)
  write(jsonFile, filePath)
}

# saveLoadJson <- function(rObject){
#   jsonFile <- toJSON(rObject)
#   
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#   write(jsonFile, "file.json")
#   getwd()
#   result <- fromJSON(file = "file.json")
#   return(result)
# }

testFilePath <- function(){
  # test function
  curPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
  print(curPath)
  filePath <- paste(curPath, .Platform$file.sep, "data", sep='')
  print(filePath)
  dir.exists(filePath)
}
