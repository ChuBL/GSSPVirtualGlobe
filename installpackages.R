# packages = c("igraph", "gbRd", "later", "httpuv", "shiny", "shinythemes", "shinydashboard", "leaflet", "readr", "SPARQL", "geojsonio", "stringr", "threejs")
packages = c("igraph", "gbRd", "later", "httpuv", "shiny", "shinythemes", "shinydashboard", "leaflet", "readr", "geojsonio", "stringr", "threejs")
install.packages(packages)

# install the sparql required packages.
packages = c("XML", "RCurl", "rstudioapi", "rjson")
install.packages(packages)
# library(this.path)
# # Get the directory from the path of the current file.
# cur_dir2 = dirname(this.path())
# print(cur_dir2)
# getwd()

setwd("/Users/blc/rspace/DTKB-AppsTest/Interactive GSSP App/source")
install.packages("SPARQL_1.16.tar.gz", repos = NULL, type="source")

install.packages("devtools", repos = "https://cloud.r-project.org")
devtools::install_github("https://github.com/xgmachina/DeeptTimeKB_RLib", upgrade = FALSE, force = TRUE)
