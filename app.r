
#setwd("/Users/tommysmale/classroom/capstone")

load_libraries <- function() {
  ## ----include=FALSE-----------------------------------------------------------------------
  local({
    r <- getOption("repos")
    r["CRAN"] <- "https://ftp.osuosl.org/pub/cran/"
    options(repos = r)
  })
  load_default_pkgs <- function() {
    default_packages <- c(
      "KernSmooth",
      "MASS",
      "Matrix",
      "base",
      "boot",
      "class",
      "cluster",
      "codetools",
      "compiler",
      "datasets",
      "foreign",
      "grDevices",
      "graphics",
      "grid",
      "lattice",
      "methods",
      "mgcv",
      "nlme",
      "nnet",
      "parallel",
      "rpart",
      "spatial",
      "splines",
      "stats",
      "stats4",
      "survival",
      "tools",
      "utils"
    )
    for(pkg in default_packages) {
      library(pkg, character.only = TRUE)
    }
  }
  library(tidyverse)
  library(leaflet)
  library(shiny)
  library(jsonlite)
  library(rnoaa)
  library(DT)
  library(shinycssloaders)
  library(rsconnect)
  library(reactable)
  library(httr)
  library(ggplot2)
  library(gridExtra)
  library(grDevices)
  library(grid)
}
load_libraries()
#Check for "Tcl/tk" dependencies
check_dependencies <- function() {
  pack <- available.packages()
  dep <- tools::package_dependencies(pack[c("tidyverse", "leaflet", "shiny", "jsonlite", "rnoaa", "DT", "shinycssloaders", "rsconnect"), "Package"])
  for(name in names(dep)) { 
    dependencies <- dep[name]
    if(length(grep("Tcl", dependencies)) != 0 || 
       length(grep("Tk", dependencies)) != 0) { 
      error <- paste(name, "has a dependency shiny will not like")
      print(error)
    }
  }
}
run <- function() {
  rm(list=ls())
  source("weather.R")
  source("ui.R")
  source("server.R")
  runApp(shinyApp(ui, server), launch.browser=TRUE)
}


## ----publish-----------------------------------------------------------------------------
publish <- function() {
  files <-
    list.files(
      getwd(),
      include.dirs = TRUE,
      recursive = TRUE,
      all.files = TRUE
    )
  git_files <- grep("git", files)
  files <- files[-git_files]
  files <-
    files[!files %in% c(
      "previous.rmd",
      "shiny_examples.Rmd",
      "shiny_test.r",
      "test.R",
      "www",
      "www/tommy.jpg",
      ".shiny_token",
      ".shiny_secret", 
      "weather.rmd"
    )]
  setup_shinyappio()
  rsconnect::deployApp(getwd(), appFiles = files, account='weathercollector')
}


## ----debug_shinyio-----------------------------------------------------------------------
debug_shinyappio <- function() {
  rsconnect::showLogs()
}

## ----setup_shinyio-----------------------------------------------------------------------
setup_shinyappio <- function() {
  shiny_token <-
    read.delim(".shiny_token", header = FALSE, col.names = "token")$token
  shiny_secret <-
    read.delim(".shiny_secret", header = FALSE, col.names = "token")$token
  rsconnect::setAccountInfo(name = 'weathercollector',
                            token = shiny_token,
                            secret = shiny_secret)
}

