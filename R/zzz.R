.onLoad = function(libname, pkgname) {
  
  # Load libraries
  library(tidyverse)
  library(shiny)
  library(shinyjs)
  library(ggforce)
  library(shiny.react)
  library(shiny.fluent)
  library(shiny.router)
  library(ggrepel)
  # JULIA 07/01/2023 probar diferentes algoritmos de scaling
  library(MASS)
  #library(smacof)
  library(patchwork)
  library(deldir)
  
  # Make inst/www directory on the server as www
  shiny::addResourcePath(
    prefix = "www", # It be used to reference your directory
    directoryPath = system.file("www", package = "visualenrollment") # without inst
  )
}
