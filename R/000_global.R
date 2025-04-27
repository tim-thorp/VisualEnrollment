globalVariables(c("subject_distance_matrix", "subject_positions"))

# Make inst/www directory available as www
shiny::addResourcePath(
  prefix = "www",
  directoryPath = system.file("www", package = "visualenrollment")
)

