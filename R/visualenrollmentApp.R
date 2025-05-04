#' Run Shiny app
#' @param ... optional parameters for the shinyApp call
#' @export
visualenrollmentApp <- function(...){

  # Suppress deprecation warnings: make_router is deprecated,
  # but router_ui is not compatible with fluent_page.
  router <- suppressWarnings(make_router(
    route(
      "ca", 
      subjectEnrollmentUI("subject_enrollment_ca", "ca"),
      function(input, output, session) {
        subjectEnrollmentServer("subject_enrollment_ca", "ca")
      }
    ),
    route(
      "es",
      subjectEnrollmentUI("subject_enrollment_es", "es"),
      function(input, output, session) {
        subjectEnrollmentServer("subject_enrollment_es", "es")
      }
    ),
    route(
      "en", 
      subjectEnrollmentUI("subject_enrollment_en", "en"),
      function(input, output, session) {
        subjectEnrollmentServer("subject_enrollment_en", "en")
      }
    ),
    NULL
  ))
  
  ui <- shinyUI(fluent_page(router))
  
  server <- shinyServer(
    function(input, output, session) {
      router$server(input, output, session)
    }
  )

  shinyApp(ui, server, ...)
}
