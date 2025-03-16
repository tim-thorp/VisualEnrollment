#' Run Shiny app
#' @param ... optional parameters for the shinyApp call
#' @export
visualenrollmentApp <- function(...){

  router <- make_router(
    route(
      "ca", 
      informaticaUI("informatica_ca", "ca"),
      function(input, output, session) {
        informaticaServer("informatica_ca", "ca")
      }
    ),
    route(
      "es",
      informaticaUI("informatica_es", "es"),
      function(input, output, session) {
        informaticaServer("informatica_es", "es")
      }
    ),
    route(
      "en", 
      informaticaUI("informatica_en", "en"),
      function(input, output, session) {
        informaticaServer("informatica_en", "en")
      }
    ),
    NULL
  )
  
  ui <- shinyUI(fluent_page(router))
  
  server <- shinyServer(
    function(input, output, session) {
      router$server(input, output, session)
    }
  )

  shinyApp(ui, server, ...)
}
