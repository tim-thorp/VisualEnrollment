fluent_page <- function(router) {
  fluidPage(
    tags$title("Visual Enrollment"),
    tags$head(
      tags$link(href = "www/css/styles.css", rel = "stylesheet", type = "text/css"),
      includeScript(system.file("www/js/scripts.js", package = "visualenrollment"))
    ),
    htmltools::htmlDependency(
      "office-ui-fabric-core",
      "11.0.0",
      list(href="https://static2.sharepointonline.com/files/fabric/office-ui-fabric-core/11.0.0/css/"),
      stylesheet = "fabric.min.css"
    ),
    shiny::tags$body(
      class = "ms-Fabric",
      dir = "ltr",
      router$ui
    )
  )
}

fluent_grid <- function(...) {
  div(
    class = "ms-Grid",
    dir = "ltr",
    ...
  )
}
fluent_grid_item <- function(..., class = "ms-sm12") {
  div(
    class = paste("ms-Grid-col", class),
    dir = "ltr",
    style = "padding: 10px;",
    ...
  )
}
fluent_card <- function(..., title = NULL) {
  Stack(
    class = "ms-depth-8",
    tokens = list(padding = 20, childrenGap = 10),
    if(!is.null(title)) Text(title, variant= "large"),
    ...
  )
}

fluent_card_panel <- function(..., title = NULL) {
  Stack(
    class = "ms-depth-8 centered",
    tokens = list(padding = 20, childrenGap = 10, maxWidth = "75rem"),
    if(!is.null(title)) Text(title, variant= "large"),
    ...
  )
}

fluent_header <- function(..., lang, title = NULL) {
  Stack(
    class = "ms-depth-8 ms-Grid",
    div(class = "ms-Grid-row",
      tags$a(
        href = route_link(lang),
        Image(class = "ms-Grid-col ms-sm8 ms-md8 ms-lg8 logo", src = "www/img/logo.png", alt = title)
      ),
      div(class = "ms-Grid-col ms-sm4 ms-md4 ms-lg4 tools", ...)
    )
  )
}

fluent_main <- function(...) {
  div(
    class = "col-sm-9",
    ...
  )
}
fluent_widgets <- function(...) {
  div(
    class = "col-sm-3", 
    div(
      class = "well", 
      ...
    )
  )
}

fluent_sidebar <- function(...) {
  div(
    class = "col-sm-3", 
    div(
      ...
    )
  )
}