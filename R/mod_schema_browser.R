#' schema_browser UI Function
#' @noRd
#' @importFrom shiny NS
mod_schema_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p("Schema Browser — coming soon")
  )
}

#' schema_browser Server Functions
#' @noRd
mod_schema_browser_server <- function(id, schema_data) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
