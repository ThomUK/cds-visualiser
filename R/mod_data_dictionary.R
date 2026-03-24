#' data_dictionary UI Function
#' @noRd
#' @importFrom shiny NS
mod_data_dictionary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p("Data Dictionary — coming soon")
  )
}

#' data_dictionary Server Functions
#' @noRd
mod_data_dictionary_server <- function(id, schema_data) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
