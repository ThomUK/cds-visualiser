#' synthetic_examples UI Function
#' @noRd
#' @importFrom shiny NS
mod_synthetic_examples_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p("Synthetic Examples — coming soon")
  )
}

#' synthetic_examples Server Functions
#' @noRd
mod_synthetic_examples_server <- function(id, schema_data) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
