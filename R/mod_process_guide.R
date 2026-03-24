#' process_guide UI Function
#' @noRd
#' @importFrom shiny NS
mod_process_guide_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p("Process Guide — coming soon")
  )
}

#' process_guide Server Functions
#' @noRd
mod_process_guide_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}
