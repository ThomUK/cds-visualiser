#' The application server-side
#'
#' @param input,output,session Internal parameters for shiny
#' @importFrom shiny reactiveVal reactiveValues observeEvent showNotification removeNotification
#' @noRd
app_server <- function(input, output, session) {
  # Shared sidebar filter state — persists when switching tabs
  shared <- shiny::reactiveValues(cds_code = "130", field_filter = "required")
  # --- Shared schema data reactive ---
  schema_rds_path <- system.file(
    "extdata/schema_data.rds",
    package = "cds-visualiser"
  )

  schema_data <- shiny::reactiveVal(
    if (file.exists(schema_rds_path)) {
      readRDS(schema_rds_path)
    } else {
      NULL
    }
  )

  # --- Reload schema button ---
  shiny::observeEvent(input$reload_schema, {
    shiny::showNotification(
      "Parsing XSD files — this may take a few seconds...",
      type = "message",
      duration = NULL,
      id = "parsing_notif"
    )

    tryCatch(
      {
        xsd_dir <- system.file("extdata/xsd", package = "cds-visualiser")
        new_data <- parse_cds_schema(xsd_dir)

        # Persist for future app restarts
        saveRDS(
          new_data,
          system.file("extdata/schema_data.rds", package = "cds-visualiser")
        )

        schema_data(new_data)
        shiny::removeNotification("parsing_notif")
        shiny::showNotification(
          "Schema reloaded successfully.",
          type = "message"
        )
      },
      error = function(e) {
        shiny::removeNotification("parsing_notif")
        shiny::showNotification(
          glue::glue("Parse error: {conditionMessage(e)}"),
          type = "error",
          duration = 10
        )
      }
    )
  })

  # --- Module servers ---
  mod_schema_browser_server("schema_browser", schema_data, shared)
  mod_data_dictionary_server("data_dictionary", schema_data, shared)
  mod_process_guide_server("process_guide")
  mod_synthetic_examples_server("synthetic_examples", schema_data, shared)
  mod_make_cds_fun_server("make_cds_fun")
}
