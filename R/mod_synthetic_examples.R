#' synthetic_examples UI Function
#' @noRd
#' @importFrom shiny NS checkboxInput actionButton downloadButton icon hr
#' @importFrom bslib layout_sidebar sidebar card card_header
#' @importFrom shinyAce aceEditor
mod_synthetic_examples_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 280,
      shiny::selectInput(
        ns("cds_code"), "CDS Type",
        choices = c(
          "020 \u2013 Outpatient"                  = "020",
          "120 \u2013 Finished Birth Episode"      = "120",
          "130 \u2013 Finished General Episode"    = "130",
          "140 \u2013 Finished Delivery Episode"   = "140",
          "150 \u2013 Other Birth Event"           = "150",
          "160 \u2013 Other Delivery"              = "160",
          "180 \u2013 Unfinished Birth Episode"    = "180",
          "190 \u2013 Unfinished General Episode"  = "190",
          "200 \u2013 Unfinished Delivery Episode" = "200"
        ),
        selected = "130"
      ),
      shiny::checkboxInput(ns("include_optional"), "Include optional fields", value = FALSE),
      shiny::actionButton(
        ns("generate"), "Generate XML",
        icon  = shiny::icon("code"),
        class = "btn-primary w-100 mt-2"
      ),
      shiny::hr(),
      shiny::downloadButton(ns("download_xml"), "Download XML", class = "w-100")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Generated XML"),
      shinyAce::aceEditor(
        outputId = ns("xml_display"),
        value    = "# Select a CDS type and click Generate XML",
        mode     = "xml",
        theme    = "chrome",
        readOnly = TRUE,
        height   = "600px",
        fontSize = 13
      )
    )
  )
}

#' synthetic_examples Server Functions
#' @noRd
#' @importFrom shiny moduleServer req eventReactive observeEvent downloadHandler
#' @importFrom shinyAce updateAceEditor
mod_synthetic_examples_server <- function(id, schema_data) {
  shiny::moduleServer(id, function(input, output, session) {

    xml_result <- shiny::eventReactive(input$generate, {
      shiny::req(schema_data())
      generate_synthetic_xml(
        schema_data      = schema_data(),
        cds_code         = input$cds_code,
        include_optional = input$include_optional
      )
    })

    shiny::observeEvent(xml_result(), {
      shinyAce::updateAceEditor(session, "xml_display", value = xml_result())
    })

    output$download_xml <- shiny::downloadHandler(
      filename = function() paste0("CDS_", input$cds_code, "_synthetic.xml"),
      content  = function(file) writeLines(xml_result(), file)
    )
  })
}
