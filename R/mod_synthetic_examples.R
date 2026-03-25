#' synthetic_examples UI Function
#' @noRd
#' @importFrom shiny NS checkboxInput actionButton downloadButton icon hr tagList
#' @importFrom bslib layout_sidebar sidebar card card_header card_body
#' @importFrom shinyAce aceEditor
#' @importFrom reactable reactableOutput
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
        icon = shiny::icon("code"),
        class = "btn-primary w-100 mt-2"
      ),
      shiny::hr(),
      shiny::downloadButton(ns("download_xml"), "Download XML", class = "w-100")
    ),
    shiny::tagList(
      bslib::card(
        bslib::card_header("Item Values"),
        reactable::reactableOutput(ns("items_table"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Generated XML"),
        bslib::card_body(
          style = "padding:0; overflow:hidden",
          shinyAce::aceEditor(
            outputId = ns("xml_display"),
            value    = "# Select a CDS type and click Generate XML",
            mode     = "xml",
            theme    = "chrome",
            readOnly = TRUE,
            height   = "450px",
            fontSize = 13
          )
        )
      )
    )
  )
}

#' synthetic_examples Server Functions
#' @noRd
#' @importFrom shiny moduleServer req eventReactive observeEvent downloadHandler
#' @importFrom shinyAce updateAceEditor
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom xml2 read_xml xml_find_all xml_name xml_text
#' @importFrom dplyr select distinct left_join
#' @importFrom tibble tibble
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

    output$items_table <- reactable::renderReactable({
      xml <- shiny::req(xml_result())
      sd <- schema_data()

      # Parse XML and extract all leaf nodes (elements with text content)
      doc <- xml2::read_xml(xml)
      leaves <- xml2::xml_find_all(doc, "//*[not(*)]")

      items <- tibble::tibble(
        element_name = xml2::xml_name(leaves),
        value        = xml2::xml_text(leaves)
      )

      # Join annotation from schema for context
      annotations <- dplyr::distinct(
        dplyr::select(sd$elements, element_name, annotation),
        element_name,
        .keep_all = TRUE
      )
      items <- dplyr::left_join(items, annotations, by = "element_name")

      reactable::reactable(
        items,
        striped = TRUE,
        highlight = TRUE,
        searchable = TRUE,
        defaultPageSize = 20,
        columns = list(
          element_name = reactable::colDef(name = "Item", minWidth = 180),
          value        = reactable::colDef(name = "Value", minWidth = 120),
          annotation   = reactable::colDef(name = "Description", minWidth = 200, na = "")
        )
      )
    })

    output$download_xml <- shiny::downloadHandler(
      filename = function() paste0("CDS_", input$cds_code, "_synthetic.xml"),
      content  = function(file) writeLines(xml_result(), file)
    )
  })
}
