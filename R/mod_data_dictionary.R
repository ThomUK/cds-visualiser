#' data_dictionary UI Function
#' @noRd
#' @importFrom shiny NS radioButtons uiOutput
#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
mod_data_dictionary_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 260,
      shiny::radioButtons(
        ns("cds_code"),
        "CDS Type",
        choices = c(
          "All" = "all",
          "020 \u2013 Outpatient" = "020",
          "120 \u2013 Finished Birth Episode" = "120",
          "130 \u2013 Finished General Episode" = "130",
          "140 \u2013 Finished Delivery Episode" = "140",
          "150 \u2013 Other Birth Event" = "150",
          "160 \u2013 Other Delivery" = "160",
          "180 \u2013 Unfinished Birth Episode" = "180",
          "190 \u2013 Unfinished General Episode" = "190",
          "200 \u2013 Unfinished Delivery Episode" = "200"
        ),
        selected = "130"
      ),
      shiny::uiOutput(ns("field_filter_ui"))
    ),
    reactable::reactableOutput(ns("table"))
  )
}

#' data_dictionary Server Functions
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI tags observeEvent updateRadioButtons isolate
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_lgl
#' @importFrom stringr str_replace_all
#' @importFrom reactable renderReactable reactable colDef
mod_data_dictionary_server <- function(id, schema_data, shared) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Shared state sync ---
    shiny::observeEvent(input$cds_code, {
      if (!identical(shared$cds_code, input$cds_code)) shared$cds_code <- input$cds_code
    }, ignoreInit = TRUE)
    shiny::observeEvent(shared$cds_code, {
      if (!identical(input$cds_code, shared$cds_code))
        shiny::updateRadioButtons(session, "cds_code", selected = shared$cds_code)
    })

    shiny::observeEvent(input$field_filter, {
      if (!identical(shared$field_filter, input$field_filter)) shared$field_filter <- input$field_filter
    }, ignoreInit = TRUE)
    shiny::observeEvent(shared$field_filter, {
      if (!identical(input$field_filter, shared$field_filter))
        shiny::updateRadioButtons(session, "field_filter", selected = shared$field_filter)
    })

    # --- Filtered elements ---
    cds_els <- shiny::reactive({
      shiny::req(schema_data())
      els <- schema_data()$elements
      if (input$cds_code == "all") els else dplyr::filter(
        els,
        purrr::map_lgl(cds_types, ~ input$cds_code %in% .x)
      )
    })

    counts <- shiny::reactive({
      els <- cds_els()
      ff  <- input$field_filter %||% "required"
      primary <- switch(ff,
        all      = els,
        required = dplyr::filter(els,  is_required),
        optional = dplyr::filter(els, !is_required)
      )
      list(
        n_all     = nrow(els),
        n_req     = sum(els$is_required),
        n_opt     = sum(!els$is_required),
        n_primary = nrow(primary)
      )
    })

    .count_label <- function(text, count_html) {
      shiny::tags$span(
        style = "display:inline-flex; justify-content:space-between; width:190px; vertical-align:middle",
        shiny::tags$span(text),
        shiny::tags$span(count_html, style = "color:#888; font-size:0.85em; text-align:right; margin-left:0.5rem")
      )
    }

    output$field_filter_ui <- shiny::renderUI({
      ct <- counts()
      shiny::radioButtons(
        session$ns("field_filter"),
        "Show fields",
        choiceNames  = list(
          .count_label("All",           ct$n_all),
          .count_label("Required only", ct$n_req),
          .count_label("Optional only", ct$n_opt)
        ),
        choiceValues = c("all", "required", "optional"),
        selected     = shiny::isolate(input$field_filter) %||% shiny::isolate(shared$field_filter) %||% "required"
      )
    })

    filtered_elements <- shiny::reactive({
      shiny::req(cds_els(), input$field_filter)
      els <- cds_els()
      els <- switch(input$field_filter,
        all      = els,
        required = dplyr::filter(els,  is_required),
        optional = dplyr::filter(els, !is_required)
      )
      dplyr::mutate(els,
        path_label = stringr::str_replace_all(xpath, "^/", "") |>
          stringr::str_replace_all("/", " \u203a ")
      ) |>
        dplyr::select(element_name, type_name, is_required, path_label)
    })

    output$table <- reactable::renderReactable({
      shiny::req(filtered_elements())

      tbl_id <- session$ns("table")

      .hdr <- function(label) label

      # Filter input with placeholder text
      .flt <- function(placeholder) {
        function(values, name) {
          shiny::tags$input(
            type        = "text",
            placeholder = placeholder,
            oninput     = paste0(
              "Reactable.setFilter('", tbl_id, "', '", name,
              "', event.target.value || undefined)"
            ),
            style = "width:100%; font-size:0.85em; padding:2px 6px; border:1px solid #ddd; border-radius:3px"
          )
        }
      }

      tbl <- reactable::reactable(
        dplyr::select(filtered_elements(), element_name, type_name, is_required, path_label),
        filterable      = TRUE,
        sortable        = TRUE,
        striped         = TRUE,
        highlight       = TRUE,
        defaultPageSize = 20,
        elementId       = tbl_id,
        columns = list(
          element_name = reactable::colDef(
            header           = .hdr("Element"),
            minWidth         = 160,
            defaultSortOrder = "desc",
            filterInput      = .flt("Search\u2026")
          ),
          type_name    = reactable::colDef(
            header           = .hdr("Type"),
            minWidth         = 160,
            na               = "\u2013",
            defaultSortOrder = "desc",
            filterInput      = .flt("Search\u2026")
          ),
          is_required  = reactable::colDef(
            header           = .hdr("Required"),
            maxWidth         = 95,
            defaultSortOrder = "desc",
            cell             = function(val) if (isTRUE(val)) "Y" else "",
            filterInput      = .flt("Search\u2026")
          ),
          path_label   = reactable::colDef(
            header           = .hdr("Path"),
            minWidth         = 220,
            defaultSortOrder = "desc",
            filterInput      = .flt("Search\u2026")
          )
        )
      )

      tbl
    })

  })
}
