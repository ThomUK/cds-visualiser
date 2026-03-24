#' data_dictionary UI Function
#' @noRd
#' @importFrom shiny NS checkboxInput uiOutput hr
#' @importFrom bslib layout_sidebar sidebar
#' @importFrom reactable reactableOutput
mod_data_dictionary_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 260,
      shiny::selectInput(
        ns("cds_filter"), "CDS Type",
        choices = c(
          "All types"                              = "all",
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
      shiny::checkboxInput(ns("required_only"), "Required fields only", value = FALSE),
      shiny::hr(),
      shiny::uiOutput(ns("type_detail"))
    ),
    reactable::reactableOutput(ns("table"))
  )
}

#' data_dictionary Server Functions
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI tagList tags
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_lgl
#' @importFrom stringr str_count str_replace_all
#' @importFrom reactable renderReactable reactable colDef getReactableState
mod_data_dictionary_server <- function(id, schema_data) {
  shiny::moduleServer(id, function(input, output, session) {
    filtered_elements <- shiny::reactive({
      shiny::req(schema_data())
      els <- schema_data()$elements

      if (input$cds_filter != "all") {
        code <- input$cds_filter
        els <- dplyr::filter(els, purrr::map_lgl(cds_types, ~ code %in% .x))
      }
      if (input$required_only) {
        els <- dplyr::filter(els, is_required)
      }

      dplyr::mutate(els,
        path_label = stringr::str_replace_all(xpath, "^/", "") |>
          stringr::str_replace_all("/", " \u203a ")
      ) |>
        dplyr::select(element_name, type_name, is_required, path_label, xpath, annotation)
    })

    output$table <- reactable::renderReactable({
      shiny::req(filtered_elements())
      reactable::reactable(
        dplyr::select(filtered_elements(), element_name, type_name, is_required, path_label, annotation),
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        selection = "single",
        onClick = "select",
        defaultPageSize = 20,
        columns = list(
          element_name = reactable::colDef(name = "Element", minWidth = 160),
          type_name = reactable::colDef(name = "Type", minWidth = 160, na = "\u2013"),
          is_required = reactable::colDef(
            name     = "Required",
            maxWidth = 90,
            cell     = function(val) if (isTRUE(val)) "\u2713" else ""
          ),
          path_label = reactable::colDef(name = "Path", minWidth = 220),
          annotation = reactable::colDef(name = "Description", minWidth = 200, na = "")
        )
      )
    })

    selected_row <- shiny::reactive({
      idx <- reactable::getReactableState("table", "selected")
      shiny::req(idx)
      filtered_elements()[idx, ]
    })

    output$type_detail <- shiny::renderUI({
      row <- shiny::req(selected_row())
      type_nm <- row$type_name %||% ""
      if (is.na(type_nm)) type_nm <- ""
      sd <- schema_data()

      if (nchar(type_nm) > 0) {
        type_info <- dplyr::filter(sd$types, type_name == !!type_nm)
        enums <- dplyr::filter(sd$enumerations, type_name == !!type_nm)
      } else {
        type_info <- sd$types[0L, ]
        enums <- sd$enumerations[0L, ]
      }

      shiny::tagList(
        shiny::tags$strong(row$element_name),
        shiny::tags$p(
          shiny::tags$em(if (!is.na(type_nm)) type_nm else "(complex type)"),
          style = "font-size:0.85em; color:#666; margin-bottom:4px"
        ),
        if (!is.na(row$annotation) && nchar(row$annotation) > 0) {
          shiny::tags$p(row$annotation, style = "font-size:0.85em")
        },
        if (nrow(type_info) > 0) {
          constraints <- list(
            "Base type"    = type_info$base_type[[1]],
            "Pattern"      = type_info$pattern[[1]],
            "Max length"   = type_info$max_length[[1]],
            "Fixed length" = type_info$length[[1]],
            "Min value"    = type_info$min_inclusive[[1]],
            "Max value"    = type_info$max_inclusive[[1]]
          )
          constraints <- Filter(function(x) !is.null(x) && !is.na(x), constraints)
          if (length(constraints) > 0) {
            shiny::tags$dl(
              class = "row",
              style = "font-size:0.82em; margin-bottom:4px",
              lapply(names(constraints), function(nm) {
                list(
                  shiny::tags$dt(class = "col-6", nm),
                  shiny::tags$dd(class = "col-6", as.character(constraints[[nm]]))
                )
              })
            )
          }
        },
        if (nrow(enums) > 0) {
          shiny::tagList(
            shiny::tags$p(
              shiny::tags$strong(glue::glue("Valid codes ({nrow(enums)})")),
              style = "font-size:0.85em; margin-bottom:2px"
            ),
            shiny::tags$div(
              style = "max-height:220px; overflow-y:auto; font-size:0.8em",
              shiny::tags$table(
                class = "table table-sm table-hover",
                shiny::tags$tbody(
                  lapply(seq_len(nrow(enums)), function(i) {
                    shiny::tags$tr(
                      shiny::tags$td(enums$value[[i]], style = "font-weight:600; white-space:nowrap"),
                      shiny::tags$td(enums$annotation[[i]] %||% "")
                    )
                  })
                )
              )
            )
          )
        }
      )
    })
  })
}
