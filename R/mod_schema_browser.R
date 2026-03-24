#' schema_browser UI Function
#' @noRd
#' @importFrom shiny NS radioButtons textInput uiOutput
#' @importFrom bslib layout_sidebar sidebar layout_columns card card_header card_body
#' @importFrom jsTreeR jstreeOutput
mod_schema_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 260,
      shiny::selectInput(
        ns("cds_code"),
        "CDS Type",
        choices = c(
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
      shiny::radioButtons(
        ns("field_filter"),
        "Show fields",
        choices = c(
          "All" = "all",
          "Required only" = "required",
          "Optional only" = "optional"
        ),
        selected = "required"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center gap-2",
          "Element Tree",
          shiny::textInput(
            ns("search"), NULL,
            placeholder = "Search\u2026",
            width = "160px"
          )
        ),
        jsTreeR::jstreeOutput(ns("tree"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Element Detail"),
        bslib::card_body(shiny::uiOutput(ns("detail")))
      )
    )
  )
}

#' schema_browser Server Functions
#' @noRd
#' @importFrom shiny moduleServer reactive debounce req renderUI tagList tags
#' @importFrom dplyr filter
#' @importFrom purrr map_lgl
#' @importFrom stringr regex str_detect
#' @importFrom jsTreeR renderJstree jstree
mod_schema_browser_server <- function(id, schema_data) {
  shiny::moduleServer(id, function(input, output, session) {

    search_term <- shiny::reactive({ input$search }) |> shiny::debounce(300)

    filtered_elements <- shiny::reactive({
      shiny::req(schema_data())
      code <- input$cds_code
      els <- dplyr::filter(
        schema_data()$elements,
        purrr::map_lgl(cds_types, ~ code %in% .x)
      )
      els <- switch(input$field_filter,
        required = dplyr::filter(els, is_required),
        optional = dplyr::filter(els, !is_required),
        els
      )
      term <- search_term()
      if (!is.null(term) && nchar(trimws(term)) > 0) {
        # Find matching elements, then expand to include all ancestors
        matching <- dplyr::filter(els, stringr::str_detect(element_name, stringr::regex(term, ignore_case = TRUE)))
        ancestor_xpaths <- unique(unlist(lapply(matching$xpath, .ancestor_xpaths)))
        els <- dplyr::filter(els, xpath %in% ancestor_xpaths)
      }
      els
    })

    output$tree <- jsTreeR::renderJstree({
      els  <- shiny::req(filtered_elements())
      shiny::req(nrow(els) > 0)
      term <- search_term()
      open_all <- !is.null(term) && nchar(trimws(term)) > 0
      nodes <- .build_schema_tree(els, open_all = open_all)
      jsTreeR::jstree(nodes, wholerow = TRUE)
    })

    output$detail <- shiny::renderUI({
      sel <- input$tree_selected # list of selected jsTree nodes
      if (is.null(sel) || length(sel) == 0) {
        return(shiny::tags$p(
          shiny::tags$em("Click an element in the tree to see its details."),
          style = "color:#888"
        ))
      }

      node_data <- sel[[1]]$data
      # Fields arrive as NULL (not NA) after JSON round-trip when original was NA
      type_nm <- node_data$type_name %||% ""
      ann <- node_data$annotation %||% ""
      xpath <- node_data$xpath %||% ""
      is_req <- isTRUE(node_data$is_required)
      label <- sel[[1]]$text %||% ""

      sd <- schema_data()

      # Guard against filtering with empty type_nm
      if (nchar(type_nm) > 0) {
        type_info <- dplyr::filter(sd$types, type_name == !!type_nm)
        enums <- dplyr::filter(sd$enumerations, type_name == !!type_nm)
      } else {
        type_info <- sd$types[0L, ]
        enums <- sd$enumerations[0L, ]
      }

      shiny::tagList(
        shiny::tags$h6(label, style = "font-weight:600"),
        shiny::tags$table(
          class = "table table-sm",
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$th("Required"),
              shiny::tags$td(if (is_req) "Yes" else "No")
            ),
            shiny::tags$tr(
              shiny::tags$th("Type"),
              shiny::tags$td(
                if (nchar(type_nm) > 0) type_nm else shiny::tags$em("(complex)")
              )
            ),
            shiny::tags$tr(
              shiny::tags$th("XPath"),
              shiny::tags$td(
                shiny::tags$code(
                  xpath,
                  style = "font-size:0.78em; word-break:break-all"
                )
              )
            )
          )
        ),

        # NHS Data Dictionary link — only for leaf elements (simple types only)
        if (nrow(type_info) > 0) {
          shiny::tags$p(
            shiny::tags$a(
              href = .dd_url(node_data$element_name %||% label),
              target = "_blank",
              rel = "noopener noreferrer",
              shiny::icon("arrow-up-right-from-square"),
              " NHS Data Dictionary",
              style = "font-size:0.85em"
            ),
            shiny::tags$span(
              " (may not exist for all elements)",
              style = "font-size:0.75em; color:#888"
            )
          )
        },
        if (nchar(ann) > 0) {
          shiny::tags$p(ann, style = "font-size:0.9em; margin-top:4px")
        },
        if (nrow(type_info) > 0) {
          constraints <- list(
            "Base type" = type_info$base_type[[1]],
            "Pattern" = type_info$pattern[[1]],
            "Max length" = type_info$max_length[[1]],
            "Fixed length" = type_info$length[[1]],
            "Min value" = type_info$min_inclusive[[1]],
            "Max value" = type_info$max_inclusive[[1]]
          )
          constraints <- Filter(
            function(x) !is.null(x) && !is.na(x),
            constraints
          )
          if (length(constraints) > 0) {
            shiny::tagList(
              shiny::tags$p(
                shiny::tags$strong("Constraints:"),
                style = "margin-bottom:4px"
              ),
              shiny::tags$dl(
                class = "row",
                style = "font-size:0.85em",
                lapply(names(constraints), function(nm) {
                  list(
                    shiny::tags$dt(class = "col-5", nm),
                    shiny::tags$dd(
                      class = "col-7",
                      as.character(constraints[[nm]])
                    )
                  )
                })
              )
            )
          }
        },
        if (nrow(enums) > 0) {
          shiny::tagList(
            shiny::tags$p(
              shiny::tags$strong(glue::glue("Valid codes ({nrow(enums)})")),
              style = "margin-bottom:4px"
            ),
            shiny::tags$div(
              style = "max-height:300px; overflow-y:auto; font-size:0.85em",
              shiny::tags$table(
                class = "table table-sm table-striped",
                shiny::tags$tbody(
                  lapply(seq_len(nrow(enums)), function(i) {
                    shiny::tags$tr(
                      shiny::tags$td(
                        enums$value[[i]],
                        style = "font-weight:600; white-space:nowrap"
                      ),
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

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Convert a PascalCase element name to an NHS Data Dictionary URL
#' Best-effort: structural container elements may not have a dictionary page.
#' @noRd
.dd_url <- function(element_name) {
  slug <- element_name |>
    gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x = _) |>
    gsub("([a-z0-9])([A-Z])", "\\1_\\2", x = _) |>
    tolower()
  paste0("https://www.datadictionary.nhs.uk/data_elements/", slug, ".html")
}

#' Return all ancestor xpaths of a given xpath (including itself)
#' @noRd
.ancestor_xpaths <- function(xpath) {
  parts <- strsplit(xpath, "/")[[1]]
  parts <- parts[nchar(parts) > 0]
  vapply(seq_along(parts), function(i) paste0("/", paste(parts[1:i], collapse = "/")), character(1))
}

#' Recursively build jsTreeR node list from flat elements table
#' @noRd
.build_schema_tree <- function(elements, open_all = FALSE) {
  is_root <- !elements$parent_xpath %in% elements$xpath
  roots   <- elements[is_root, ]
  lapply(seq_len(nrow(roots)), function(i) {
    .make_node(roots[i, ], elements, open = TRUE, open_all = open_all)
  })
}

#' Build a single jsTreeR node (with recursive children)
#' @noRd
.make_node <- function(row, elements, open = FALSE, open_all = FALSE) {
  xp <- row$xpath
  label <- if (isTRUE(row$is_required)) {
    row$element_name
  } else {
    paste0(row$element_name, " \u00b7")
  }

  children_rows <- dplyr::filter(elements, parent_xpath == !!xp)

  node <- list(
    text = label,
    data = list(
      xpath        = xp,
      element_name = row$element_name,
      type_name    = row$type_name %||% "",
      is_required  = isTRUE(row$is_required),
      annotation   = row$annotation %||% ""
    ),
    state = list(opened = open || open_all)
  )

  if (nrow(children_rows) > 0) {
    node$children <- lapply(seq_len(nrow(children_rows)), function(i) {
      .make_node(children_rows[i, ], elements, open = open_all, open_all = open_all)
    })
  }

  node
}
