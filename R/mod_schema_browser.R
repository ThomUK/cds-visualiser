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
      shiny::uiOutput(ns("field_filter_ui")),
      shiny::uiOutput(ns("ancestor_mode_ui"))
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center",
          "Element Tree",
          shiny::div(
            style = "flex:1; margin-left:0.75rem",
            shiny::textInput(
              ns("search"),
              NULL,
              placeholder = "Search\u2026",
              width = "100%"
            )
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
#' @importFrom shiny moduleServer reactive debounce req renderUI tagList tags observeEvent updateRadioButtons isolate
#' @importFrom dplyr filter
#' @importFrom purrr map_lgl
#' @importFrom stringr regex str_detect
#' @importFrom jsTreeR renderJstree jstree
mod_schema_browser_server <- function(id, schema_data, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    # --- Shared state sync ---
    shiny::observeEvent(
      input$cds_code,
      {
        if (!identical(shared$cds_code, input$cds_code)) {
          shared$cds_code <- input$cds_code
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(shared$cds_code, {
      if (!identical(input$cds_code, shared$cds_code)) {
        shiny::updateRadioButtons(
          session,
          "cds_code",
          selected = shared$cds_code
        )
      }
    })

    shiny::observeEvent(
      input$field_filter,
      {
        if (!identical(shared$field_filter, input$field_filter)) {
          shared$field_filter <- input$field_filter
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(shared$field_filter, {
      if (!identical(input$field_filter, shared$field_filter)) {
        shiny::updateRadioButtons(
          session,
          "field_filter",
          selected = shared$field_filter
        )
      }
    })
    search_term <- shiny::reactive({
      input$search
    }) |>
      shiny::debounce(300)

    cds_els <- shiny::reactive({
      shiny::req(schema_data())
      els <- schema_data()$elements
      if (input$cds_code == "all") {
        els
      } else {
        dplyr::filter(
          els,
          purrr::map_lgl(cds_types, ~ input$cds_code %in% .x)
        )
      }
    })

    counts <- shiny::reactive({
      all_els <- cds_els()

      # Apply search to get the base counts reflect what is visible
      term <- search_term()
      els <- if (!is.null(term) && nchar(trimws(term)) > 0) {
        dplyr::filter(
          all_els,
          stringr::str_detect(
            element_name,
            stringr::regex(term, ignore_case = TRUE)
          )
        )
      } else {
        all_els
      }

      ff <- input$field_filter %||% "required"
      primary <- switch(
        ff,
        all = els,
        required = dplyr::filter(els, is_required),
        optional = dplyr::filter(els, !is_required)
      )
      ax <- unique(unlist(lapply(primary$xpath, .ancestor_xpaths)))

      .n_types <- function(x) length(unique(unlist(x$cds_types)))
      list(
        n_all = nrow(els),
        n_req = sum(els$is_required),
        n_opt = sum(!els$is_required),
        n_primary = nrow(primary),
        n_with_ancestors = sum(all_els$xpath %in% ax),
        n_types_all = .n_types(els),
        n_types_req = .n_types(dplyr::filter(els, is_required)),
        n_types_opt = .n_types(dplyr::filter(els, !is_required)),
        n_types_primary = .n_types(primary)
      )
    })

    # Helper: label + right-aligned count on one row
    .count_label <- function(text, count_html) {
      shiny::tags$span(
        style = "display:inline-flex; justify-content:space-between; width:190px; vertical-align:middle",
        shiny::tags$span(text),
        shiny::tags$span(
          count_html,
          style = "color:#888; font-size:0.85em; text-align:right; margin-left:0.5rem"
        )
      )
    }

    .fmt_count <- function(n, n_types) paste0(n, " (+", n_types, ")")

    output$field_filter_ui <- shiny::renderUI({
      ct <- counts()
      shiny::radioButtons(
        session$ns("field_filter"),
        "Show fields",
        choiceNames = list(
          .count_label("All",           .fmt_count(ct$n_all, ct$n_types_all)),
          .count_label("Required only", .fmt_count(ct$n_req, ct$n_types_req)),
          .count_label("Optional only", .fmt_count(ct$n_opt, ct$n_types_opt))
        ),
        choiceValues = c("all", "required", "optional"),
        selected = shiny::isolate(input$field_filter) %||%
          shiny::isolate(shared$field_filter) %||%
          "required"
      )
    })

    output$ancestor_mode_ui <- shiny::renderUI({
      ct <- counts()
      extra <- ct$n_with_ancestors - ct$n_primary + ct$n_types_primary
      anc <- paste0(ct$n_primary, " (+", extra, ")")
      shiny::radioButtons(
        session$ns("ancestor_mode"),
        "Structure",
        choiceNames = list(
          .count_label("Items only",        .fmt_count(ct$n_primary, ct$n_types_primary)),
          .count_label("Include ancestors", anc)
        ),
        choiceValues = c("items", "ancestors"),
        selected = shiny::isolate(input$ancestor_mode) %||% "ancestors"
      )
    })

    filtered_elements <- shiny::reactive({
      shiny::req(cds_els(), input$field_filter, input$ancestor_mode)
      cds_els <- cds_els()

      # Apply field filter
      if (input$field_filter == "all") {
        els <- cds_els
      } else {
        primary <- switch(
          input$field_filter,
          required = dplyr::filter(cds_els, is_required),
          optional = dplyr::filter(cds_els, !is_required)
        )
        if (input$ancestor_mode == "ancestors") {
          ancestor_xpaths <- unique(unlist(lapply(
            primary$xpath,
            .ancestor_xpaths
          )))
          els <- dplyr::filter(cds_els, xpath %in% ancestor_xpaths)
        } else {
          els <- primary
        }
      }

      # Apply search filter
      term <- search_term()
      if (!is.null(term) && nchar(trimws(term)) > 0) {
        matching <- dplyr::filter(
          els,
          stringr::str_detect(
            element_name,
            stringr::regex(term, ignore_case = TRUE)
          )
        )
        if (input$ancestor_mode == "ancestors") {
          ancestor_xpaths <- unique(unlist(lapply(
            matching$xpath,
            .ancestor_xpaths
          )))
          els <- dplyr::filter(els, xpath %in% ancestor_xpaths)
        } else {
          els <- matching
        }
      }
      els
    })

    output$tree <- jsTreeR::renderJstree({
      els <- shiny::req(filtered_elements())
      shiny::req(nrow(els) > 0)

      cds_codes <- c("020", "120", "130", "140", "150", "160", "180", "190", "200")
      cds_labels <- c(
        "020" = "020 \u2013 Outpatient",
        "120" = "120 \u2013 Finished Birth Episode",
        "130" = "130 \u2013 Finished General Episode",
        "140" = "140 \u2013 Finished Delivery Episode",
        "150" = "150 \u2013 Other Birth Event",
        "160" = "160 \u2013 Other Delivery",
        "180" = "180 \u2013 Unfinished Birth Episode",
        "190" = "190 \u2013 Unfinished General Episode",
        "200" = "200 \u2013 Unfinished Delivery Episode"
      )
      term <- search_term()
      hl <- if (!is.null(term) && nchar(trimws(term)) > 0) trimws(term) else NULL

      nodes <- Filter(
        Negate(is.null),
        lapply(cds_codes, function(code) {
          code_els <- dplyr::filter(els, purrr::map_lgl(cds_types, ~ code %in% .x))
          if (nrow(code_els) == 0) return(NULL)
          list(
            text     = cds_labels[[code]],
            state    = list(opened = TRUE),
            children = .build_schema_tree(code_els, open_all = TRUE, highlight = hl)
          )
        })
      )

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

      # CDS type header nodes have no xpath — skip detail for those
      if (nchar(xpath) == 0) {
        return(shiny::tags$p(
          shiny::tags$em(
            "Select an element (not a CDS type header) to see its details."
          ),
          style = "color:#888"
        ))
      }

      sd <- schema_data()

      # Resolve CDS types this element belongs to
      el_row <- dplyr::filter(sd$elements, xpath == !!xpath)
      el_cds_types <- if (nrow(el_row) > 0) {
        sort(unique(unlist(el_row$cds_types)))
      } else {
        character(0)
      }

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
            if (length(el_cds_types) > 0) {
              shiny::tags$tr(
                shiny::tags$th("Parent CDS Type"),
                shiny::tags$td(paste(el_cds_types, collapse = ", "))
              )
            },
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
                      )
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
  vapply(
    seq_along(parts),
    function(i) paste0("/", paste(parts[1:i], collapse = "/")),
    character(1)
  )
}

#' Recursively build jsTreeR node list from flat elements table
#' @noRd
.build_schema_tree <- function(elements, open_all = FALSE, highlight = NULL) {
  is_root <- !elements$parent_xpath %in% elements$xpath
  roots <- elements[is_root, ]
  lapply(seq_len(nrow(roots)), function(i) {
    .make_node(roots[i, ], elements, open = TRUE, open_all = open_all, highlight = highlight)
  })
}

#' Build a single jsTreeR node (with recursive children)
#' @noRd
.make_node <- function(row, elements, open = FALSE, open_all = FALSE, highlight = NULL) {
  xp <- row$xpath

  name <- if (!is.null(highlight)) {
    escaped <- gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", highlight, perl = TRUE)
    gsub(
      paste0("(", escaped, ")"),
      '<span style="background:#c6efce; border-radius:2px">\\1</span>',
      row$element_name,
      ignore.case = TRUE,
      perl = TRUE
    )
  } else {
    row$element_name
  }

  label <- if (isTRUE(row$is_required)) name else paste0(name, " \u00b7")

  children_rows <- dplyr::filter(elements, parent_xpath == !!xp)

  node <- list(
    text = label,
    data = list(
      xpath = xp,
      element_name = row$element_name,
      type_name = row$type_name %||% "",
      is_required = isTRUE(row$is_required),
      annotation = row$annotation %||% ""
    ),
    state = list(opened = open || open_all),
    a_attr = list(
      style = if (!isTRUE(row$is_required)) {
        "background-color:#e0e0e0; border-radius:3px"
      } else {
        ""
      }
    )
  )

  if (nrow(children_rows) > 0) {
    node$children <- lapply(seq_len(nrow(children_rows)), function(i) {
      .make_node(
        children_rows[i, ],
        elements,
        open = open_all,
        open_all = open_all,
        highlight = highlight
      )
    })
  }

  node
}
