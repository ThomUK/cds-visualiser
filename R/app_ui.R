#' The application User Interface
#'
#' @param request Internal parameter for shiny bookmarking
#' @importFrom shiny tagList
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_item bs_theme
#' @importFrom shinyWidgets actionBttn
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "CDS Visualiser",
      id = "main_nav",
      selected = "schema_browser",
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#005EB8",
        secondary = "#AEB7BD"
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("route"), "Explain CDS"),
        value = "process_guide",
        mod_process_guide_ui("process_guide")
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("sitemap"), "Schema Browser"),
        value = "schema_browser",
        mod_schema_browser_ui("schema_browser")
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("table"), "Data Dictionary"),
        value = "data_dictionary",
        mod_data_dictionary_ui("data_dictionary")
      ),
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("code"), "Synthetic Examples"),
        value = "synthetic_examples",
        mod_synthetic_examples_ui("synthetic_examples")
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        shinyWidgets::actionBttn(
          inputId = "reload_schema",
          label = "Reload Schema",
          icon = shiny::icon("rotate"),
          style = "minimal",
          size = "sm",
          color = "warning"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' @importFrom golem add_resource_path activate_js favicon
#' @importFrom shiny tags HTML
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    tags$title("CDS Visualiser"),
    tags$style(HTML("
      /* Shiny radioButtons: prevent label text wrapping under the button */
      .radio label {
        display: inline-flex;
        align-items: flex-start;
        gap: 0.4rem;
      }
      .radio label input[type='radio'] {
        flex-shrink: 0;
        margin-top: 0.25em;
        margin-right: 0;
      }

      /* Reactable sort indicators: stacked ▲▼ triangles.
         Grey by default; active triangle turns dark.
         Covers .rt-sort-right / .rt-sort-left (asc/desc)
         and .rt-sort (unsorted, aria-sort=none). */
      .rt-th[aria-sort] .rt-sort-right,
      .rt-th[aria-sort] .rt-sort-left,
      .rt-th[aria-sort=none] .rt-sort {
        display: inline-flex !important;
        flex-direction: column;
        align-items: center;
        padding-left: 4px !important;
        padding-right: 0 !important;
        line-height: 0 !important;
        gap: 1px;
      }
      .rt-th[aria-sort] .rt-sort-right::before,
      .rt-th[aria-sort] .rt-sort-left::before,
      .rt-th[aria-sort=none] .rt-sort::before {
        content: '▲';
        font-size: 0.5em;
        color: #ccc;
        line-height: 1;
      }
      .rt-th[aria-sort] .rt-sort-right::after,
      .rt-th[aria-sort] .rt-sort-left::after,
      .rt-th[aria-sort=none] .rt-sort::after {
        content: '▼' !important;
        font-size: 0.5em;
        color: #ccc !important;
        line-height: 1;
        padding-left: 0 !important;
        padding-right: 0 !important;
        opacity: 1 !important;
      }
      /* Active ascending: ▲ dark */
      .rt-th[aria-sort=ascending] .rt-sort-right::before,
      .rt-th[aria-sort=ascending] .rt-sort-left::before {
        color: #333;
      }
      /* Active descending: ▼ dark */
      .rt-th[aria-sort=descending] .rt-sort-right::after,
      .rt-th[aria-sort=descending] .rt-sort-left::after {
        color: #333 !important;
      }
    "))
  )
}
