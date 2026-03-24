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
      id    = "main_nav",
      theme = bslib::bs_theme(
        version    = 5,
        bootswatch = "flatly",
        primary    = "#005EB8",
        secondary  = "#AEB7BD"
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
        title = shiny::tagList(shiny::icon("route"), "Process Guide"),
        value = "process_guide",
        mod_process_guide_ui("process_guide")
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
          label   = "Reload Schema",
          icon    = shiny::icon("rotate"),
          style   = "minimal",
          size    = "sm",
          color   = "warning"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shiny tags
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CDS Visualiser"
    )
  )
}
