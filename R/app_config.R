#' Access files in the current app
#'
#' @param ... path elements passed to `system.file`
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "cdsvisualiser")
}

#' Read App Config
#'
#' @param value character, value to retrieve from config
#' @param config character, which config to use
#' @param use_parent logical, passed to config::get
#'
#' @importFrom golem get_golem_config
#' @noRd
get_golem_config <- function(value,
                              config = Sys.getenv(
                                "GOLEM_CONFIG_ACTIVE",
                                Sys.getenv(
                                  "R_CONFIG_ACTIVE",
                                  "default"
                                )
                              ),
                              use_parent = TRUE) {
  golem::get_golem_config(
    value = value,
    config = config,
    use_parent = use_parent,
    # Modify this if your config file is not golem-config.yml
    golem_config_file = app_sys("golem-config.yml")
  )
}
