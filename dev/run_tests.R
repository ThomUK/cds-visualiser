# Test runner script — run from project root:
#   Rscript dev/run_tests.R [test_file_pattern]
#
# Usage examples:
#   Rscript dev/run_tests.R                   # run all tests
#   Rscript dev/run_tests.R schema_parser     # run schema parser tests only
#   Rscript dev/run_tests.R xml_generator     # run XML generator tests only

args <- commandArgs(trailingOnly = TRUE)
pattern <- if (length(args) > 0) args[[1]] else NULL

suppressMessages({
  library(testthat)
  library(here)
  library(golem)
  library(shiny)
  library(bslib)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(readr)
  library(rlang)
  library(glue)
})

# Source all R files (simulates load_all without devtools crash)
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f)

# Run tests
if (is.null(pattern)) {
  test_dir("tests/testthat", reporter = "progress")
} else {
  test_file(
    list.files("tests/testthat", pattern = pattern, full.names = TRUE)[[1]],
    reporter = "progress"
  )
}
