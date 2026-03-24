# Run this script to re-parse all CDS XSD files and save schema_data.rds
# Usage: Rscript data-raw/parse_schema.R
#
# This is also triggered by the "Reload Schema" button in the Shiny app.

suppressMessages({
  library(here)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(glue)
  library(rlang)
})

# Source parser utils
source(here("R/utils_schema_parser.R"))

xsd_dir  <- here("dd_cds_6.3.1_20220629000001")
out_path <- here("inst/app/schema_data.rds")

message("Parsing CDS XSD files from: ", xsd_dir)
t0          <- proc.time()
schema_data <- parse_cds_schema(xsd_dir)
elapsed     <- round((proc.time() - t0)[["elapsed"]], 1)

message(sprintf(
  "Done in %.1fs: %d elements, %d types, %d enumeration values, %d CDS types",
  elapsed,
  nrow(schema_data$elements),
  nrow(schema_data$types),
  nrow(schema_data$enumerations),
  length(schema_data$tree_data)
))

saveRDS(schema_data, out_path)
message("Saved to: ", out_path)
