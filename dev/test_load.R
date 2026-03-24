library(golem)
library(shiny)
library(bslib)
library(shinyWidgets)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  tryCatch(source(f), error = function(e) message("Error in ", f, ": ", e$message))
}
cat("All sourced ok\n")
