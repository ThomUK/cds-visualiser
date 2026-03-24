# Tests for pure-R helpers in mod_schema_browser.R
# (No Shiny dependency — these are plain functions)

# ---------------------------------------------------------------------------
# .dd_url()
# ---------------------------------------------------------------------------

test_that(".dd_url converts simple PascalCase correctly", {
  url <- .dd_url("ActivityTreatmentFunctionCode")
  expect_equal(url, "https://www.datadictionary.nhs.uk/data_elements/activity_treatment_function_code.html")
})

test_that(".dd_url handles leading acronym followed by mixed case", {
  url <- .dd_url("NHSNumber")
  expect_equal(url, "https://www.datadictionary.nhs.uk/data_elements/nhs_number.html")
})

test_that(".dd_url handles mid-string acronym", {
  url <- .dd_url("AgeAtCDSActivityDate")
  expect_equal(url, "https://www.datadictionary.nhs.uk/data_elements/age_at_cds_activity_date.html")
})

test_that(".dd_url always starts with the base NHS DD URL", {
  url <- .dd_url("SomeElement")
  expect_true(startsWith(url, "https://www.datadictionary.nhs.uk/data_elements/"))
})

test_that(".dd_url always ends with .html", {
  url <- .dd_url("SomeElement")
  expect_true(endsWith(url, ".html"))
})

test_that(".dd_url produces only lowercase slug", {
  url  <- .dd_url("AdmissionMethodCode")
  slug <- sub("https://www.datadictionary.nhs.uk/data_elements/", "", url, fixed = TRUE)
  slug <- sub(".html", "", slug, fixed = TRUE)
  expect_equal(slug, tolower(slug))
})

# ---------------------------------------------------------------------------
# .ancestor_xpaths()
# ---------------------------------------------------------------------------

test_that(".ancestor_xpaths returns all ancestor xpaths including self", {
  result <- .ancestor_xpaths("/Root/Parent/Child")
  expect_equal(result, c("/Root", "/Root/Parent", "/Root/Parent/Child"))
})

test_that(".ancestor_xpaths returns just self for a root-level xpath", {
  result <- .ancestor_xpaths("/Root")
  expect_equal(result, "/Root")
})

test_that(".ancestor_xpaths returns character vector", {
  result <- .ancestor_xpaths("/A/B/C/D")
  expect_type(result, "character")
  expect_length(result, 4L)
})

test_that(".ancestor_xpaths each result starts with /", {
  result <- .ancestor_xpaths("/Root/Parent/Child")
  expect_true(all(startsWith(result, "/")))
})

# ---------------------------------------------------------------------------
# .build_schema_tree() / .make_node()
# ---------------------------------------------------------------------------

# Minimal synthetic elements table for tree-building tests
make_test_elements <- function() {
  tibble::tibble(
    xpath        = c("/Root", "/Root/GroupA", "/Root/GroupA/LeafOne", "/Root/LeafTwo"),
    element_name = c("Root", "GroupA", "LeafOne", "LeafTwo"),
    parent_xpath = c("", "/Root", "/Root/GroupA", "/Root"),
    type_name    = c(NA_character_, NA_character_, "SomeType", "OtherType"),
    is_required  = c(TRUE, TRUE, TRUE, FALSE),
    annotation   = c(NA_character_, NA_character_, "Leaf one desc", NA_character_)
  )
}

test_that(".build_schema_tree returns a list", {
  nodes <- .build_schema_tree(make_test_elements())
  expect_type(nodes, "list")
})

test_that(".build_schema_tree root node has correct text", {
  nodes <- .build_schema_tree(make_test_elements())
  expect_equal(nodes[[1]]$text, "Root")
})

test_that(".build_schema_tree root node is opened by default", {
  nodes <- .build_schema_tree(make_test_elements())
  expect_true(nodes[[1]]$state$opened)
})

test_that(".build_schema_tree child nodes are closed by default", {
  nodes    <- .build_schema_tree(make_test_elements())
  children <- nodes[[1]]$children
  expect_false(children[[1]]$state$opened)
})

test_that(".build_schema_tree open_all=TRUE opens all nodes", {
  nodes    <- .build_schema_tree(make_test_elements(), open_all = TRUE)
  children <- nodes[[1]]$children
  expect_true(all(vapply(children, function(n) isTRUE(n$state$opened), logical(1))))
})

test_that(".make_node appends middle dot to optional element label", {
  els <- make_test_elements()
  row <- els[els$element_name == "LeafTwo", ]
  node <- .make_node(row, els)
  expect_true(endsWith(node$text, "\u00b7"))
})

test_that(".make_node does not append dot to required element label", {
  els <- make_test_elements()
  row <- els[els$element_name == "LeafOne", ]
  node <- .make_node(row, els)
  expect_false(endsWith(node$text, "\u00b7"))
})

test_that(".make_node data contains expected fields", {
  els  <- make_test_elements()
  row  <- els[els$element_name == "LeafOne", ]
  node <- .make_node(row, els)
  expect_true(all(c("xpath", "element_name", "type_name", "is_required", "annotation") %in% names(node$data)))
})

test_that(".make_node leaf node has no children", {
  els  <- make_test_elements()
  row  <- els[els$element_name == "LeafOne", ]
  node <- .make_node(row, els)
  expect_null(node$children)
})

test_that(".make_node parent node has children list", {
  els  <- make_test_elements()
  row  <- els[els$element_name == "GroupA", ]
  node <- .make_node(row, els)
  expect_type(node$children, "list")
  expect_length(node$children, 1L)
})
