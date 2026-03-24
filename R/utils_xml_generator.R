# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Generate a synthetic CDS XML example for a given CDS type
#'
#' Produces a minimal valid XML document using required fields only
#' (or all fields when include_optional = TRUE). All values are plausible
#' placeholders derived from the schema constraints.
#'
#' @param schema_data A parsed schema list as returned by parse_cds_schema().
#' @param cds_code Character. One of "020","120","130","140","150","160",
#'   "180","190","200".
#' @param include_optional Logical. If TRUE, optional fields are also included.
#' @return A character string of formatted XML.
#' @export
generate_synthetic_xml <- function(schema_data, cds_code,
                                   include_optional = FALSE) {
  elements <- schema_data$elements |>
    dplyr::filter(purrr::map_lgl(cds_types, ~ cds_code %in% .x)) |>
    dplyr::arrange(xpath)

  if (!include_optional) {
    elements <- dplyr::filter(elements, is_required)
  }

  if (nrow(elements) == 0L) {
    stop(glue::glue("No elements found for CDS type '{cds_code}'"))
  }

  # Build nested XML document from the element tree
  root_xpath <- elements$xpath[[1]]
  root_name <- stringr::str_extract(root_xpath, "^/([^/]+)", group = 1)

  doc <- xml2::xml_new_root(root_name)
  root_el <- xml2::xml_root(doc)

  # Walk every element in xpath order, creating nodes as needed
  for (i in seq_len(nrow(elements))) {
    row <- elements[i, ]
    xpath <- row$xpath
    el_name <- row$element_name
    type_name <- row$type_name
    max_occ <- row$max_occurs

    # Skip the root itself (already created)
    if (xpath == root_xpath) next

    # Find or create the parent node
    parent_xpath <- row$parent_xpath
    parent_node <- find_or_create_node(root_el, parent_xpath, root_xpath)
    if (is.null(parent_node)) next

    # Skip if this element already exists under parent (avoid duplicates)
    existing <- xml2::xml_find_first(
      parent_node,
      glue::glue("./{el_name}")
    )
    if (!inherits(existing, "xml_missing")) next

    child <- xml2::xml_add_child(parent_node, el_name)

    # Leaf element: set text content if it has a simple type
    is_leaf <- !is.na(type_name) && type_name %in% schema_data$types$type_name
    if (is_leaf) {
      val <- placeholder_value(type_name, schema_data)
      xml2::xml_text(child) <- val
    }
  }

  # Serialise to formatted XML string
  raw_xml <- as.character(doc)
  format_xml(raw_xml)
}

# ---------------------------------------------------------------------------
# placeholder_value()
# ---------------------------------------------------------------------------

#' Generate a plausible placeholder value for a given schema type
#'
#' @param type_name Character. A type name from the schema types tibble.
#' @param schema_data A parsed schema list.
#' @return A character scalar.
#' @export
placeholder_value <- function(type_name, schema_data) {
  # 1. If the type has enumeration values, use the first valid code
  enums <- schema_data$enumerations |>
    dplyr::filter(type_name == !!type_name)

  if (nrow(enums) > 0L) {
    return(enums$value[[1]])
  }

  # 2. Derive a plausible value from well-known type names
  known <- known_placeholder(type_name)
  if (!is.na(known)) {
    return(known)
  }

  # 3. Fall back to base type inspection
  type_info <- schema_data$types |>
    dplyr::filter(type_name == !!type_name)

  if (nrow(type_info) == 0L) {
    return("PLACEHOLDER")
  }

  base <- type_info$base_type[[1]] %||% ""

  if (stringr::str_detect(base, "date")) {
    return(as.character(Sys.Date()))
  }
  if (stringr::str_detect(base, "time")) {
    return("09:00:00")
  }
  if (stringr::str_detect(base, "integer|decimal")) {
    return("1")
  }

  max_len <- as.integer(type_info$max_length[[1]])
  if (!is.na(max_len) && max_len > 0L) {
    return(strrep("X", min(max_len, 5L)))
  }

  len <- as.integer(type_info$length[[1]])
  if (!is.na(len) && len > 0L) {
    return(strrep("X", len))
  }

  "PLACEHOLDER"
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Return a hard-coded placeholder for well-known type names
#' @noRd
known_placeholder <- function(type_name) {
  dplyr::case_when(
    stringr::str_detect(type_name, "NHSNumber") ~ "9000000009",
    stringr::str_detect(type_name, "OrgCode") ~ "RJ1",
    stringr::str_detect(type_name, "ICD") ~ "Z000",
    stringr::str_detect(type_name, "OPCS") ~ "X998",
    stringr::str_detect(type_name, "GMP_Code") ~ "G1234567",
    stringr::str_detect(type_name, "ConsultantCode") ~ "C1234567",
    stringr::str_detect(type_name, "PracticeCode") ~ "A81001",
    stringr::str_detect(type_name, "Date") ~ as.character(Sys.Date()),
    stringr::str_detect(type_name, "Time") ~ "09:00:00",
    stringr::str_detect(type_name, "PositiveInteger") ~ "1",
    stringr::str_detect(type_name, "Integer") ~ "0",
    .default = NA_character_
  )
}

#' Find a node by xpath relative to root, creating missing intermediate nodes
#' @noRd
find_or_create_node <- function(root_node, target_xpath, root_xpath) {
  # Convert absolute xpath to relative path segments
  # e.g. "/RootType/Parent/Child" with root_xpath="/RootType" -> c("Parent","Child")
  relative <- stringr::str_remove(
    target_xpath,
    paste0("^", stringr::str_escape(root_xpath))
  )
  segments <- stringr::str_split(relative, "/")[[1]]
  segments <- segments[nchar(segments) > 0L]

  if (length(segments) == 0L) {
    return(root_node)
  }

  current <- root_node
  for (seg in segments) {
    child <- xml2::xml_find_first(current, glue::glue("./{seg}"))
    if (inherits(child, "xml_missing")) {
      current <- xml2::xml_add_child(current, seg)
    } else {
      current <- child
    }
  }
  current
}

#' Indent XML output for readability
#' @noRd
format_xml <- function(xml_string) {
  tryCatch(
    {
      doc <- xml2::read_xml(xml_string)
      lines <- walk_for_format(xml2::xml_root(doc), depth = 0L, lines = character(0))
      paste(c('<?xml version="1.0" encoding="UTF-8"?>', lines), collapse = "\n")
    },
    error = function(e) xml_string
  )
}

#' Recursively format XML nodes with indentation
#' @noRd
walk_for_format <- function(node, depth, lines) {
  indent <- strrep("  ", depth)
  name <- xml2::xml_name(node)
  children <- xml2::xml_children(node)
  text <- xml2::xml_text(node, trim = TRUE)

  if (length(children) == 0L) {
    # Leaf node
    if (nchar(text) > 0L) {
      lines <- c(lines, glue::glue("{indent}<{name}>{text}</{name}>"))
    } else {
      lines <- c(lines, glue::glue("{indent}<{name}/>"))
    }
  } else {
    lines <- c(lines, glue::glue("{indent}<{name}>"))
    for (child in children) {
      lines <- walk_for_format(child, depth + 1L, lines)
    }
    lines <- c(lines, glue::glue("{indent}</{name}>"))
  }
  lines
}
