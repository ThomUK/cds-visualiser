# XSD namespace prefix used throughout the CDS schemas
XS_NS <- c(xs = "http://www.w3.org/2001/XMLSchema")

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Parse all CDS XSD files into a structured list
#'
#' @param xsd_dir Path to the directory containing the CDS XSD files.
#' @return A named list with components: elements, types, enumerations,
#'   complex_types, tree_data.
#' @export
parse_cds_schema <- function(xsd_dir) {
  xsd_dir <- normalizePath(xsd_dir, mustWork = TRUE)

  # --- Read all XSD documents ---
  xsd_files <- list(
    elements   = xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd")),
    structures = xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Structures-V6-3-1.xsd")),
    headers    = xml2::read_xml(file.path(xsd_dir, "CDS_000_Message_Headers_And_Trailers-V6-3-1.xsd")),
    cds_020    = xml2::read_xml(file.path(xsd_dir, "CDS_020_Outpatient-V6-3-1.xsd")),
    cds_120    = xml2::read_xml(file.path(xsd_dir, "CDS_120_AdmittedPatientCare_Finished_BirthEpisode-V6-3-1.xsd")),
    cds_130    = xml2::read_xml(file.path(xsd_dir, "CDS_130_AdmittedPatientCare_Finished_GeneralEpisode-V6-3-1.xsd")),
    cds_140    = xml2::read_xml(file.path(xsd_dir, "CDS_140_AdmittedPatientCare_Finished_DeliveryEpisode-V6-3-1.xsd")),
    cds_150    = xml2::read_xml(file.path(xsd_dir, "CDS_150_AdmittedPatientCare_OtherBirthEvent-V6-3-1.xsd")),
    cds_160    = xml2::read_xml(file.path(xsd_dir, "CDS_160_AdmittedPatientCare_OtherDelivery-V6-3-1.xsd")),
    cds_180    = xml2::read_xml(file.path(xsd_dir, "CDS_180_AdmittedPatientCare_Unfinished_BirthEpisode-V6-3-1.xsd")),
    cds_190    = xml2::read_xml(file.path(xsd_dir, "CDS_190_AdmittedPatientCare_Unfinished_GeneralEpisode-V6-3-1.xsd")),
    cds_200    = xml2::read_xml(file.path(xsd_dir, "CDS_200_AdmittedPatientCare_Unfinished_DeliveryEpisode-V6-3-1.xsd"))
  )

  # --- Extract type definitions ---
  types <- extract_simple_types(xsd_files$elements)
  enumerations <- extract_enumerations(xsd_files$elements)
  complex_types <- extract_complex_types(unname(xsd_files))

  # --- Walk each CDS-type XSD to build element paths ---
  cds_type_map <- list(
    "020" = xsd_files$cds_020,
    "120" = xsd_files$cds_120,
    "130" = xsd_files$cds_130,
    "140" = xsd_files$cds_140,
    "150" = xsd_files$cds_150,
    "160" = xsd_files$cds_160,
    "180" = xsd_files$cds_180,
    "190" = xsd_files$cds_190,
    "200" = xsd_files$cds_200
  )

  elements <- build_elements_tibble(cds_type_map, complex_types)
  tree_data <- build_tree_data(elements)

  list(
    elements      = elements,
    types         = types,
    enumerations  = enumerations,
    complex_types = complex_types,
    tree_data     = tree_data
  )
}

# ---------------------------------------------------------------------------
# extract_simple_types()
# ---------------------------------------------------------------------------

#' Extract all xs:simpleType definitions from an XSD document
#' @param doc An xml2 document object
#' @return A tibble with columns: type_name, base_type, pattern, min_length,
#'   max_length, length, min_inclusive, max_inclusive, total_digits,
#'   fraction_digits, annotation
#' @noRd
extract_simple_types <- function(doc) {
  nodes <- xml2::xml_find_all(doc, ".//xs:simpleType[@name]", XS_NS)

  purrr::map_dfr(nodes, function(node) {
    type_name <- xml2::xml_attr(node, "name")
    restriction <- xml2::xml_find_first(node, ".//xs:restriction", XS_NS)

    if (inherits(restriction, "xml_missing")) {
      base_type <- NA_character_
    } else {
      base_type <- xml2::xml_attr(restriction, "base") %||% NA_character_
    }

    get_facet <- function(facet_name) {
      if (inherits(restriction, "xml_missing")) {
        return(NA_character_)
      }
      n <- xml2::xml_find_first(restriction, paste0("xs:", facet_name), XS_NS)
      if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_attr(n, "value")
    }

    tibble::tibble(
      type_name       = type_name,
      base_type       = base_type,
      pattern         = get_facet("pattern"),
      min_length      = get_facet("minLength"),
      max_length      = get_facet("maxLength"),
      length          = get_facet("length"),
      min_inclusive   = get_facet("minInclusive"),
      max_inclusive   = get_facet("maxInclusive"),
      total_digits    = get_facet("totalDigits"),
      fraction_digits = get_facet("fractionDigits"),
      annotation      = extract_annotation(node)
    )
  })
}

# ---------------------------------------------------------------------------
# extract_enumerations()
# ---------------------------------------------------------------------------

#' Extract all xs:enumeration values from an XSD document
#' @param doc An xml2 document object
#' @return A tibble with columns: type_name, value, annotation
#' @noRd
extract_enumerations <- function(doc) {
  nodes <- xml2::xml_find_all(doc, ".//xs:simpleType[@name]", XS_NS)

  purrr::map_dfr(nodes, function(node) {
    type_name <- xml2::xml_attr(node, "name")
    enum_nodes <- xml2::xml_find_all(node, ".//xs:enumeration", XS_NS)
    if (length(enum_nodes) == 0L) {
      return(NULL)
    }

    tibble::tibble(
      type_name  = type_name,
      value      = xml2::xml_attr(enum_nodes, "value"),
      annotation = purrr::map_chr(enum_nodes, extract_annotation)
    )
  })
}

# ---------------------------------------------------------------------------
# extract_complex_types()
# ---------------------------------------------------------------------------

#' Build a named lookup list of all xs:complexType nodes across XSD documents
#' @param docs A list of xml2 document objects
#' @return A named list: type_name -> xml2 node
#' @noRd
extract_complex_types <- function(docs) {
  all_entries <- purrr::map(docs, function(doc) {
    nodes <- xml2::xml_find_all(doc, ".//xs:complexType[@name]", XS_NS)
    type_names <- xml2::xml_attr(nodes, "name")
    purrr::set_names(as.list(nodes), type_names)
  })

  # Flatten; later entries override earlier on name collision (safe for this schema)
  purrr::reduce(all_entries, function(acc, x) c(acc, x))
}

# ---------------------------------------------------------------------------
# build_elements_tibble()
# ---------------------------------------------------------------------------

#' Walk each CDS-type XSD and produce a flat elements tibble
#' @param cds_type_map Named list: CDS code -> xml2 doc
#' @param complex_types Named list from extract_complex_types()
#' @return A tibble with element definitions
#' @noRd
build_elements_tibble <- function(cds_type_map, complex_types) {
  rows_by_cds <- purrr::imap(cds_type_map, function(doc, cds_code) {
    # The root complexType of each CDS-type XSD defines the message structure
    root_node <- xml2::xml_find_first(doc, ".//xs:complexType[@name]", XS_NS)
    if (inherits(root_node, "xml_missing")) {
      return(NULL)
    }

    root_name <- xml2::xml_attr(root_node, "name")

    walk_complex_type(
      type_name     = root_name,
      xpath_prefix  = glue::glue("/{root_name}"),
      cds_code      = cds_code,
      complex_types = complex_types,
      visited       = character(0),
      depth         = 0L
    )
  })

  all_rows <- dplyr::bind_rows(rows_by_cds)
  if (nrow(all_rows) == 0L) {
    return(all_rows)
  }

  # Merge duplicate elements (same xpath) that appear in multiple CDS types
  all_rows |>
    dplyr::group_by(xpath) |>
    dplyr::summarise(
      element_name = dplyr::first(element_name),
      type_name    = dplyr::first(type_name),
      min_occurs   = dplyr::first(min_occurs),
      max_occurs   = dplyr::first(max_occurs),
      is_required  = dplyr::first(is_required),
      in_choice    = dplyr::coalesce(dplyr::first(in_choice), FALSE),
      parent_xpath = dplyr::first(parent_xpath),
      annotation   = dplyr::first(annotation),
      cds_types    = list(unique(cds_code)),
      .groups      = "drop"
    )
}

# ---------------------------------------------------------------------------
# walk_complex_type() — recursive tree walker
# ---------------------------------------------------------------------------

#' Recursively walk a named complexType and collect all descendant elements
#' @noRd
walk_complex_type <- function(type_name, xpath_prefix, cds_code,
                              complex_types, visited, depth,
                              in_choice = FALSE) {
  # Cycle guard and depth limit
  if (depth > 25L || type_name %in% visited) {
    return(NULL)
  }
  visited <- c(visited, type_name)

  type_node <- complex_types[[type_name]]
  if (is.null(type_node)) {
    return(NULL)
  }

  walk_element_list(
    parent_node   = type_node,
    xpath_prefix  = xpath_prefix,
    cds_code      = cds_code,
    complex_types = complex_types,
    visited       = visited,
    depth         = depth,
    in_choice     = in_choice
  )
}

#' Walk xs:sequence and xs:choice children of a node, returning element rows
#'
#' Processes sequence elements (in_choice=FALSE) and choice elements
#' (in_choice=TRUE) separately to avoid xml2 node identity comparison issues.
#' @noRd
walk_element_list <- function(parent_node, xpath_prefix, cds_code,
                              complex_types, visited, depth,
                              in_choice = FALSE) {
  # Sequence elements (not inside a nested xs:choice)
  seq_elements <- xml2::xml_find_all(
    parent_node,
    paste0(
      "xs:sequence/xs:element",
      " | xs:complexContent/xs:extension/xs:sequence/xs:element"
    ),
    XS_NS
  )

  # Choice elements (the element IS inside a xs:choice)
  choice_elements <- xml2::xml_find_all(
    parent_node,
    "xs:sequence/xs:choice/xs:element | xs:choice/xs:element",
    XS_NS
  )

  if (length(seq_elements) == 0L && length(choice_elements) == 0L) {
    return(NULL)
  }

  process_elements <- function(els, this_in_choice) {
    if (length(els) == 0L) {
      return(NULL)
    }
    purrr::map_dfr(els, function(el) {
      el_name <- xml2::xml_attr(el, "name")
      if (is.na(el_name)) {
        return(NULL)
      } # skip ref= elements

      el_type <- xml2::xml_attr(el, "type")
      # xml_attr returns NA (not NULL) for absent attributes; use coalesce not %||%
      min_occ <- dplyr::coalesce(xml2::xml_attr(el, "minOccurs"), "1")
      max_occ <- dplyr::coalesce(xml2::xml_attr(el, "maxOccurs"), "1")
      this_xpath <- glue::glue("{xpath_prefix}/{el_name}")

      # Strip namespace prefix (ns:TypeName -> TypeName)
      el_type_clean <- if (!is.na(el_type)) {
        stringr::str_remove(el_type, "^[^:]+:")
      } else {
        NA_character_
      }

      # Inherit parent's in_choice status or use the local flag
      eff_in_choice <- isTRUE(in_choice) || isTRUE(this_in_choice)

      this_row <- tibble::tibble(
        element_name = el_name,
        type_name    = el_type_clean,
        min_occurs   = min_occ,
        max_occurs   = max_occ,
        xpath        = this_xpath,
        cds_code     = cds_code,
        is_required  = (min_occ != "0"),
        in_choice    = eff_in_choice,
        parent_xpath = xpath_prefix,
        annotation   = extract_annotation(el)
      )

      # Recurse into named complexType
      children <- if (!is.na(el_type_clean) &&
        el_type_clean %in% names(complex_types)) {
        walk_complex_type(
          type_name     = el_type_clean,
          xpath_prefix  = this_xpath,
          cds_code      = cds_code,
          complex_types = complex_types,
          visited       = visited,
          depth         = depth + 1L,
          in_choice     = eff_in_choice
        )
      } else if (is.na(el_type)) {
        # Inline anonymous complexType
        inline_ct <- xml2::xml_find_first(el, ".//xs:complexType", XS_NS)
        if (!inherits(inline_ct, "xml_missing")) {
          walk_element_list(
            parent_node   = inline_ct,
            xpath_prefix  = this_xpath,
            cds_code      = cds_code,
            complex_types = complex_types,
            visited       = visited,
            depth         = depth + 1L,
            in_choice     = eff_in_choice
          )
        } else {
          NULL
        }
      } else {
        NULL
      }

      dplyr::bind_rows(this_row, children)
    })
  }

  dplyr::bind_rows(
    process_elements(seq_elements, this_in_choice = FALSE),
    process_elements(choice_elements, this_in_choice = TRUE)
  )
}

# ---------------------------------------------------------------------------
# build_tree_data()
# ---------------------------------------------------------------------------

#' Build a jsTreeR-compatible nested list from the elements tibble
#' @param elements A tibble as produced by build_elements_tibble()
#' @return A list of lists (one per CDS type) suitable for jsTreeR::jstree()
#' @noRd
build_tree_data <- function(elements) {
  all_cds <- sort(unique(unlist(elements$cds_types)))

  purrr::map(all_cds, function(cds_code) {
    cds_els <- elements |>
      dplyr::filter(purrr::map_lgl(cds_types, ~ cds_code %in% .x)) |>
      dplyr::arrange(xpath)

    list(
      text     = cds_label(cds_code),
      id       = paste0("cds_", cds_code),
      state    = list(opened = FALSE),
      children = build_subtree(cds_els, parent = NA_character_)
    )
  })
}

#' Recursively build subtree nodes for jsTreeR
#' @noRd
build_subtree <- function(elements, parent) {
  if (is.na(parent)) {
    # Root level: elements whose parent is the CDS root type (one slash in xpath)
    children_rows <- elements |>
      dplyr::filter(stringr::str_count(xpath, "/") == 2L)
  } else {
    children_rows <- elements |>
      dplyr::filter(parent_xpath == parent)
  }

  if (nrow(children_rows) == 0L) {
    return(list())
  }

  purrr::pmap(
    list(
      children_rows$element_name,
      children_rows$xpath,
      children_rows$type_name,
      children_rows$is_required,
      children_rows$in_choice
    ),
    function(element_name, xpath, type_name, is_required, in_choice) {
      icon <- if (is_required && !in_choice) {
        "fas fa-asterisk text-danger"
      } else if (in_choice) {
        "fas fa-code-branch text-warning"
      } else {
        "fas fa-circle text-muted"
      }

      list(
        text = element_name,
        id = stringr::str_replace_all(xpath, "[^A-Za-z0-9_-]", "_"),
        icon = icon,
        data = list(
          xpath     = xpath,
          type      = type_name %||% "",
          required  = is_required,
          in_choice = in_choice
        ),
        children = build_subtree(elements, parent = xpath)
      )
    }
  )
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Extract xs:annotation/xs:documentation text from an XML node
#' @noRd
extract_annotation <- function(node) {
  doc_node <- xml2::xml_find_first(node, ".//xs:annotation/xs:documentation", XS_NS)
  if (inherits(doc_node, "xml_missing")) {
    return(NA_character_)
  }
  stringr::str_squish(xml2::xml_text(doc_node))
}

#' Human-readable label for a CDS type code
#' @noRd
cds_label <- function(code) {
  labels <- c(
    "020" = "020 \u2014 Outpatient",
    "120" = "120 \u2014 APC Finished Birth Episode",
    "130" = "130 \u2014 APC Finished General Episode",
    "140" = "140 \u2014 APC Finished Delivery Episode",
    "150" = "150 \u2014 APC Other Birth Event",
    "160" = "160 \u2014 APC Other Delivery",
    "180" = "180 \u2014 APC Unfinished Birth Episode",
    "190" = "190 \u2014 APC Unfinished General Episode",
    "200" = "200 \u2014 APC Unfinished Delivery Episode"
  )
  labels[[code]] %||% paste0("CDS ", code)
}
