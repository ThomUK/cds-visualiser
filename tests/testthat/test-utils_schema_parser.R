xsd_dir <- here::here("inst/extdata/xsd")

# Helper: only run schema-dependent tests if XSD files are present
skip_if_no_xsd <- function() {
  skip_if_not(
    dir.exists(xsd_dir),
    "XSD directory not found — skipping schema parser tests"
  )
}

# ---------------------------------------------------------------------------
# extract_simple_types()
# ---------------------------------------------------------------------------

test_that("extract_simple_types returns >= 299 rows", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  types <- extract_simple_types(doc)

  expect_s3_class(types, "data.frame")
  expect_gte(nrow(types), 299)
  expect_true(all(c(
    "type_name", "base_type", "pattern", "min_length",
    "max_length", "annotation"
  ) %in% names(types)))
})

test_that("extract_simple_types captures constraints correctly", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  types <- extract_simple_types(doc)

  # Date_Type uses minInclusive/maxInclusive (not pattern)
  date_type <- dplyr::filter(types, type_name == "Date_Type")
  expect_equal(nrow(date_type), 1)
  expect_false(is.na(date_type$min_inclusive))
  expect_equal(date_type$min_inclusive, "1900-01-01")

  # NHSNumber_Type has a pattern constraint
  nhs_type <- dplyr::filter(types, type_name == "NHSNumber_Type")
  expect_equal(nrow(nhs_type), 1)
  expect_false(is.na(nhs_type$pattern))
})

# ---------------------------------------------------------------------------
# extract_enumerations()
# ---------------------------------------------------------------------------

test_that("AttendedOrDidNotAttendCode_Type does NOT include code '1'", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  enums <- extract_enumerations(doc)

  att <- dplyr::filter(enums, type_name == "AttendedOrDidNotAttendCode_Type")
  expect_gt(nrow(att), 0)
  expect_false("1" %in% att$value)
})

test_that("PresentOnAdmissionIndicator types have exactly Y, N, 8", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  enums <- extract_enumerations(doc)

  poi <- dplyr::filter(enums, stringr::str_detect(type_name, "PresentOnAdmission"))
  expect_gt(nrow(poi), 0)
  expect_true(all(c("Y", "N", "8") %in% poi$value))
  # Must NOT contain numeric 0 or 1 (common error expectation)
  expect_false("0" %in% poi$value)
  expect_false("1" %in% poi$value)
})

test_that("PersonGenderCode_Type has correct CDS v6.3.1 values (1, 2, 9, X)", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  enums <- extract_enumerations(doc)

  # CDS v6.3.1 uses legacy gender codes: 1=Male, 2=Female, 9=Not specified, X=Not known
  # (PersonGenderCodeCurrent_Type derives from PersonGenderCode_Type which holds the enums)
  pgc <- dplyr::filter(enums, type_name == "PersonGenderCode_Type")
  expect_gt(nrow(pgc), 0)
  expect_true("1" %in% pgc$value)
  expect_true("2" %in% pgc$value)
  expect_true("9" %in% pgc$value)
  expect_true("X" %in% pgc$value)
})

test_that("AdmissionMethodCode has exactly 19 values", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  enums <- extract_enumerations(doc)

  amc <- dplyr::filter(enums, type_name == "AdmissionMethodCode_HospitalProviderSpell_Type")
  expect_equal(nrow(amc), 19)
})

test_that("DischargeDestinationCode has 22 values", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  enums <- extract_enumerations(doc)

  ddc <- dplyr::filter(enums, type_name == "DischargeDestinationCode_HospitalProviderSpell_Type")
  expect_equal(nrow(ddc), 22)
  # Must include code 19 (usual residence) and 87 (hospital at home)
  expect_true("19" %in% ddc$value)
  expect_true("87" %in% ddc$value)
})

test_that("extract_enumerations returns type_name, value, annotation columns", {
  skip_if_no_xsd()
  doc <- xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd"))
  enums <- extract_enumerations(doc)

  expect_s3_class(enums, "data.frame")
  expect_true(all(c("type_name", "value", "annotation") %in% names(enums)))
  expect_gt(nrow(enums), 100)
})

# ---------------------------------------------------------------------------
# extract_complex_types()
# ---------------------------------------------------------------------------

test_that("extract_complex_types returns named list with >= 97 entries", {
  skip_if_no_xsd()
  xsd_files <- list(
    xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd")),
    xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Structures-V6-3-1.xsd")),
    xml2::read_xml(file.path(xsd_dir, "CDS_000_Message_Headers_And_Trailers-V6-3-1.xsd"))
  )
  ct <- extract_complex_types(xsd_files)

  expect_type(ct, "list")
  expect_gte(length(ct), 97)
  expect_false(is.null(names(ct)))
})

test_that("FinishedGeneralEpisode_Structure is present in complex types", {
  skip_if_no_xsd()
  xsd_files <- list(
    xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Elements-V6-3-1.xsd")),
    xml2::read_xml(file.path(xsd_dir, "CDS-XML_Standard_Data_Structures-V6-3-1.xsd"))
  )
  ct <- extract_complex_types(xsd_files)

  expect_true("FinishedGeneralEpisode_Structure" %in% names(ct))
})

# ---------------------------------------------------------------------------
# build_elements_tibble() via parse_cds_schema()
# ---------------------------------------------------------------------------

# Cache parsed schema for reuse across tests (expensive to re-parse)
local_schema <- local({
  if (!dir.exists(xsd_dir)) {
    return(NULL)
  }
  parse_cds_schema(xsd_dir)
})

test_that("parse_cds_schema returns all expected list components", {
  skip_if_no_xsd()
  expect_type(local_schema, "list")
  expect_true(all(c(
    "elements", "types", "enumerations",
    "complex_types", "tree_data"
  ) %in% names(local_schema)))
})

test_that("CDS 130 has > 30 elements", {
  skip_if_no_xsd()
  cds130 <- dplyr::filter(
    local_schema$elements,
    purrr::map_lgl(cds_types, ~ "130" %in% .x)
  )
  expect_gt(nrow(cds130), 30)
})

test_that("CDS 020 has > 20 elements", {
  skip_if_no_xsd()
  cds020 <- dplyr::filter(
    local_schema$elements,
    purrr::map_lgl(cds_types, ~ "020" %in% .x)
  )
  expect_gt(nrow(cds020), 20)
})

test_that("elements tibble has required columns", {
  skip_if_no_xsd()
  required_cols <- c(
    "element_name", "type_name", "min_occurs", "max_occurs",
    "xpath", "cds_types", "is_required", "parent_xpath"
  )
  expect_true(all(required_cols %in% names(local_schema$elements)))
})

test_that("is_required is FALSE when minOccurs is 0", {
  skip_if_no_xsd()
  optional_els <- dplyr::filter(local_schema$elements, min_occurs == "0")
  expect_gt(nrow(optional_els), 0)
  expect_true(all(!optional_els$is_required))
})

test_that("is_required is TRUE when minOccurs is 1", {
  skip_if_no_xsd()
  required_els <- dplyr::filter(local_schema$elements, min_occurs == "1")
  expect_gt(nrow(required_els), 0)
  expect_true(all(required_els$is_required))
})

test_that("cds_types list-column contains character vectors", {
  skip_if_no_xsd()
  expect_type(local_schema$elements$cds_types, "list")
  first <- local_schema$elements$cds_types[[1]]
  expect_type(first, "character")
})

test_that("all 9 CDS types are represented in elements", {
  skip_if_no_xsd()
  all_codes <- unique(unlist(local_schema$elements$cds_types))
  expected <- c("020", "120", "130", "140", "150", "160", "180", "190", "200")
  expect_true(all(expected %in% all_codes))
})

test_that("no element has an NA xpath", {
  skip_if_no_xsd()
  expect_false(any(is.na(local_schema$elements$xpath)))
})

# ---------------------------------------------------------------------------
# build_tree_data()
# ---------------------------------------------------------------------------

test_that("tree_data has one top-level node per CDS type", {
  skip_if_no_xsd()
  tree <- local_schema$tree_data
  expect_type(tree, "list")
  expect_equal(length(tree), 9)
})

test_that("each top-level tree node has a non-empty children list", {
  skip_if_no_xsd()
  tree <- local_schema$tree_data
  for (node in tree) {
    expect_true("children" %in% names(node))
    expect_gt(length(node$children), 0)
  }
})

test_that("tree node text matches CDS label format", {
  skip_if_no_xsd()
  tree <- local_schema$tree_data
  labels <- purrr::map_chr(tree, "text")
  expect_true(any(stringr::str_detect(labels, "130")))
  expect_true(any(stringr::str_detect(labels, "020")))
})
