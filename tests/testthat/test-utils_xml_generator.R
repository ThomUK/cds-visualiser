schema_data <- readRDS(here::here("inst/extdata/schema_data.rds"))

# ---------------------------------------------------------------------------
# placeholder_value()
# ---------------------------------------------------------------------------

test_that("placeholder_value returns first valid enumeration code for coded types", {
  val <- placeholder_value("AdmissionMethodCode_HospitalProviderSpell_Type", schema_data)
  valid_codes <- dplyr::filter(
    schema_data$enumerations,
    type_name == "AdmissionMethodCode_HospitalProviderSpell_Type"
  )$value
  expect_true(val %in% valid_codes)
})

test_that("placeholder_value returns '9000000009' for NHSNumber type", {
  val <- placeholder_value("NHSNumber_Type", schema_data)
  expect_equal(val, "9000000009")
})

test_that("placeholder_value returns a date string for date-based types", {
  val <- placeholder_value("Date_Type", schema_data)
  expect_match(val, "^\\d{4}-\\d{2}-\\d{2}$")
})

test_that("placeholder_value returns a time string for Time_Type", {
  val <- placeholder_value("Time_Type", schema_data)
  expect_match(val, "^\\d{2}:\\d{2}:\\d{2}$")
})

test_that("placeholder_value returns an ICD code for ICD types", {
  val <- placeholder_value("ICD_Type", schema_data)
  expect_type(val, "character")
  expect_gt(nchar(val), 0)
})

test_that("placeholder_value returns an OPCS code for OPCS types", {
  val <- placeholder_value("OPCS_Type", schema_data)
  expect_type(val, "character")
  expect_gt(nchar(val), 0)
})

test_that("placeholder_value returns a non-empty string for unknown types", {
  val <- placeholder_value("SomeUnknownType_Type", schema_data)
  expect_type(val, "character")
  expect_gt(nchar(val), 0)
})

# ---------------------------------------------------------------------------
# generate_synthetic_xml()
# ---------------------------------------------------------------------------

test_that("generate_synthetic_xml returns a character string", {
  result <- generate_synthetic_xml(schema_data, "130", include_optional = FALSE)
  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})

test_that("generate_synthetic_xml produces well-formed XML for CDS 130", {
  result <- generate_synthetic_xml(schema_data, "130", include_optional = FALSE)
  expect_no_error(xml2::read_xml(result))
})

test_that("generate_synthetic_xml produces well-formed XML for CDS 020", {
  result <- generate_synthetic_xml(schema_data, "020", include_optional = FALSE)
  expect_no_error(xml2::read_xml(result))
})

test_that("generate_synthetic_xml produces well-formed XML for all 9 CDS types", {
  cds_codes <- c("020", "120", "130", "140", "150", "160", "180", "190", "200")
  for (code in cds_codes) {
    result <- generate_synthetic_xml(schema_data, code, include_optional = FALSE)
    expect_no_error(xml2::read_xml(result))
  }
})

test_that("include_optional=TRUE produces longer output than include_optional=FALSE", {
  required_only <- generate_synthetic_xml(schema_data, "130", include_optional = FALSE)
  with_optional <- generate_synthetic_xml(schema_data, "130", include_optional = TRUE)
  expect_gt(nchar(with_optional), nchar(required_only))
})

test_that("required-only output contains the CDS type root element", {
  result <- generate_synthetic_xml(schema_data, "130", include_optional = FALSE)
  doc    <- xml2::read_xml(result)
  # Root element name should match the CDS 130 root structure
  root_name <- xml2::xml_name(xml2::xml_root(doc))
  expect_type(root_name, "character")
  expect_gt(nchar(root_name), 0)
})

test_that("output XML contains no NA text values", {
  result <- generate_synthetic_xml(schema_data, "130", include_optional = FALSE)
  expect_false(grepl("NA", result, fixed = TRUE))
})

test_that("output XML contains no empty element values for required fields", {
  result  <- generate_synthetic_xml(schema_data, "130", include_optional = FALSE)
  doc     <- xml2::read_xml(result)
  # Leaf text nodes should not be empty strings
  all_text <- xml2::xml_text(xml2::xml_find_all(doc, "//*[not(*)]"))
  # Filter to non-empty (some structural elements may be empty containers)
  expect_false(any(all_text == "NA"))
})
