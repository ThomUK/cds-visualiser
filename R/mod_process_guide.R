#' process_guide UI Function
#' @noRd
#' @importFrom shiny NS
#' @importFrom bslib navset_card_tab nav_panel layout_columns card card_header
#'   card_body accordion accordion_panel
#' @importFrom DiagrammeR DiagrammeROutput
mod_process_guide_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    id = ns("guide_tabs"),

    # ---- Tab 1: What is CDS ------------------------------------------------
    bslib::nav_panel(
      "What is CDS?",
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header("Commissioning Data Sets"),
          bslib::card_body(
            shiny::tags$p(
              "The ", shiny::tags$strong("Commissioning Data Sets (CDS)"),
              " are a standardised set of data definitions used across the NHS in England.
               Provider organisations (hospitals, mental health trusts, community providers)
               collect patient activity data and submit it to NHS England via the
               Secondary Uses Service (SUS+) using CDS XML messages."
            ),
            shiny::tags$p(
              "CDS data is the primary basis for:",
              shiny::tags$ul(
                shiny::tags$li("NHS payment (PbR / HRG tariffs)"),
                shiny::tags$li("Commissioning intelligence and planning"),
                shiny::tags$li("National statistics (HES — Hospital Episode Statistics)"),
                shiny::tags$li("Quality dashboards and performance indicators")
              )
            ),
            shiny::tags$p(
              "The schema used by this app is ", shiny::tags$strong("CDS v6.3.1"),
              " (released June 2022), which is the current live version for acute
               provider submissions."
            )
          )
        ),
        bslib::card(
          bslib::card_header("Key terms"),
          bslib::card_body(
            shiny::tags$dl(
              .guide_term(
                "Spell",
                "A continuous period of care under one provider. Starts on admission, ends on discharge."
              ),
              .guide_term(
                "Episode",
                "A period of care under one consultant. A spell contains one or more episodes."
              ),
              .guide_term(
                "HRG",
                "Healthcare Resource Group — a clinical category used to set the payment tariff."
              ),
              .guide_term(
                "SUS+",
                "Secondary Uses Service — the NHS England portal that receives CDS submissions."
              ),
              .guide_term(
                "SLAM",
                "Service Line Activity Monitoring — internal reporting often fed from CDS data."
              ),
              .guide_term(
                "CDS type",
                "Each XML message type covers a specific category of patient activity
                 (e.g. 130 = Finished General Episode, 020 = Outpatient)."
              )
            )
          )
        )
      )
    ),

    # ---- Tab 2: CDS types at an acute trust --------------------------------
    bslib::nav_panel(
      "CDS Types",
      bslib::card(
        bslib::card_header("CDS types relevant to an acute provider trust"),
        bslib::card_body(
          shiny::tags$p(
            "An acute trust typically generates the following CDS message types.
             Priority types (highest volume / payment impact) are highlighted."
          ),
          shiny::tags$table(
            class = "table table-bordered table-hover",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Code"),
                shiny::tags$th("Name"),
                shiny::tags$th("Description"),
                shiny::tags$th("Priority")
              )
            ),
            shiny::tags$tbody(
              .cds_type_row(
                "130", "Finished General Episode",
                "Elective and emergency inpatient admissions that have been discharged.
                 Largest volume CDS type; drives the majority of PbR income.", TRUE
              ),
              .cds_type_row(
                "020", "Outpatient",
                "Consultant-led outpatient clinic attendances. High volume; includes
                 first and follow-up appointments.", TRUE
              ),
              .cds_type_row(
                "120", "Finished Birth Episode",
                "Maternity — baby's birth episode (admitted patient care).", TRUE
              ),
              .cds_type_row(
                "140", "Finished Delivery Episode",
                "Maternity — mother's delivery episode.", TRUE
              ),
              .cds_type_row(
                "180", "Unfinished Birth Episode",
                "Birth episode where the baby is still an inpatient at month end.", FALSE
              ),
              .cds_type_row(
                "190", "Unfinished General Episode",
                "General episode where the patient is still admitted at month end.", FALSE
              ),
              .cds_type_row(
                "200", "Unfinished Delivery Episode",
                "Delivery episode where the mother is still an inpatient at month end.", FALSE
              ),
              .cds_type_row(
                "150", "Other Birth Event",
                "Maternity births that don't result in an admitted patient episode
                 (e.g. home births).", FALSE
              ),
              .cds_type_row(
                "160", "Other Delivery",
                "Maternity deliveries occurring outside an admitted episode.", FALSE
              )
            )
          )
        )
      )
    ),

    # ---- Tab 3: Submission flow --------------------------------------------
    bslib::nav_panel(
      "Submission Flow",
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("Monthly CDS submission process"),
          bslib::card_body(
            shiny::tags$div(
              style = "height:480px; overflow-y:auto",
              DiagrammeR::DiagrammeROutput(ns("flow_diagram"), height = "900px")
            )
          )
        ),
        bslib::card(
          bslib::card_header("Key dates"),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li(
                shiny::tags$strong("Month end:"),
                " Activity is frozen in PAS."
              ),
              shiny::tags$li(
                shiny::tags$strong("Day 1\u20135:"),
                " CDS extract generated and validated locally."
              ),
              shiny::tags$li(
                shiny::tags$strong("Day 5\u201311:"),
                " Submission window open; files uploaded to SUS+."
              ),
              shiny::tags$li(
                shiny::tags$strong("Reconciliation:"),
                " SUS+ feedback files checked; rejected records investigated."
              ),
              shiny::tags$li(
                shiny::tags$strong("SLAM / Tariff:"),
                " HRG grouping applied; payment calculated by commissioner."
              )
            ),
            shiny::tags$hr(),
            shiny::tags$p(
              shiny::tags$strong("File format:"),
              " CDS v6.3.1 XML, one ZIP per CDS type per submission batch.",
              style = "font-size:0.9em"
            ),
            shiny::tags$p(
              shiny::tags$strong("Mechanism:"),
              " MESH (Message Exchange for Social Care and Health) or direct SUS+ portal upload.",
              style = "font-size:0.9em"
            )
          )
        )
      )
    ),

    # ---- Tab 4: Common errors ----------------------------------------------
    bslib::nav_panel(
      "Common Errors",
      bslib::card(
        bslib::card_header("Frequent CDS data quality issues"),
        bslib::card_body(
          bslib::accordion(
            open = FALSE,
            .error_panel(
              "AttendanceStatus starts at code 2",
              "For outpatient (CDS 020), AttendanceStatus valid codes start at 2
               (attended), 3 (attended — cancelled by patient), 4 (did not attend),
               5 (cancelled by provider). There is no code 1. Systems that default
               to '1' will fail SUS+ validation."
            ),
            .error_panel(
              "PresentOnAdmissionIndicator — Y, N, or 8 only",
              "This field (used for HSMR/SHMI) accepts only 'Y' (present on
               admission), 'N' (not present on admission), or '8' (not applicable /
               not recorded). Sending any other value, including blank, causes
               rejection."
            ),
            .error_panel(
              "Spell dates vs episode dates",
              "Hospital Provider Spell Start Date covers the entire admission;
               Episode Start Date is when the consultant episode began. Common
               error: both set to the same value when a patient has had more than
               one consultant during a spell. The episode start must be \u2265 the
               spell start."
            ),
            .error_panel(
              "Multiple commissioners (GP and CCG/ICB)",
              "An episode may have both a GP Practice commissioner and an
               ICB/commissioner organisation. Both can appear in the same CDS
               record — omitting one where both are applicable causes incomplete
               payment attribution."
            ),
            .error_panel(
              "ICD-10 code format",
              "Diagnosis codes must be submitted without the decimal point and
               with no trailing spaces: 'J189' not 'J18.9'. Codes must also be
               valid for the submitted activity date using the correct ICD-10
               edition in force at that time."
            ),
            .error_panel(
              "Duplicate records across months",
              "Unfinished episodes (CDS 180\u2013200) submitted in earlier months
               must also be re-submitted in the month they finish, as a finished
               episode (CDS 120\u2013140). Failure to include the finished record
               leaves SUS+ with an open episode that never closes."
            )
          )
        )
      )
    )
  )
}

#' process_guide Server Functions
#' @noRd
#' @importFrom shiny moduleServer
#' @importFrom DiagrammeR renderDiagrammeR mermaid
mod_process_guide_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$flow_diagram <- DiagrammeR::renderDiagrammeR(
      DiagrammeR::mermaid("
        graph TD
          A[Patient activity<br/>recorded in PAS] --> B[Clinical coding<br/>ICD-10 / OPCS-4]
          B --> C[CDS extract<br/>from PAS / middleware]
          C --> D{Local<br/>validation}
          D -- Pass --> E[CDS XML generated<br/>& zipped]
          D -- Fail --> F[Error correction<br/>& resubmit]
          F --> D
          E --> G[Upload to SUS+<br/>via MESH / portal]
          G --> H{SUS+<br/>processing}
          H -- Rejected --> I[Rejection report<br/>investigated]
          I --> F
          H -- Accepted --> J[HRG grouping<br/>applied]
          J --> K[Commissioner<br/>payment / tariff]
          J --> L[HES / national<br/>statistics]

        style A fill:#005EB8,color:#fff
        style K fill:#007f3b,color:#fff
        style L fill:#007f3b,color:#fff
        style D fill:#ffb81c,color:#000
        style H fill:#ffb81c,color:#000
      ")
    )
  })
}

# ---------------------------------------------------------------------------
# UI helper functions
# ---------------------------------------------------------------------------

.guide_term <- function(term, definition) {
  shiny::tagList(
    shiny::tags$dt(term),
    shiny::tags$dd(definition, style = "font-size:0.9em; color:#555")
  )
}

.cds_type_row <- function(code, name, description, priority) {
  shiny::tags$tr(
    class = if (priority) "table-primary" else "",
    shiny::tags$td(shiny::tags$strong(code)),
    shiny::tags$td(name),
    shiny::tags$td(description, style = "font-size:0.9em"),
    shiny::tags$td(
      if (priority) {
        shiny::tags$span("High", class = "badge bg-primary")
      } else {
        shiny::tags$span("Standard", class = "badge bg-secondary")
      }
    )
  )
}

.error_panel <- function(title, body) {
  bslib::accordion_panel(
    title,
    shiny::tags$p(body, style = "font-size:0.9em")
  )
}
