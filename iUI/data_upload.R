##
# Data upload tabPanel
##
tp_data_upload <- tabPanel(
  "Upload",
  fluidRow(
    column(
      tags$h5("DATASET"),
      width = 12,
      fileInput(
        "dsu_upload", 
        NULL,
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      )
    )
  ),
  fluidRow(
    column(
      tags$h5("OVERVIEW"),
      width = 3,
      DT::dataTableOutput("dsu_ovw")
    ),
    column(
      tags$h5("DATASET"),
      width = 7,
      DT::dataTableOutput("dsu_dts")
    ),
    column(
      tags$h5("LABELS"),
      width = 2,
      DT::dataTableOutput("dsu_lbs")
    )
  )
)