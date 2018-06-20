##
# Data upload tabPanel
##
tp_data_upload <- tabPanel(
  "Upload",
  fluidRow(
    column(
      tags$h5("DATASET"),
      width = 12,
      selectInput("dsu_upload", NULL, choices = ListTblsFromADB(db_path), selected = db_predictors_src, selectize = TRUE)
    )
  ),
  fluidRow(
    column(
      tags$h5("PREDICTORS"),
      width = tp_wid_wde,
      DT::dataTableOutput("dsu_dts")
    )
  ),
  fluidRow(
    column(
      tags$h5("SPECS"),
      width = tp_wid_hlf,
      DT::dataTableOutput("dsu_ovw")
    ),
    column(
      tags$h5("TARGET"),
      width = tp_wid_nar,
      DT::dataTableOutput("dsu_lbs")
    )
  )
)