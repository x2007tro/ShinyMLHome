##
# Data upload tabPanel
##
tp_data_upload <- tabPanel(
  "Upload",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Source data")),
      width = tp_wid_nar,
      selectInput("dsu_upload", NULL, choices = ListTblsFromADB(db_path), selected = db_predictors_src, selectize = TRUE)
    )
  ),
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictors")),
      width = tp_wid_wde,
      DT::dataTableOutput("dsu_dts")
    )
  ),
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictor Specs")),
      width = tp_wid_hlf,
      DT::dataTableOutput("dsu_ovw")
    ),
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Target")),
      width = tp_wid_nar,
      DT::dataTableOutput("dsu_lbs")
    )
  )
)