tp_util_corr_viewer <- tabPanel(
  "Correlation viewer",
  fluidRow(
    column(
      width = tp_wid_hlf,
      tags$h5("DATASET"),
      fileInput(
        "ucv_upload", 
        NULL,
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      )
    ),
    column(
      width = 12 - tp_wid_hlf,
      tags$h5("FIELDS"),
      uiOutput("ucv_features")
    )
  ),
  fluidRow(
    column(
      width = tp_wid_hlf,
      tags$h5("PEARSON CORR MATRIX"),
      DT::dataTableOutput("corr_mtx")
    ),
    column(
      width = 12 - tp_wid_hlf,
      tags$h5("PEARSON CORR MATRIX PLOT"),
      plotOutput("corr_plot")
    )
  ),
  fluidRow(
    column(
      width = tp_wid_hlf,
      tags$h5("GK CORR MATRIX"),
      DT::dataTableOutput("gk_corr_mtx")
    ),
    column(
      width = 12 - tp_wid_hlf,
      tags$h5("GK CORR MATRIX PLOT"),
      plotOutput("gk_corr_plot")
    )
  )
)