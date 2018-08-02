tp_util_corr_viewer <- tabPanel(
  "Correlation viewer",
  fluidRow(
    column(
      width = tp_wid_hlf,
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictors")),
      selectInput("ucv_upload", NULL, choices = ListTblsFromSSviaCS(proj_db_name), selected = db_predictors_src, 
                  selectize = TRUE)
    ),
    column(
      width = 12 - tp_wid_hlf,
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "fields")),
      uiOutput("ucv_features")
    )
  ),
  fluidRow(
    column(
      width = tp_wid_hlf,
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Pearson Correlation Matrix")),
      DT::dataTableOutput("corr_mtx")
    ),
    column(
      width = 12 - tp_wid_hlf,
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Pearson Correlation Matrix Plot")),
      plotOutput("corr_plot")
    )
  ),
  fluidRow(
    column(
      width = tp_wid_hlf,
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "GK Correlation Matrix")),
      DT::dataTableOutput("gk_corr_mtx")
    ),
    column(
      width = 12 - tp_wid_hlf,
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "GK Correlation Matrix Plot")),
      plotOutput("gk_corr_plot")
    )
  )
)