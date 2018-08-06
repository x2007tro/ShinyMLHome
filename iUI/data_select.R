tp_data_select <- tabPanel(
  "Selection",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Features")),
      width = tp_wid_hlf,
      uiOutput("dss_features")
    ),
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Save")),
      width = tp_wid_hlf,
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        textInput("dss_nname", label = NULL, 
                  value = "ml_predictors**", 
                  width = file_dir_field_width)
      ),
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        actionButton("dss_save", "Save", width = blotter_field_default_width),
        textOutput("dss_save_message")
      )
    )   
  ),
  br(),
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictors")),
      width = 12,
      DT::dataTableOutput("dss_dts")
    )
  ),
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictor Specs")),
      width = tp_wid_wde + 2,
      DT::dataTableOutput("dss_ovw")
    ),
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Target")),
      width = tp_wid_nar,
      DT::dataTableOutput("dss_lbs")
    )
  )
)