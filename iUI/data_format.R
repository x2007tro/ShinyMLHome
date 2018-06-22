tp_data_format <- tabPanel(
  "Format",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Features")),
      width = tp_wid_nar,
      selectInput("dsf_format_choice", label = "Options", choices = format_options,
                  multiple = TRUE, selectize = TRUE, selected = format_options),
      uiOutput("dsf_ex_cols"),
      actionButton("dsf_format", "Format", width = blotter_field_default_width)
    ),
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Save")),
      width = tp_wid_hlf,
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        textInput("dsf_nname", label = NULL, 
                  value = "Predictors R**", 
                  width = file_dir_field_width)
      ),
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        actionButton("dsf_save", "Save", width = blotter_field_default_width),
        textOutput("dsf_save_message")
      )
    )   
  ),
  br(),
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictors")),
      width = tp_wid_wde,
      DT::dataTableOutput("dsf_dts")
    ) 
  ),
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Predictor Specs")),
      width = tp_wid_hlf,
      DT::dataTableOutput("dsf_ovw")
    ),
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Target")),
      width = tp_wid_nar,
      DT::dataTableOutput("dsf_lbs")
    )
  )
)