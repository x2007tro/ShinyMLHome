tp_data_format <- tabPanel(
  "Format",
  fluidRow(
    column(
      tags$h5("FEATURES"),
      width = 4,
      selectInput("dsf_format_choice", label = "Options", choices = format_options,
                  multiple = TRUE, selectize = TRUE, selected = format_options),
      uiOutput("dsf_ex_cols"),
      actionButton("dsf_format", "Format", width = blotter_field_default_width)
    ),
    column(
      tags$h5("SAVE"),
      width = 8,
      tags$div(
        style="padding:0px, margin:0px, height:50%",
        textInput("dsf_nname", label = NULL, 
                  value = paste0(proj_dir, "Dataset/train_final.csv"), 
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
      tags$h5("OVERVIEW"),
      width = 3,
      DT::dataTableOutput("dsf_ovw")
    ),
    column(
      tags$h5("DATASET"),
      width = 7,
      DT::dataTableOutput("dsf_dts")
    ),
    column(
      tags$h5("LABELS"),
      width = 2,
      DT::dataTableOutput("dsf_lbs")
    )
  )
)