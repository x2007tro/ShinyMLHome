tp_util_png_viewer <- tabPanel(
  "PNG Viewer",
  fluidRow(
    column(
      tags$div(class = "title_wrapper", tags$h6(class = "title_content_sm", "Source Data")),
      width = 12,
      fileInput(
        "upv_upload", 
        "Up to 4 files",
        multiple = TRUE,
        accept = c(".png")
      )
    )
  ),
  fluidRow(
    column(
      width = tp_wid_hlf,
      imageOutput("pngview1", width = image_dim[1], height = image_dim[2])
    ),
    column(
      width = 12 - tp_wid_hlf,
      imageOutput("pngview2", width = image_dim[1], height = image_dim[2])
    )
  ),
  fluidRow(
    column(
      width = tp_wid_hlf,
      imageOutput("pngview3", width = image_dim[1], height = image_dim[2])
    ),
    column(
      width = 12 - tp_wid_hlf,
      imageOutput("pngview4", width = image_dim[1], height = image_dim[2])
    )
  )
)