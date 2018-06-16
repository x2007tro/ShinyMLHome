##
# Load data
##
pngs <- reactive({
  req(input$upv_upload$datapath)
  fs <- input$upv_upload$datapath
})

##
# Handle data setup for pdf viewer
##
observe({
  lapply(1:length(pngs()), function(i){
    output[[paste0("pngview",i)]] <- renderImage({
      list(src = pngs()[[i]],
           contentType = 'image/png',
           width = image_dim[1],
           height = image_dim[2],
           alt = "This is alternate text")
    }, deleteFile = FALSE)
  })
})
