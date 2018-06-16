##
# Handle data setup for pdf viewer
##
observeEvent(input$vpv_upload, {
  fs <- input$vpv_upload$datapath
  n_pdf <- length(fs)
  fn <- input$vpv_upload$name
  
  # update weighted selector parameter
  lapply(1:n_pdf, function(i){
    file.copy(fs[[i]],paste0(shiny_dir, "www"), overwrite = T)
    output[[paste0("pngview",i)]] <- renderUI({
      tags$iframe(style = "height:500px;width:660px", 
                  src = paste0(i-1,".png"))
    })
  })
  
})
