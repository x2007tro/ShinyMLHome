##
# Listen to save data request
##
observeEvent(input$mec_save, {
  
  ff <- input$mec_save_to
  fdir <- dirname(ff)
  CreateDirIfNotExist(fdir)
  
  if(substr(ff,(nchar(ff)+1)-3,nchar(ff)) == "csv"){
    validate(need(pred_data(), message = "Error: missing prediction data!"))
    validate(need(targets(), message = "Error: missing original labels!"))
    
    meat1 <- pred_data()$pred
    meat1 <- meat1[,!(colnames(meat1) %in% "index")]
    
    meat2 <- targets()
    meat <- cbind(meat1, meat2)
    
    colnames(meat)[length(colnames(meat))] <- input$cgen_label_field
    
    output$mec_preview <- DT::renderDataTable({
      DT::datatable(
        meat, 
        options = list(
          pageLength = 10,
          orderClasses = TRUE,
          searching = TRUE,
          paging = TRUE,
          scrollX = 400,
          scrollY = 400,
          scrollCollapse = TRUE),
        rownames = FALSE
      )
    })
    
    write.csv(meat, file = ff, row.names = FALSE)
    
    msg <- paste0("Data is saved at ",
                  format(Sys.Date(),"%Y-%m-%d")," ",
                  format(Sys.time(),"%H:%M:%S"))
  } else {
    msg <- "Data is not saved!"
  }
  
  # display a message
  output$mec_save_message <- renderText({ msg })
})