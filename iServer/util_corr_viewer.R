##
# Handle data setup for correlation viewer
##
corr_data <- eventReactive({c(
  input$ucv_upload,
  input$dsf_save,
  input$dss_save
)}, {
  dsb1 <- ReadDataFromSSviaCS(input$cgen_db_path, input$ucv_upload)
  dsb2 <- ReadDataFromSSviaCS(input$cgen_db_path, input$cgen_db_tgt_src)
  dsb <- cbind.data.frame(dsb2, dsb1)
  return(dsb)
})

##
# Feature selection display
##
output$ucv_features <- renderUI({
  dsb <- corr_data()
  list(
    selectInput("ucv_slt_fts", label = NULL, 
                choices = colnames(dsb),
                multiple = TRUE, selectize = TRUE, 
                selected = colnames(dsb)),
    actionButton("ucv_select", "Run", width = blotter_field_default_width)
  )
})

##
# output correlation matrix
##
observeEvent(input$ucv_select, {
  dsb <- corr_data()
  
  # Kendall correlation
  dsb_dis <- ToNumeric(input$ucv_slt_fts, dsb)
  corr_res <- tryCatch({
    cm <- cor(dsb_dis, method = "pearson")
    msg <- "correlation matrix success!"
    list(cm = cm, msg = msg)
  }, 
  error = function(cond) {
    print("Here's the original error message:")
    print(cond)
    corr <- data.frame(result = paste0("correlation matrix failed: ", cond))
    msg <- "correlation matrix failed!"
    list(cm = cm, msg = msg)
  },
  finally = {
    
  })
  
  output$corr_mtx <- DT::renderDataTable(
    DT::datatable(
      corr_res$cm, 
      options = list(
        pageLength = 10,
        orderClasses = TRUE,
        searching = TRUE,
        paging = TRUE,
        scrollX = 400,
        scrollY = 400,
        scrollCollapse = TRUE),
      rownames = TRUE
    )
  )
  
  if(corr_res$msg == "correlation matrix success!"){
    output$corr_plot <- renderPlot(
      corrplot::corrplot(corr_res$cm, type = "upper", order = "original", 
               tl.col = "black", tl.srt = 45)
    )
  }
  
  # Goodman and Kruskal
  dsb2_dis <- GKTauMatrix(input$ucv_slt_fts, dsb)
  output$gk_corr_mtx <- DT::renderDataTable(
    DT::datatable(
      dsb2_dis, 
      options = list(
        pageLength = 10,
        orderClasses = TRUE,
        searching = TRUE,
        paging = TRUE,
        scrollX = 400,
        scrollY = 400,
        scrollCollapse = TRUE),
      rownames = TRUE
    )
  )
  
  output$gk_corr_plot <- renderPlot(
    corrplot::corrplot(dsb2_dis, type = "full", order = "original", 
             tl.col = "black", tl.srt = 45)
  )
})
