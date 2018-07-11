##
# Retrieve dataset and assign it to a variable
##
dataset <- eventReactive({c(
  input$dsu_upload,
  input$dsf_save,
  input$dss_save
)}, {
  # First, read table content
  a <- ReadDataFromADB(input$cgen_db_path, input$dsu_upload)
  rownames(a) <- 1:nrow(a)
  b <- DataInspection(a)
  c <- ReadDataFromADB(input$cgen_db_path, input$cgen_db_tgt_src)
  d <- ReadDataFromADB(input$cgen_db_path, input$cgen_db_tgt_map)
  e <- ListTblsFromADB(input$cgen_db_path)
  
  # Second, update upload values
  updateSelectInput(session, "dsu_upload", NULL, choices = e, selected = input$dsu_upload)
  
  res <- list(
    predictors = a,
    specs = b,
    target = c,
    target_map = d
  )
  
  return(res)
})

# targetsf <- reactive({
#   tgts <- targets()
#   labels_train <- as.factor(tgts)
#   levels(labels_train) <- paste0("fc_", levels(labels_train))
#   
#   labels_train
# })

##
# Display data peek
##
output$dsu_ovw <- DT::renderDataTable({
  DT::datatable(
    dataset()$specs, 
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

##
# Display detailed data
##
output$dsu_dts <- DT::renderDataTable({
  DT::datatable(
    dataset()$predictors, 
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
})

##
# Display label data
##
output$dsu_lbs <- DT::renderDataTable({
  DT::datatable(
    dataset()$target,
    options = list(
      pageLength = nrow(dataset()$specs),
      orderClasses = FALSE,
      searching = FALSE,
      paging = TRUE,
      scrollX = 400,
      scrollY = 400,
      scrollCollapse = TRUE),
    rownames = FALSE
  )
})