##
# Handle data setup for model ensemble
##
pred_data <- reactive({
  req(input$med_upload)
  fs <- input$med_upload$datapath
  n_mdl <- length(fs)
  
  # find the models for the selected file
  fn <- input$med_upload$name
  mdls <- lapply(1:n_mdl, function(i){
    fn <- fn[i]
    res <- "NA"
    for(j in 1:length(all_models)){
      mdl_nm <- all_models[j]
      if(grepl(mdl_nm, fn)){
        res <- mdl_nm
        break
      }
    }
    res
  })
  mdl_nms <- unlist(mdls)
  
  # compile data
  prob <- NULL
  clas <- NULL
  for(i in 1:length(fs)){
    dsb <- read.csv(fs[i], header = TRUE, stringsAsFactors = FALSE)
    pb <- dsb[,c("index", "prob")]
    cl <- dsb[,c("index", "pred")]
    
    if(i == 1){
      prob <- pb
      clas <- cl
    } else {
      prob <- dplyr::inner_join(prob, pb, by = "index")
      clas <- dplyr::inner_join(clas, cl, by = "index")
    }
  }
  colnames(prob) <- c("index", mdl_nms)
  colnames(clas) <- c("index", mdl_nms)
  
  # update weighted selector parameter
  output$mew_par_selector <- renderUI({
    lapply(1:n_mdl, function(i){
      tags$div(class = "pa_fields", 
               numericInput(paste0('wgt_',mdl_nms[i]), mdl_nms[i], value = 1/n_mdl, width = blotter_field_default_width))
    })
  })
  
  res <- list(
    n_model = n_mdl,
    model_names = mdl_nms,
    prob = prob,
    pred = clas
  )
})

##
# Preview probability data
##
output$med_prob <- DT::renderDataTable({
  DT::datatable(
    pred_data()$prob, 
    options = list(
      pageLength = 5,
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
# Preview class data
##
output$med_pred <- DT::renderDataTable({
  DT::datatable(
    pred_data()$pred, 
    options = list(
      pageLength = 5,
      orderClasses = TRUE,
      searching = TRUE,
      paging = TRUE,
      scrollX = 400,
      scrollY = 400,
      scrollCollapse = TRUE),
    rownames = FALSE
  )
})