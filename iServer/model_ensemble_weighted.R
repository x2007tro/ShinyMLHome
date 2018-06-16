##
# Handle model stacking - weighted
##
observeEvent(input$mew_run, {
  
  if(input$cgen_job_type == "bc"){
    prob_bc <- pred_data()$prob
    prob_bc_meat <- prob_bc[,!(colnames(prob_bc) %in% "index")]
    prob_bc_idx <- prob_bc[,"index"]
    
    # calc weighted probability
    mdls <- colnames(prob_bc_meat)
    wgts <- sapply(1:length(mdls), function(i){
      nm <- mdls[i]
      wgt <- input[[paste0('wgt_',nm)]]
      return(wgt)
    })
    wgt_mtx <- matrix(wgts, ncol = 1)
    meat_mtx <- as.matrix(prob_bc_meat)
    wgt_prob <- meat_mtx %*% wgt_mtx
    wgt_pred <- as.numeric(wgt_prob > 0.5)
    
    # sort lablels
    shiny::validate(shiny::need(targets(), "Error: please select original data and then try again."))
    ori_lbls <- targets()
    lbls <- ori_lbls[prob_bc_idx]
    
    # outout for bc weight
    full_size <- length(lbls)
    val_size <- input$cgen_val_size
    tr_idxs <- 1:(full_size - val_size)
    val_idxs <- (full_size - val_size + 1):full_size
    
    tr_pred <- wgt_pred[tr_idxs]
    val_pred <- wgt_pred[val_idxs]
    
    tr_tgt <- lbls[tr_idxs]
    val_tgt <- lbls[val_idxs]
    
    tr_accr <- mean(tr_pred == tr_tgt)
    val_accr <- mean(val_pred == val_tgt)
    
    tr_cmtx <- caret::confusionMatrix(as.factor(tr_pred), as.factor(tr_tgt))
    val_cmtx <- caret::confusionMatrix(as.factor(val_pred), as.factor(val_tgt))
    
    # output
    output$mew_tr_accr <- renderText({
      paste0("Accuracy: ", format(tr_accr, digits = 2))
    })
    
    output$mew_val_accr <- renderText({
      paste0("Accuracy: ", format(val_accr, digits = 2))
    })
    
    output$mew_tr_cfmtx <- DT::renderDataTable({
      DT::datatable(
        tr_cmtx$table, 
        # options = list(
        #   pageLength = 10,
        #   orderClasses = TRUE,
        #   searching = TRUE,
        #   paging = TRUE,
        #   scrollX = 400,
        #   scrollY = 400,
        #   scrollCollapse = TRUE),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
    
    output$mew_val_cfmtx <- DT::renderDataTable({
      DT::datatable(
        val_cmtx$table, 
        # options = list(
        #   pageLength = 10,
        #   orderClasses = TRUE,
        #   searching = TRUE,
        #   paging = TRUE,
        #   scrollX = 400,
        #   scrollY = 400,
        #   scrollCollapse = TRUE),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  } else if(input$cgen_job_type == "mc"){
    showNotification("Stacking is currently not available for multiple classification job!")
    wgt_pred <- -1
    lbls <- -1
  } else if (input$cgen_job_type == "reg"){
    showNotification("Stacking is currently not available for regression job!")
    wgt_pred <- -1
    lbls <- -1
  } else {
    # nothing
  }
  
  # construct table output
  ori_pred <- pred_data()$pred
  ot_tbl <- cbind(ori_pred[,!(colnames(prob_bc) %in% "index")], wgt_pred, lbls)
  colnames(ot_tbl)[(ncol(ot_tbl)-1):ncol(ot_tbl)] <- c("Wgtd Prediction", "Targets")
  
  output$mew_new_pred <- DT::renderDataTable({
    DT::datatable(
      ot_tbl, 
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
})