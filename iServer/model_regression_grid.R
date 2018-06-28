observeEvent(input$mrgg_run, {
  ##
  # first thing first
  mdl_nm <- "regression"
  score_board_opt <- model_output_specs[model_output_specs$model == mdl_nm, "score_board"]
  conf_mtrx_opt <- model_output_specs[model_output_specs$model == mdl_nm, "conf_mtrx"]
  var_imp_opt <- model_output_specs[model_output_specs$model == mdl_nm, "var_imp"]
  tree_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "tree_plot"]
  cp_table_opt <- model_output_specs[model_output_specs$model == mdl_nm, "cp_table"]
  
  # step 1. data formatting
  fmtd_data <- FormatData4Model(
    prdctrs = dataset()$predictors,
    tgt = dataset()$target, 
    tgt_map = dataset()$target_map,
    job = input$cgen_job_type,
    model = mdl_nm
  )
  
  # step 2. model specific parameters
  res <- lapply(1:nrow(rg_pars), function(i){
    pnm <- paste0("mrgp_", rg_pars[i, "par"])
    res <- CreateParRange("grid", input[[paste0(pnm, "_beg")]], input[[paste0(pnm, "_end")]], input[[paste0(pnm, "_inc")]])
  })
  names(res) <- rg_pars$par
  tuning_pars <- expand.grid(res)
  
  # step 3. universal model parameters
  static_pars <- lapply(1:nrow(unv_pars), function(i){
    res <- input[[paste0("mrgg_", unv_pars[i, "par"])]]
    ifelse(res == "y", TRUE, FALSE)
  })
  names(static_pars) <- unv_pars$par
  
  # step 2. run grid search
  withProgress(
    message = paste0(mdl_nm, " train in progress. "),
    detail = 'This may take a while ...', value = 0, {
      tuning_res <- tryCatch({
        br <- GridSearchReg2(
          proj = input$cgen_proj_name,
          model_name = mdl_nm,
          dataset = fmtd_data$predictors,
          labels = fmtd_data$target,
          job = input$cgen_job_type,
          val_size = input$cgen_val_size,
          cv_rep = input$cgen_cv_rep,
          mdl_pars = tuning_pars,   # data.frame
          stc_pars = static_pars    # list
        )
        msg <- paste0(mdl_nm, " run success!")
        list(br, msg)
      },
      error = function(cond) {
        print("Here's the original error message:")
        print(cond)
        msg <- paste0(mdl_nm, " run failed!")
        br <- data.frame(result = paste0(mdl_nm, " run failed: ", cond))
        list(br, msg)
      },
      # warning=function(cond) {
      #   msg <- paste0(mdl_nm, " run failed!")
      #   res <- data.frame(result = paste0(mdl_nm, " run failed: ", cond))
      #   list(res, msg)
      # },
      finally={
      })
    })
  
  # res structure
  # res = list(
  #   score_board (data.frame),
  #   models = list (
  #     parameter set 1 = list (model1, model2, model3, ...),
  #     parameter set 2 = list (model1, model2, model3, ...),
  #     ......
  #   ),
  #   train_results = list (
  #     parameter set 1 = list (
  #        cross val 1 = list (
  #          item 1 = probablity (data.frame),
  #          item 2 = prediction (data.frame),
  #          item 3 = accuracy (numeric),
  #          item 4 = prediction na (numeric),
  #          item 5 = confusion matrix (matrix)
  #        )
  #        cross val 2 = list (
  #          item 1 = probablity (data.frame),
  #          item 2 = prediction (data.frame),
  #          item 3 = accuracy (numeric),
  #          item 4 = prediction na (numeric),
  #          item 5 = confusion matrix (matrix)
  #        )        
  #     )
  #     parameter set 2 ......
  #     ......
  #   ),
  #   valdn_results same structure as train_results
  # )
  # 
  res <- tuning_res[[1]]
  msg <- tuning_res[[2]]
  
  # step 3. if output text results
  SaveResults(res$score_board, static_pars$output_dir, mdl_nm, static_pars$save_res)
  
  # step 4. if training run successfully, output
  if(msg != paste0(mdl_nm, " run failed!")){
    ##
    # output scoreboard
    ##
    if(score_board_opt) {
      output$mrgg_sb <- DT::renderDataTable({
        DT::datatable(
          res$score_board, 
          options = list(dom = "t"),
          rownames = FALSE
        )
      })
    }
    
    ##
    # output confusion matrix only if job = bc
    ##
    if(conf_mtrx_opt & input$cgen_job_type == "bc"){
      ##
      # first create output objects
      output$mrgg_cfmtx <- renderUI({
        opt <- lapply(1:length(res$valdn_results), function(i){
          cv_sets <- res$valdn_results[[i]]
          fluidRow(
            lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
              column(
                width = floor(12 * 1/length(cv_sets)),
                tags$div(class = "title_wrapper", 
                         tags$h6(class = "title_content_sm", 
                                 paste0("Parameter set ",ps_id," (CV #", cv_id, ")"))),
                DT::dataTableOutput(paste0(mdl_nm, "_cfm_", ps_id, "_", cv_id))
              )
            }, i, cv_sets) 
          )
        })
        do.call(tagList, opt)
      })
      
      ##
      # then render confusion matrix objects
      lapply(1:length(res$valdn_results), function(i){
        local({
          cv_sets <- res$valdn_results[[i]]
          lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
            ##
            # render output
            output[[paste0(mdl_nm, "_cfm_", ps_id, "_", cv_id)]] <- DT::renderDataTable({
              ##
              # produce meat
              meat <- cv_sets[[cv_id]]$cf
              ##
              # present meat
              DT::datatable(meat,
                            options = list(
                              pageLength = 5,
                              orderClasses = TRUE,
                              searching = FALSE,
                              paging = FALSE,
                              scrollX = 50,
                              scrollY = 100,
                              scrollCollapse = TRUE,
                              autoWidth = FALSE),
                            rownames = FALSE)
            })
          }, i, cv_sets)
        })
      })
    }
    
    ##
    # Model specific output - variance importance
    # (modify for specific model)
    ##
    if(var_imp_opt){
      ##
      # first create output objects
      output$mrgg_varimp <- renderUI({
        opt <- lapply(1:length(res$models), function(i){
          cv_sets <- res$models[[i]]
          fluidRow(
            lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
              column(
                width = floor(12 * 1/length(cv_sets)),
                tags$div(class = "title_wrapper", 
                         tags$h6(class = "title_content_sm", 
                                 paste0("Parameter set ",ps_id," (CV #", cv_id, ")"))),
                DT::dataTableOutput(paste0(mdl_nm, "_vi_", ps_id, "_", cv_id))
              )
            }, i, cv_sets) 
          )
        })
        do.call(tagList, opt)
      })
      
      ##
      # then render output objects
      lapply(1:length(res$models), function(i){
        local({
          cv_sets <- res$models[[i]]
          lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
            ##
            # render output (modify)
            output[[paste0(mdl_nm, "_vi_", ps_id, "_", cv_id)]] <- DT::renderDataTable({
              ##
              # produce meat (modify)
              mdl_var_imp <- cv_sets[[cv_id]]$variable.importance
              meat <- data.frame(
                variable = names(mdl_var_imp),
                importance = round(mdl_var_imp, 2),
                stringsAsFactors = FALSE
              ) 
              ##
              # present meat
              DT::datatable(meat,
                            options = list(
                              pageLength = 5,
                              orderClasses = TRUE,
                              searching = FALSE,
                              paging = FALSE,
                              scrollX = 50,
                              scrollY = 200,
                              scrollCollapse = TRUE,
                              autoWidth = FALSE),
                            rownames = FALSE)
            })
          }, i, cv_sets)
        })
      })
    }
    
    ##
    # Model specific output - cp table
    # (modify for specific model)
    ##
    if(cp_table_opt){
      ##
      # first create output objects
      output$mrgg_cpt <- renderUI({
        opt <- lapply(1:length(res$models), function(i){
          cv_sets <- res$models[[i]]
          fluidRow(
            lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
              column(
                width = floor(12 * 1/length(cv_sets)),
                tags$div(class = "title_wrapper", 
                         tags$h6(class = "title_content_sm", 
                                 paste0("Parameter set ",ps_id," (CV #", cv_id, ")"))),
                DT::dataTableOutput(paste0(mdl_nm, "_cpt_", ps_id, "_", cv_id))
              )
            }, i, cv_sets) 
          )
        })
        do.call(tagList, opt)
      })
      
      ##
      # then render output objects
      lapply(1:length(res$models), function(i){
        local({
          cv_sets <- res$models[[i]]
          lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
            ##
            # render output (modify)
            output[[paste0(mdl_nm, "_cpt_", ps_id, "_", cv_id)]] <- DT::renderDataTable({
              ##
              # produce meat (modify)
              meat <- round(cv_sets[[cv_id]]$cptable, 2)
              ##
              # present meat
              DT::datatable(meat,
                            options = list(
                              pageLength = 5,
                              orderClasses = TRUE,
                              searching = FALSE,
                              paging = FALSE,
                              scrollX = 50,
                              scrollY = 200,
                              scrollCollapse = TRUE,
                              autoWidth = FALSE),
                            rownames = FALSE)
            })
          }, i, cv_sets)
        })
      })
    }
    
    ##
    # Model specific output - tree plot
    ##
    if(tree_plot_opt){
      ##
      # first create output objects
      output$mrgp_orig <- renderUI({
        opt <- lapply(1:length(res$models), function(i){
          cv_sets <- res$models[[i]]
          fluidRow(
            column(
              width = 12,
              lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
                fluidRow(
                  column(
                    width = 12,
                    tags$div(class = "title_wrapper", 
                             tags$h6(class = "title_content_sm", 
                                     paste0("Parameter set ",ps_id," (CV #", cv_id, ")"))),
                    plotOutput(paste0(mdl_nm, "_tr_", ps_id, "_", cv_id))
                  )
                )
              }, i, cv_sets)  
            )
          )
        })
        do.call(tagList, opt)
      })
      
      ##
      # then render output objects
      lapply(1:length(res$models), function(i){
        local({
          cv_sets <- res$models[[i]]
          lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
            ##
            # render output (modify)
            output[[paste0(mdl_nm, "_tr_", ps_id, "_", cv_id)]] <- renderPlot({
              ##
              # produce meat (modify)
              mdl <- cv_sets[[cv_id]]
              ##
              # present meat
              par(mfrow=c(1, ncol(fmtd_data$predictors)))
              plot(mdl, ask = FALSE)
            })
          }, i, cv_sets)
        })
      })
    } 
  }
  
  ##
  # step 5. output run message
  ##
  output$mrgg_run_msg <- renderText({
    msg
  })
  
})