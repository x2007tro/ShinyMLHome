observeEvent(input$mtfg_run, {
  ##
  # first thing first
  mdl_nm <- "tensorflow"
  score_board_opt <- model_output_specs[model_output_specs$model == mdl_nm, "score_board"]
  conf_mtrx_opt <- model_output_specs[model_output_specs$model == mdl_nm, "conf_mtrx"]
  var_imp_opt <- model_output_specs[model_output_specs$model == mdl_nm, "var_imp"]
  tree_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "tree_plot"]
  cp_table_opt <- model_output_specs[model_output_specs$model == mdl_nm, "cp_table"]
  lc_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "learning_curve_plot"]
  tree_pick_ipt <- model_output_specs[model_output_specs$model == mdl_nm, "tree_pick_input"]
  cv_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "cv_plot"]
  pd_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "partial_dep_plot"]
  dn_plot_opt <- model_output_specs[model_output_specs$model == mdl_nm, "deepness_plot"]
  
  # step 1. data formatting
  fmtd_data <- FormatData4Model(
    prdctrs = dataset()$predictors,
    tgt = dataset()$target, 
    tgt_map = dataset()$target_map,
    job = input$cgen_job_type,
    model = mdl_nm
  )
  
  # step 2. model specific parameters
  res <- lapply(1:nrow(tf_pars), function(i){
    pnm <- paste0("mtfp_", tf_pars[i, "par"])
    res <- CreateParRange("grid", input[[paste0(pnm, "_beg")]], input[[paste0(pnm, "_end")]], input[[paste0(pnm, "_inc")]])
  })
  names(res) <- tf_pars$par
  tuning_pars <- expand.grid(res)
  
  # step 3. universal model parameters
  static_pars <- lapply(1:nrow(unv_pars), function(i){
    res <- input[[paste0("mtfg_", unv_pars[i, "par"])]]
    ifelse(res == "y", TRUE, FALSE)
  })
  names(static_pars) <- unv_pars$par
  
  # step 2. run grid search
  withProgress(
    message = paste0(mdl_nm, " train in progress. "),
    detail = 'This may take a while ...', value = 0, {
      tuning_res <- tryCatch({
        br <- GridSearchTensorflow2(
          proj = input$cgen_proj_name,
          model_name = mdl_nm,
          dataset = fmtd_data$predictors,
          labels = fmtd_data$target,
          job = input$cgen_job_type,
          val_size = input$cgen_val_size,
          cv_rep = input$cgen_cv_rep,
          mdl_pars = tuning_pars,   # data.frame
          stc_pars = static_pars,   # list
          tgt_map = dataset()$target_map
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
      output$mtfg_sb <- DT::renderDataTable({
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
      output$mtfg_cfmtx <- renderUI({
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
      output$mtfg_varimp <- renderUI({
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
              mdl <- cv_sets[[cv_id]]
              meat <- xgboost::xgb.importance(mdl)
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
      output$mtfg_cpt <- renderUI({
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
    # Model specific input - tree pick
    ##
    if(tree_pick_ipt){
      # output$mtfpl_tree_pick <- renderUI({
      #   ##
      #   # determine max num of trees
      #   max_num_tree <- min(tuning_pars$nrounds, na.rm = TRUE)
      #   ##
      #   # output ui component
      #   selectInput("tree_pick_id", label = "random tree printed", choices = 1:max_num_tree,
      #               selected = floor(sample.int(max_num_tree, 1)), multiple = FALSE, selectize = TRUE,
      #               width = "200px")
      # })
    }
    
    ##
    # Model specific output - tree plot
    ##
    if(tree_plot_opt){
      ##
      # generate a random tree id
      max_num_tree <- min(tuning_pars$nrounds, na.rm = TRUE)
      tree_id <- sample.int(max_num_tree, 1)
      
      ##
      # first create output objects
      output$mtfpl_tree <- renderUI({
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
                                     paste0("Parameter set ",ps_id," (CV #", cv_id, ")",
                                            " (Tree ", tree_id, ")"))),
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
              xgboost::xgb.plot.tree(mdl, trees = tree_id-1)
            })
          }, i, cv_sets)
        })
      })
    } 
    
    ##
    # Model specific output - learning curve plot
    ##
    if(lc_plot_opt){
      ##
      # first create output objects
      output$mtfpl_lc <- renderUI({
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
                    plotOutput(paste0(mdl_nm, "_lc_", ps_id, "_", cv_id))
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
            output[[paste0(mdl_nm, "_lc_", ps_id, "_", cv_id)]] <- renderPlot({
              ##
              # plot the learning curve
              mdl <- cv_sets[[cv_id]]
			        met <- mdl$metrics
			        mdl_lc <- dplyr::bind_cols(met)
			        mdl_lc$iter <- 1:nrow(mdl_lc)
			        FitPlot("Tensorflow via Keras", input$cgen_job_type, mdl_lc, "iter", "acc", "val_acc")
            })
          }, i, cv_sets)
        })
      })
    } 
    
    ##
    # Model specific output - cv plot
    ##
    if(cv_plot_opt){
      ##
      # first create output objects
      output$mtfpl_cv <- renderUI({
        opt <- lapply(1:length(res$cv_results), function(i){
          cv_sets <- res$cv_results[[i]]
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
                    plotOutput(paste0(mdl_nm, "_cv_", ps_id, "_", cv_id))
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
      lapply(1:length(res$cv_results), function(i){
        local({
          cv_sets <- res$cv_results[[i]]
          lapply(1:length(cv_sets), function(cv_id, ps_id, cv_sets){
            ##
            # render output (modify)
            output[[paste0(mdl_nm, "_cv_", ps_id, "_", cv_id)]] <- renderPlot({
              ##
              # plot the learning curve
              result <- cv_sets[[cv_id]]
              plot(result$n.var, result$error.cv, log="x", type="o", lwd=2)
            })
          }, i, cv_sets)
        })
      })
    } 
    
    ##
    # Model specific output - partial dependence plot
    ##
    if(pd_plot_opt){
      ##
      # first create output objects
      output$mtfpl_pd <- renderUI({
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
                    plotOutput(paste0(mdl_nm, "_pd_", ps_id, "_", cv_id))
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
            output[[paste0(mdl_nm, "_pd_", ps_id, "_", cv_id)]] <- renderPlot({
              ##
              # plot the partial dependence graph
              impvar <- fmtd_data$specs$feature
              
              n_fea <- ncol(fmtd_data$predictors)
              par(mfrow = c(ceiling(n_fea/5), 5))
              
              #par(mfrow=c(1, 1))
              for (i in seq_along(impvar)) {
                RRF::partialPlot(cv_sets[[cv_id]], fmtd_data$predictors, impvar[i], xlab=impvar[i],
                                 main=paste("Partial Dependence on", impvar[i]),
                                 ylim=c(30, 70))
              }
            })
          }, i, cv_sets)
        })
      })
    } 
  }
  
  ##
	# Model specific output - learning curve plot
  ##
  if(dn_plot_opt){
    ##
    # first create output objects
    output$mtfpl_dn <- renderUI({
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
                  plotOutput(paste0(mdl_nm, "_dn_", ps_id, "_", cv_id))
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
          output[[paste0(mdl_nm, "_dn_", ps_id, "_", cv_id)]] <- renderPlot({
            ##
            # plot the learning curve
            mdl <- cv_sets[[cv_id]]
            xgboost::xgb.ggplot.deepness(mdl, which = "2x1", pch=16, col=rgb(0,0,1,0.3), cex=2)
          })
        }, i, cv_sets)
      })
    })
  } 
  
  ##
  # step 5. output run message
  ##
  output$mtfg_run_msg <- renderText({
    msg
  })
  
})