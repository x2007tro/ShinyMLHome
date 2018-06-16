##
# Retrieve dataset and assign it to a variable
##
data <- reactive({
  req(input$dsu_upload)
  dsb <- read.csv(input$dsu_upload$datapath, header = TRUE, stringsAsFactors = FALSE)   
})

peek <- reactive({
  res <- DataInspection(data())
})

dataset <- reactive({
  d <- data()
  if(input$cgen_label_field == ""){
    res <- d
  } else {
    res <- tryCatch({
      d[,!(colnames(d) %in% input$cgen_label_field)]
    },
    error=function(cond) {
      print("Here's the original error message:")
      print(cond)
      return(d)
    },
    warning=function(cond) {
    },
    finally={
    }
    )
  }
  res
})

targets <- reactive({
  d <- data()
  if(input$cgen_label_field == ""){
    res <- rep("na", 10)
  } else {
    res <- tryCatch({
      d[,input$cgen_label_field]
    },
    error=function(cond) {
      print("Error creating targets (labels):")
      print(cond)
      return(rep("na", 10))
    },
    warning=function(cond) {
    },
    finally={
    })
  }
  res
})

targetsf <- reactive({
  tgts <- targets()
  labels_train <- as.factor(tgts)
  levels(labels_train) <- paste0("fc_", levels(labels_train))
  
  labels_train
})

##
# Display data peek
##
output$dsu_ovw <- DT::renderDataTable({
  DT::datatable(
    peek(), 
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
    dataset(), 
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
# Display label data
##
output$dsu_lbs <- DT::renderDataTable({
  lb_4d <- data.frame(labels = targets()[1:10])
  colnames(lb_4d) <- input$cgen_label_field
  DT::datatable(
    lb_4d, 
    options = list(
      pageLength = 10,
      orderClasses = FALSE,
      searching = TRUE,
      paging = FALSE),
    rownames = FALSE
  )
})