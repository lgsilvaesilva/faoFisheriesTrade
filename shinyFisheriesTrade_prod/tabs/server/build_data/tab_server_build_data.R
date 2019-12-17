##-- Reactive Values ----
bd_values <- reactiveValues(trade_built_data = list(tradedata = data.table(), call = list()))

##-- Confirmation build data dialog box ----
observeEvent(input$btn_build_data, {

  #' the user must confirm the task: building data
  confirmSweetAlert(
    session = session,
    inputId = "run_build_data",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Loading data could take a while.",
    danger_mode = TRUE, 
    html = T
  )

})

##-- Build Trade data ----
#' This observe event aims to create a data that will be used as input data by the modules:
#' mapping and imputation.
#' The user should select the year and the target country to build the data using different data sources for
#' each flow. For example:
#'                Year: 2018
#'                Reporter (country): 76 (Brazil)
#'                Flow 1 (import): UNSD data source
#'                Flow 2 (export): TDM data source
#' *Warning: this process DOES NOT SAVE the data on SWS*                
observeEvent(input$run_build_data, {
  
  validate(
    need(input$bd_year, 'Please choose at least one year'),
    need(input$bd_reporter, 'Please choose at least one country.'),
    need(input$bd_import_source, 'Please choose at least one source for imports.'),
    need(input$bd_export_source, 'Please choose at least one source for exports.')
  )
  
  #' it checks if the user has cancelled the building task and manage it properly
  if(is.null(input$run_build_data)) return(list(tradedata = data.table(), call = list()))
  if(input$run_build_data == FALSE) return(list(tradedata = data.table(), call = list()))
  
  year_selected     <- year(input$bd_year)
  source_selected   <- input$bd_source
  reporter_selected <- input$bd_reporter
  source_selected <- c(input$bd_import_source, input$bd_export_source)
  
  ##-- Progress bar 
  #' progress bar to be shown to user while the process is running
  progressSweetAlert(
    session = session, id = "save_imputation_progress",
    title = "Data building in progress",
    display_pct = TRUE, 
    value = 0
  )
  
  ##** updating progress bar 15 ----
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 55
  )
  ##---
  bd_values$trade_built_data <- get_tradedata(.year = year_selected,
                                              .reporter = reporter_selected,
                                              .flow = 1:2,
                                              .source = source_selected)
  
  ##---
  
  #' close progress bar
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 100
  )

  #' check if there are any data to be saved
  if(nrow(bd_values$trade_built_data$tradedata) == 0) {
    
    sendSweetAlert(
      session = session,
      title = "No data!",
      text = "Please, check if you already have built the data before to load them.",
      type = "error"
    )
    
    return(list(tradedata = data.table(), call = list()))
    
  } else {
    
    sendSweetAlert(
      session = session,
      title = "Data built successfully!",
      type = "success"
    )
    
  }

  
})

##-- Download built data ----
output$download_built_data <- downloadHandler(
  
  filename = function() {
    
    sprintf("%s-R%s-Y%s-I%s-E%s-trade_built_data.csv", 
            Sys.Date(),
            input$bd_reporter,
            year(input$bd_year),
            tolower(input$bd_import_source),
            tolower(input$bd_export_source))
    
  },
  
  content = function(file) {
    
    write.csv(bd_values$trade_built_data$tradedata, file, row.names = FALSE)
    
  }
)

##-- Confirmation save data dialog box ----
observeEvent(input$btn_save_built_data, {
  
  ##-- Overwrite check ----
  #' it checks if the user has already built a data previously for the year and country selected
  #' and try to avoid a data overwrite accidentally.
  sql_filter <- sprintf("rep = '%s' AND year = '%s'", 
                        bd_values$trade_built_data$call$reporter,  
                        bd_values$trade_built_data$call$year)
  
  data_check <- ReadDatatable(fishtrade_built_data, where = sql_filter, limit = 1)
  
  if(nrow(data_check) > 0) {
    
    confirmSweetAlert(
      session = session,
      inputId = "run_save_built_data",
      type = "warning",
      title = "Do you want to overwrite the data?",
      text = tags$span(tags$h3("If you overwrite the data, you will lose all modifications already done for this data. For instance, imputation, mapping, outliers correction.")),
      danger_mode = TRUE, 
      html = T
    )
    
  } else {
    
    confirmSweetAlert(
      session = session,
      inputId = "run_save_built_data",
      type = "warning",
      text = tags$span(tags$h3("Do you want to confirm?")),
      title = "Saving data could take a while.",
      danger_mode = TRUE, 
      html = T
    )
    
  }
  
  
  
})

##-- Save built data on SWS ----
#' After building the data, the user can validate it and the wish to save it on SWS as final input data.
#' Be aware that builds the data does not mean save the data on SWS. You should use the button "Save data"
#' to be sure that data was saved.
observeEvent(input$run_save_built_data, {
  
  #'if the user cancel the process
  if(input$run_save_built_data == FALSE) return(NULL)
  
  #' check if there are any data to be saved
  if(nrow(bd_values$trade_built_data$tradedata) == 0) {
    
    sendSweetAlert(
      session = session,
      title = "No data!",
      text = "Please, check if you already have built the data before to save them.",
      type = "error"
    )
    
    return(NULL)
    
  }
  
  
  ##-- Progress bar
  #' progress bar to be shown to user while the process is running
  progress <- Progress$new(session, min = 1, max = 10)
  on.exit(progress$close())
  
  progress$set(message = 'Saving data in progress',
               detail = 'This may take a while...')
  
  progress$set(value = 5.5)
  ##---
  
  sql_filter <- sprintf("rep = '%s' AND year = '%s'", 
                        bd_values$trade_built_data$call$reporter,  
                        bd_values$trade_built_data$call$year)
  
  data2save <- copy(bd_values$trade_built_data$tradedata)
  
  if(all(c("reporter", "partner", "hs") %in% names(data2save))) {
    setnames(data2save, old = c("reporter", "partner", "hs"), new = c("rep", "prt", "comm")) 
  }
  
  write_status <- writeDT(dt_name = fishtrade_built_data, 
                          data = data2save, 
                          sql_filter = sql_filter)
  
  ##---
  #' close the progress bar
  progress$set(value = 10)

  if(write_status == 0) {
    
    sendSweetAlert(
      session = session,
      title = "Data saved!",
      text = "Data saved successfully.",
      type = "success"
    )
    
    #' it updates the reporter status in the "building data" step from 0 to 1
    #' Once we build the data, all process that we have already done with the old data version will be lost 
    #' because the new data version can contain new records with new 
    #' commodities or values that might rise new mappings or new outliers.
    workflow_label <- names(ReadDatatable(table = "fishtrade_reporter_workflow", limit = 1))[-c(1:2)]
    updateReporterStatus(.year = bd_values$trade_built_data$call$year, 
                         .reporter = bd_values$trade_built_data$call$reporter, 
                         .process = c("is_built_data", workflow_label[-1]), 
                         .value = c(1, rep(0, length(workflow_label) - 1)))
    
  }
  
  
})

##-- Confirmation load data dialog box ----
observeEvent(input$btn_load_built_data, {
  
  #' the user must confirm the task: loading data
  confirmSweetAlert(
    session = session,
    inputId = "run_load_built_data",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Loading data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

##-- Load built data on SWS ----
#' Once the data has been saved on SWS the user may load it and visualises on Shiny app.
#' The function below reads the data from SWS and shows it to the user.
observeEvent(input$run_load_built_data, {

  #'if the user cancel the process
  if(input$run_load_built_data == FALSE) return(NULL)

  validate(
    need(input$bd_year, 'Please choose at least one year'),
    need(input$bd_reporter, 'Please choose at least one country.')
  )
  
  ##-- Progress bar
  progress <- Progress$new(session, min = 1, max = 10)
  on.exit(progress$close())
  
  progress$set(message = 'Loading data in progress',
               detail = 'This may take a while...')
  
  progress$set(value = 5.5)
  ##---
  
  sql_filter <- sprintf("rep = '%s' AND year = '%s'", input$bd_reporter,  year(input$bd_year))
  bd_values$trade_built_data$tradedata <- ReadDatatable(table = fishtrade_built_data, where = sql_filter)
  bd_values$trade_built_data$call <- list(reporter = input$bd_reporter, 
                                          year = year(input$bd_year), 
                                          source = c(input$bd_import_source, input$bd_export_source), 
                                          flow = 1:2)
  #' close the progress bar
  progress$set(value = 10)
  
  #' check if there are any data to be saved
  if(nrow(bd_values$trade_built_data$tradedata) == 0) {
    
    sendSweetAlert(
      session = session,
      title = "No data!",
      text = "Please, check if you already have built the data before to load them.",
      type = "error"
    )
    
    return(NULL)
    
  } else {
    
    sendSweetAlert(
      session = session,
      title = "Data loaded successfully!",
      type = "success"
    )
    
  }
  
  
  
})

##-- Table: built data ----
#' This the table shows the built or loaded by the user.
output$tbl_built_data <- DT::renderDataTable(server = TRUE, {
  
  #'it checks if there are data to be shown
  if(nrow(bd_values$trade_built_data$tradedata) == 0 & ncol(bd_values$trade_built_data$tradedata)) {

    sendSweetAlert(
      session = session,
      title = "No data!",
      text = "There is no data to be shown.",
      type = "info"
    )
    
    return(NULL)
    
  }
  
  datatable(bd_values$trade_built_data$tradedata, 
            rownames = FALSE, 
            filter = 'top'
  ) 
  
})








