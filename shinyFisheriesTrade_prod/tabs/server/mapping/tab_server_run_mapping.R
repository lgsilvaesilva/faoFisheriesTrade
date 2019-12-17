run_mapping_values <- reactiveValues(ft_workflow = ReadDatatable("fishtrade_reporter_workflow"))

values <- reactiveValues(data_mapping = data.frame(), 
                         data_loaded = data.frame(), 
                         data_full = data.frame()
                         # data_full = get_tradedata_mapped()
                         )
##-- Button: Years ----
#' It shows only the years which there are data built for them.
#' 

observe({
  
  if(input$navbar != "mapping") return(NULL)
  
  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  ft_workflow <- isolate(run_mapping_values$ft_workflow)
  trade_years <- run_mapping_values$ft_workflow[is_built_data == 1, unique(year)]
  
  updatePickerInput(session = session, 
                    inputId = "mapping_years", 
                    choices = trade_years)
  
})

observeEvent(c(input$btn_save_built_data, input$mapping_years, input$navbar), {
  
  if(input$navbar != "mapping") return(NULL)
  
  reporters_built <- run_mapping_values$ft_workflow[year == input$mapping_years & is_built_data == 1, unique(rep)]
  reporters_built <- m49_codes[reporter %in% reporters_built, ]
  rep2map_list <- reporters_built$reporter
  names(rep2map_list) <- reporters_built$country
  
  updatePickerInput(session = session,
                    inputId = "reporter_to_map", 
                    choices = rep2map_list)
  
})

# output$btn_mapping_year <- renderUI({
#   
#   input$btn_save_built_data
#   ft_workflow <- isolate(run_mapping_values$ft_workflow)
#   trade_years <- run_mapping_values$ft_workflow[is_built_data == 1, unique(year)]
# 
#   pickerInput(inputId = "mapping_years", 
#               multiple = FALSE,
#               label = "Year", 
#               choices = trade_years, 
#               options = list(
#                 `actions-box` = TRUE)
#   )
#   
# })

##-- Button: Reporter ----
#' given the selected year, It shows only the reporters which have data built.
# output$btn_reporter_to_map <- renderUI({
#   
#   validate(need(input$mapping_years, "Please, select a year."))
#   
#   reporters_built <- run_mapping_values$ft_workflow[year == input$mapping_years & is_built_data == 1, unique(rep)]
#   reporters_built <- m49_codes[reporter %in% reporters_built, ]
#   rep2map_list <- reporters_built$reporter
#   names(rep2map_list) <- reporters_built$country
#   
#   pickerInput(inputId = "reporter_to_map", 
#               label = "Reporter:", 
#               choices = rep2map_list,
#               options = list(`live-search` = TRUE))
# })

##-- Confirmation dialog box ----
observeEvent(input$run_map, {
  
  confirmSweetAlert(
    session = session,
    inputId = "run_map_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Mapping process could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

##-- Running Mapping ----
observeEvent(input$run_map_confirmation, {
  
  if(input$run_map_confirmation == TRUE) {
    
    validate(need(input$reporter_to_map, message = "Please select at least a country"),
             need(input$mapping_years, message = "Please select at least a year"))
    
    ##-- Progress bar
    #' progress bar to be shown to user while the process is running
    progress <- Progress$new(session, min = 1, max = 10)
    on.exit(progress$close())
    
    progress$set(message = 'Mapping in progress',
                 detail = 'This may take a while...')
    
    progress$set(value = 5.5)
    
    ##---
    sql_filter <- sprintf("rep = '%s' AND year = '%s'", 
                          input$reporter_to_map,  
                          input$mapping_years)
   
    data2map <- ReadDatatable(fishtrade_built_data, where = sql_filter)
    out <- run_mapping_module(.data = data2map, 
                              .eucountry = input$is_european, 
                              .local = .local)
    
    ##-- Update data view ----
    # tradedata_mapped <- ReadDatatable(table = fishtrade_table_to_map)
    # setnames(tradedata_mapped, "rep", "reporter")
    # # tradedata_mapped_descr <- left_join(tradedata_mapped, m49_codes, by = 'reporter')
    # tradedata_mapped_descr <- copy(tradedata_mapped)
    # setDT(tradedata_mapped_descr, key = c('reporter', 'tariff_line'))
    # tradedata_mapped_descr[, `:=`(reporter = as.integer(reporter),
    #                               flow = as.integer(flow),
    #                               startyear = as.integer(startyear),
    #                               endyear = as.integer(endyear),
    #                               isscfc_code = as.character(ifelse(is.na(isscfc_code), "", 
    #                                                                 isscfc_code)),
    #                               id = 1:.N)]
    # 
    # values$data_full <- tradedata_mapped_descr
    
    #' close the progress bar
    progress$set(value = 10)
    
    if(out$status == "to_map") {
      
      updateReporterStatus(.year = input$mapping_years, 
                           .reporter = input$reporter_to_map, 
                           .process = c("is_run_mapping", "is_mapped_data", "is_run_imputation", 
                                        "is_imputed_data", "is_run_outlier", "is_outlier_validated", 
                                        "is_run_mirroring", "is_mirroring_validated"), 
                           .value = c(1, rep(0, 7)))
      
      run_mapping_values$ft_workflow <-  ReadDatatable("fishtrade_reporter_workflow")
      
      sendSweetAlert(
        session = session,
        title = "Mapping process has been finished!",
        text = sprintf("There are %s codes to be mapped.", out$n_missing),
        type = "warning"
      )
      
    }

    if(out$status == "nothing_to_map" & out$n_improve > 0) {
      
      updateReporterStatus(.year = input$mapping_years, 
                           .reporter = input$reporter_to_map, 
                           .process = c("is_run_mapping", "is_mapped_data", "is_run_imputation", 
                                        "is_imputed_data", "is_run_outlier", "is_outlier_validated", 
                                        "is_run_mirroring", "is_mirroring_validated"), 
                           .value = c(1, 1, rep(0, 6)))
      
      run_mapping_values$ft_workflow <-  ReadDatatable("fishtrade_reporter_workflow")
      
      msg_nothing_to_map <- "There are *NO* codes to be mapped."
      msg_to_improve <- sprintf("There are %s codes to be improved.", ifelse(out$n_improve == 0, "*NO*", out$n_improve))
      
      sendSweetAlert(
        session = session,
        title = "Great!",
        text = paste(msg_nothing_to_map, msg_to_improve, sep = "\n"),
        type = "success"
      )
      
    }
    
    if(out$status == "nothing_to_map" & out$n_improve == 0) {
      
      updateReporterStatus(.year = input$mapping_years, 
                           .reporter = input$reporter_to_map, 
                           .process = c("is_run_mapping", "is_mapped_data"), 
                           .value = c(1, 1))
      
      run_mapping_values$ft_workflow <-  ReadDatatable("fishtrade_reporter_workflow")
      
      msg_nothing_to_map <- "There are *NO* codes to be mapped."
      msg_to_improve <- sprintf("There are %s codes to be improved.", ifelse(out$n_improve == 0, "*NO*", out$n_improve))
      
      sendSweetAlert(
        session = session,
        title = "Great!",
        text = paste(msg_nothing_to_map, msg_to_improve, sep = "\n"),
        type = "success"
      )
      
    }
    
    
  }
})




















