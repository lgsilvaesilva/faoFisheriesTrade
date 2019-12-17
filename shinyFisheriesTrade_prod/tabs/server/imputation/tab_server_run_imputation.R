##-- Reactive Values ----
values_imputation <- reactiveValues(data_imputed = data.table())

##-- Button: Years ----
observe({

  if(input$navbar != "imputation") return(NULL)

  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  trade_years <- run_mapping_values$ft_workflow[is_built_data == 1 & is_run_mapping == 1 & is_mapped_data == 1, unique(year)]
  
  updatePickerInput(session = session, 
                    inputId = "impute_years", 
                    choices = trade_years)
  
})
##-- Button: reporter ----
observeEvent(c(input$btn_save_built_data, input$impute_years, input$tab_imputation), {
  
  validate(need(input$impute_years, ""))
  
  reporters_built <- run_mapping_values$ft_workflow[year == input$impute_years & is_built_data == 1 & is_run_mapping == 1 & is_mapped_data == 1, unique(rep)]
  reporters_built <- m49_codes[reporter %in% reporters_built, ]
  rep2map_list <- reporters_built$reporter
  names(rep2map_list) <- reporters_built$country
  
  updatePickerInput(session = session,
                    inputId = "reporter_to_impute", 
                    choices = rep2map_list)
  
})


##-- Confirmation dialog box to impute missing data ----
observeEvent(input$run_imputation, {
  
  confirmSweetAlert(
    session = session,
    inputId = "run_imputation_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Imputation process could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

##-- Get data ----
get_data_to_impute <- eventReactive(input$run_imputation_confirmation, {
  
  validate(need(input$impute_years, "Please select a year."))
  validate(need(input$reporter_to_impute, "Please select a country."))
  
  years_imp    <- input$impute_years
  reporter_imp <- input$reporter_to_impute
  
  sql_filter <- sprintf("rep = '%s' AND year = '%s'", 
                        reporter_imp,  
                        years_imp)
  
  data2impute <- ReadDatatable(fishtrade_built_data, where = sql_filter)
  
  return(data2impute)
  
})

##-- Run imputation ----
observeEvent(input$run_imputation_confirmation, {
  
  if(input$run_imputation_confirmation == FALSE) return(NULL)
  
  validate(need(input$impute_years, "Please select a year."))
  validate(need(input$reporter_to_impute, "Please select a country."))
  
  progress_save_imputation <<- shiny::Progress$new()
  progress_save_imputation$set(message = "Running imputation procedures: ", value = 0)
  progress_save_imputation$inc(.3, detail = " Getting reporter data.")
  
  data2impute <- get_data_to_impute()
  
  if(nrow(data2impute) == 0) {
    
    progress_save_imputation$close()
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE, 
      title = "No data.",
      text = "There is no data for this combination of year and country.",
      type = "info"
    )  
    
    return(NULL)
  }
  
  progress_save_imputation$inc(.6, detail = " Running imputation process.")
  
  no_imputation <- ReadDatatable('fishtrade_hs_not_impute')$start
  
  out <- run_imputation(.reporter = input$reporter_to_impute, 
                        .no_imputation = no_imputation,
                        .data2impute = data2impute, 
                        .zero_masked_threshold = input$threshold_masked_zero/100)
  
  out$.data2impute[, imputation_method := factor(imputation_method)]
  values_imputation$data_imputed <- out$.data2impute
  # values_imputation$data_imputed$uv <- NULL
  
  #' save data into SWS
  progress_save_imputation$inc(.85, detail = " Saving data imputed.")
  query <- paste0('rep = ', input$reporter_to_impute, " AND ", "year = ", input$impute_years)
  writeDT(dt_name = fishtrade_data_imputed, data = values_imputation$data_imputed, sql_filter = query)
  
  
  file_name <- paste0("imputed", "_y",input$impute_years, "y_", "c",input$reporter_to_impute, "c", ".rds", collapse = "")
  file_path <- file.path("data", "output", "imputed", file_name)
  # saveRDS(values_imputation$data_imputed, file_path)
  
  ##-- Update reporter workflow status ----
  progress_save_imputation$inc(.95, detail = " Updating reporter status.")

  updateReporterStatus(.year = input$impute_years, 
                       .reporter = input$reporter_to_impute, 
                       .process = c("is_run_imputation", "is_imputed_data", "is_run_outlier", 
                                    "is_outlier_validated", "is_run_mirroring", "is_mirroring_validated"), 
                       .value = c(1, rep(0, 5)))
  
  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  
  progress_save_imputation$close()
  
  n_no_imputed_values <- values_imputation$data_imputed[imputation_method == "not_imputed", .N]
  
  if(n_no_imputed_values > 0) {
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE, 
      html = T,
      title = "Great!",
      text = tags$span(tags$h4("The imputation procedure has been finished successfuly."),
                       # tags$br(), 
                       tags$h4("There are", tags$b(n_no_imputed_values), "values to be imputed manually..")),
      type = "info"
    )  
  } else {
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE,
      html = TRUE,
      title = "Great!",
      text = tags$span(tags$h4("The imputation procedure has been finished successfuly."),
                       tags$h4("There are", tags$b("NO"), "values to be imputed manually.")),
      type = "success"
    ) 
  }
  
  
  
})

##-- Table: data imputed ----
output$tbl_imputed_data <- DT::renderDataTable(server = TRUE, {
  
  validate(
    need(input$run_imputation_confirmation, "")
  )
  
  datatable(values_imputation$data_imputed,
            rownames = FALSE,
            filter = 'top',
            options = list(dom = 'tip')) %>%
    formatCurrency("value", digits = 0) %>%
    formatRound(c("uv", "weight"), 2)
  
})











