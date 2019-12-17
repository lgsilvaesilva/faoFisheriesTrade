##-- Reactive Values ----
values_od <- reactiveValues(data_partner = data.table(),
                            data_full = data.table(),
                            outliers_detected = data.table())


observe({

  if(input$navbar != "outlier_detection") return(NULL)

  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  trade_years <- run_mapping_values$ft_workflow[is_built_data == 1 & 
                                                  is_run_mapping == 1 & 
                                                  is_mapped_data == 1 & 
                                                  is_run_imputation == 1 &
                                                  is_imputed_data == 1, unique(year)]

  updatePickerInput(session = session,
                    inputId = "od_years",
                    choices = trade_years)

})
 
observeEvent(c(input$od_years, input$navbar), {

  if(input$navbar != "outlier_detection") return(NULL)

  reporters_built <- run_mapping_values$ft_workflow[year == input$od_years & 
                                                      is_built_data == 1 & 
                                                      is_run_mapping == 1 & 
                                                      is_mapped_data == 1 & 
                                                      is_run_imputation == 1 & 
                                                      is_imputed_data == 1, 
                                                    unique(rep)]
  
  reporters_built <- m49_codes[reporter %in% reporters_built, ]
  rep2map_list <- reporters_built$reporter
  names(rep2map_list) <- reporters_built$country
  
  updatePickerInput(session = session,
                    inputId = "reporter_to_detect",
                    choices = rep2map_list)

})
##-- Button: Years ----
# output$btn_od_year <- renderUI({
# browser()
#   input$export_imputed_data_confirmation
#   input$tab_outlier
#   run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
#   trade_years <- run_mapping_values$ft_workflow[is_built_data == 1 & 
#                                                   is_run_mapping == 1 & 
#                                                   is_mapped_data == 1 & 
#                                                   is_run_imputation == 1 &
#                                                   is_imputed_data == 1, unique(year)]
#   
#   pickerInput(inputId = "od_years", 
#               multiple = FALSE,
#               label = "Year", 
#               choices = trade_years,
#               options = list(
#                 `actions-box` = TRUE)
#   )
#   
# })

##-- Button: reporter ----
# output$btn_od_reporter <- renderUI({
#   input$export_imputed_data_confirmation
#   input$tab_outlier  
#   reporters_built <- run_mapping_values$ft_workflow[year == input$od_years & 
#                                                       is_built_data == 1 & 
#                                                       is_run_mapping == 1 & 
#                                                       is_mapped_data == 1 & 
#                                                       is_run_imputation == 1 & 
#                                                       is_imputed_data == 1, 
#                                                     unique(rep)]
#   
#   reporters_built <- m49_codes[reporter %in% reporters_built, ]
#   rep2map_list <- reporters_built$reporter
#   names(rep2map_list) <- reporters_built$country
#   
#   pickerInput(inputId = "reporter_to_detect", 
#               label = "Reporter:", 
#               choices = rep2map_list,
#               options = list(`live-search` = TRUE))
# })

##-- Confirmation dialog box to run outlier ----
observeEvent(input$run_detection, {
  
  confirmSweetAlert(
    session = session,
    inputId = "run_outlier_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Outlier detection process could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

##-- Get data ----
# get_data <- eventReactive(input$run_outlier_confirmation, {
get_data <- reactive({  
  validate(need(input$od_years, "Please select a year."))
  validate(need(input$reporter_to_detect, "Please select a country."))
  validate(need(input$source_to_detect, "Please select a source."))
  
  years_od    <- input$od_years
  reporter_od <- input$reporter_to_detect
  input$run_outlier_confirmation
  get_historical_data(.year = years_od, 
                      .reporter = reporter_od, 
                      .reactive_values = values_od, 
                      .source = input$source_to_detect)
  
  # sql_filter <- sprintf("rep = '%s' AND year = '%s'", 
  #                       reporter_od,  
  #                       years_od)
  # 
  # data_to_detect <- ReadDatatable("fishtrade_data_imputed_validated", where = sql_filter)
  # 
  # sql_filter <- sprintf("rep = '%s'", reporter_od)
  # rep_trade <- ReadDatatable("fishtrade_trade_map", where = sql_filter)
  # 
  # ##** Mapping data to ISSCFC ----
  # data_to_detect <- map_trade(.data = data_to_detect, .map = rep_trade)
  # setnames(data_to_detect, c("rep"), c("reporter"))
  # data_to_detect[, reporter := stringr::str_pad(reporter, 3, "left", "0")]
  # 
  # ##** Remove ISSCFC discarded ----
  # data_to_detect <- data_to_detect[isscfc_code != -1, ]
  # 
  # ##-- Get Historical Data ----
  # sql_rep_hist <- sprintf("rep = '%s' AND year <> '%s' AND value_usd IS NOT NULL", stringr::str_pad(reporter_od, 3, "left", "0"), years_od)
  # rep_hist <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist)
  # setnames(rep_hist, c("rep", "remarks", "isscfc"), c("reporter", "remark", "isscfc_code"))
  # setkey(rep_hist,  reporter, flow, year)
  # rep_hist <- rep_hist[, list( year, reporter, flow, scheda, isscfc_code, tariff_line, weight = qty, value = value_usd)]
  # 
  # # sql_comm <- sprintf("(%s)", toString(sprintf("'%s'", data_to_detect[, unique(isscfc_code)])))
  # # sql_rep_hist_comm <- paste(sql_rep_hist, "AND isscfc IN", sql_comm)
  # # rep_hist2 <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist_comm)  
  # 
  # values_od$data_partner <- data_to_detect
  # 
  # data_to_detect <- data_to_detect[, list(year, reporter, flow, prt, scheda, isscfc_code, tariff_line = comm, weight, value)]
  # data_full <- rbindlist(l = list(rep_hist, data_to_detect), fill = TRUE, idcol = "case")
  # data_full[, 
  #           `:=`(value = as.numeric(value),
  #                weight = as.numeric(weight),
  #                year = as.numeric(year))]
  # 
  # values_od$data_full <- data_full
  
  
})

##-- Run outlier detection ----
observeEvent(input$run_outlier_confirmation, {
  
  if(input$run_outlier_confirmation == FALSE) return(NULL)
  
  validate(need(input$od_years, "Please select a year."))
  validate(need(input$reporter_to_detect, "Please select a country."))
  validate(need(input$source_to_detect, "Please select a source"))
  
  ##-- Progress bar 
  #' progress bar to be shown to user while the process is running
  progressSweetAlert(
    session = session, id = "outlier_detection_progress",
    title = "Anomaly detection in progress.",
    display_pct = TRUE,
    value = 0
  )
  
  ##** updating progress bar 30 ----
  updateProgressBar(
    title = "Data acquisition",
    session = session,
    id = "outlier_detection_progress",
    value = 30
  )
  ##---
  
  data_full <- get_data()
  
  if(nrow(data_full) == 0) {
    
    closeSweetAlert(session = session)
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE, 
      title = "No data.",
      text = "There is no data for this combination of year and country.",
      type = "info"
    )  
    
    return(NULL)
  }
  
  ##** updating progress bar 30 ----
  updateProgressBar(
    title = "Running anomaly detection",
    session = session,
    id = "outlier_detection_progress",
    value = 60
  )
  
  ##** Parameters ----
  reporter_selected <- stringr::str_pad(input$reporter_to_detect, width = "3", side = "left", pad = "0")
  year_selected <- input$od_years
  
  ##** Make levels to aggregate ----
  data_full[, `:=`(hs6 = substr(tariff_line, 1, 6),
                   hs4 = substr(tariff_line, 1, 4),
                   hs2 = substr(tariff_line, 1, 2))]
  
  data_full <- merge(data_full, fao_group_dt, by = c("isscfc_code"), all.x = T)
  data_full[, reporter := as.numeric(reporter)]
  
  ##** Run outlier detection ---- 
  out_uv <- try(run_outlier_detection(.data = data_full[weight != 0, ], .variable = "uv", .levels = "all", 
                                      .seq_len = input$par_n_sequential, 
                                      .logarithm = input$par_logarithm,
                                      .n_years = input$par_n_values, 
                                      .box_coef = input$par_coef_box))
  out_wt <- try(run_outlier_detection(.data = data_full, .variable = "weight", .levels = "all", 
                                      .seq_len = input$par_n_sequential, 
                                      .logarithm = input$par_logarithm,
                                      .n_years = input$par_n_values,
                                      .box_coef = input$par_coef_box))
  out_vl <- try(run_outlier_detection(.data = data_full, .variable = "value", .levels = "all", 
                                      .seq_len = input$par_n_sequential, 
                                      .logarithm = input$par_logarithm,
                                      .n_years = input$par_n_values,
                                      .box_coef = input$par_coef_box))

  if(class(out_uv) == "try-error" | class(out_wt) == "try-error" | class(out_vl) == "try-error") {
    
    closeSweetAlert(session = session)
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE, 
      title = "Error in outlier detection process.",
      text = out,
      type = "error"
    )  
    
    return(NULL)
    
  }
  
  out_uv_dt <- rbindlist(out_uv, idcol = "level_aggregation", fill = T)
  out_wt_dt <- rbindlist(out_wt, idcol = "level_aggregation", fill = T)
  out_vl_dt <- rbindlist(out_vl, idcol = "level_aggregation", fill = T)
  
  out_uv_detected <- tidy_outlier(out_uv_dt[(out_box == TRUE | out_ts == TRUE) & year == year_selected & reporter == as.numeric(reporter_selected), ], .variable = "uv")
  out_wt_detected <- tidy_outlier(out_wt_dt[(out_box == TRUE | out_ts == TRUE) & year == year_selected & reporter == as.numeric(reporter_selected), ], .variable = "weight")
  out_vl_detected <- tidy_outlier(out_vl_dt[(out_box == TRUE | out_ts == TRUE) & year == year_selected & reporter == as.numeric(reporter_selected), ], .variable = "value")
  
  ##** updating progress bar 30 ----
  updateProgressBar(
    title = "Saving outliers detected",
    session = session,
    id = "outlier_detection_progress",
    value = 80
  )
  
  data2sws <- rbind(out_uv_detected, out_wt_detected, out_vl_detected)
  
  # none outlier has been detected
  if(nrow(data2sws) == 0) {
    
    data_full[, c("weight_correction", "value_correction", 
                  "weight_method", "value_method", 
                  "level_agg", "criteria") := NA]
    
    #' save outliers fixed into SWS
    setnames(data_full, "reporter", "rep")
    columns_sws <- names(ReadDatatable(table = "fishtrade_outlier_validated", limit = 1))
    data_full <- data_full[, columns_sws, with = F]
    
    query <- sprintf("rep = '%s' OR rep = '%s' AND year = %s", 
                     as.numeric(reporter_selected), 
                     stringr::str_pad(reporter_selected, 3, "left", "0"),
                     year_selected)

    writeDT(data = data_full, 
            dt_name = "fishtrade_outlier_validated", 
            sql_filter = query)
    
    ##** Update reporter workflow status ----
    updateProgressBar(
      title = "Updating reporter status",
      session = session,
      id = "outlier_detection_progress",
      value = 95
    )
    
    updateReporterStatus(.year = year_selected, 
                         .reporter = as.numeric(reporter_selected), 
                         .process = c("is_run_outlier", "is_outlier_validated", 
                                      "is_run_mirroring", "is_mirroring_validated"), 
                         .value = c(1, 1, rep(0, 2)))
    
    updateProgressBar(
      title = "Updating reporter status",
      session = session,
      id = "outlier_detection_progress",
      value = 100
    )
    
    closeSweetAlert(session = session)
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE,
      html = TRUE,
      title = "Great!",
      text = tags$span(tags$h4("The anomaly detection has been finished successfuly."),
                       tags$h4("There are", tags$b("NO"), "values to be validated.")),
      type = "success"
    ) 
    
    return(NULL)
    
  }

  ##** Data correction ----
  data_fixed <- fix_outlier(.data_full = data_full, .out = data2sws, .box_coef = input$par_coef_box, .n_prt = input$par_n_values)
  data_fixed[, corrected := !is.na(criteria)]
  
  data_fixed[, `:=`(weight_correction = NA_real_, value_correction = NA_real_)]
  
  data_fixed[!is.na(weight_box_fix) | !is.na(weight_ts_fix), weight_correction := rowMeans(.SD, na.rm = TRUE), .SDcols = c("weight_box_fix", "weight_ts_fix")]
  data_fixed[!is.na(value_box_fix) | !is.na(value_ts_fix), value_correction := rowMeans(.SD, na.rm = TRUE), .SDcols = c("value_box_fix", "value_ts_fix")]
  
  data_fixed[, weight_method := case_when(is.na(weight_ts_fix) & !is.na(weight_box_fix) ~ "boxplot",
                                          !is.na(weight_ts_fix) & is.na(weight_box_fix) ~ "time_series",
                                          !is.na(weight_ts_fix) & !is.na(weight_box_fix) ~ "boxplot-time_series",
                                          TRUE ~ NA_character_)]
  
  data_fixed[, value_method := case_when(is.na(value_ts_fix) & !is.na(value_box_fix) ~ "boxplot",
                                         !is.na(value_ts_fix) & is.na(value_box_fix) ~ "time_series",
                                         !is.na(value_ts_fix) & !is.na(value_box_fix) ~ "boxplot-time_series",
                                         TRUE ~ NA_character_)]
  
  data_fixed[, c("weight_box_fix", "weight_ts_fix", "value_box_fix", "value_ts_fix") := NULL]
  
  df_code_fixed <- data_fixed[corrected == TRUE, c("tariff_line", "isscfc_code", paste0("hs", c(6, 4, 2)), "prt","fao_group", "flow"), with = F]
  df_code_fixed <- df_code_fixed[, lapply(.SD, as.character)]
  df_code_fixed$id <- 1:nrow(df_code_fixed)
  df_code_fixed <- melt(df_code_fixed, "id", variable.name = "level_aggregation", value.name = "code_aggregation")[, id := NULL]
  df_code_fixed <- df_code_fixed[, .N, by = c("level_aggregation", "code_aggregation")][, N := NULL]
  
  data2sws <- merge(df_code_fixed, data2sws, by = c("level_aggregation", "code_aggregation"))
  
  data2sws[!is.na(out_ts_correction) | !is.na(out_box_correction), correction := rowMeans(.SD, na.rm = TRUE), .SDcols = c("out_ts_correction", "out_box_correction")]
  data2sws[, method := case_when(is.na(out_ts_correction) & !is.na(out_box_correction) ~ "boxplot",
                                 !is.na(out_ts_correction) & is.na(out_box_correction) ~ "time_series",
                                 !is.na(out_ts_correction) & !is.na(out_box_correction) ~ "boxplot-time_series",
                                 TRUE ~ NA_character_)]
  
  data2sws[, c("out_ts_correction", "out_box_correction") := NULL]
  
  #' save outliers detected data into SWS
  setnames(data2sws, "reporter", "rep")
  data2sws[, rep := as.numeric(rep)]
  query <- sprintf("rep = '%s' OR rep = '%s' AND year = %s", 
                   as.numeric(reporter_selected), 
                   stringr::str_pad(reporter_selected, 3, "left", "0"),
                   year_selected)
  
  data2sws[, `:=`(checked = FALSE, validated = FALSE)]
  column_order <- names(ReadDatatable("fishtrade_outliers_detected", limit = 1))
  data2sws <- data2sws[, column_order, with = F]
  
  writeDT(dt_name = "fishtrade_outliers_detected", data = data2sws, sql_filter = query)
  
  values_od$outliers_detected <- data2sws
  
  updateProgressBar(
    title = "Saving outliers fixed",
    session = session,
    id = "outlier_detection_progress",
    value = 90
  )
  
  #' save outliers fixed into SWS
  setnames(data_fixed, "reporter", "rep")
  columns_sws <- names(ReadDatatable("fishtrade_outlier_corrected", limit = 1))
  
  data_fixed[, accepted := FALSE]
  data_fixed <- data_fixed[, columns_sws, with = F]
  
  writeDT(dt_name = "fishtrade_outlier_corrected", 
          data = data_fixed[year == year_selected, ], 
          sql_filter = query)
  
  
  ##** Update reporter workflow status ----
  updateProgressBar(
    title = "Updating reporter status",
    session = session,
    id = "outlier_detection_progress",
    value = 95
  )
  
  updateReporterStatus(.year = year_selected, 
                       .reporter = as.numeric(reporter_selected), 
                       .process = c("is_run_outlier", "is_outlier_validated", 
                                    "is_run_mirroring", "is_mirroring_validated"), 
                       .value = c(1, rep(0, 3)))
  
  updateProgressBar(
    title = "Updating reporter status",
    session = session,
    id = "outlier_detection_progress",
    value = 100
  )
  
  closeSweetAlert(session = session)
  
  n_outliers <- nrow(data2sws)
  
  if(n_outliers > 0) {
    
    sendSweetAlert(
      session = session,
      closeOnClickOutside = FALSE,
      type = "info",
      text = tags$span(tags$h4("The imputation procedure has been finished successfuly."), tags$h4("There are", tags$b(n_outliers), "values to be validated.")),
      title = "Great!",
      html = T
      
    )
    
  } else {
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE,
      html = TRUE,
      title = "Great!",
      text = tags$span(tags$h4("The anomaly detection has been finished successfuly."),
                       tags$h4("There are", tags$b("NO"), "values to be validated.")),
      type = "success"
    ) 
    
  }
  
  
})











