##--+ Button: Years ----
observe({

  if(input$navbar != "outlier_detection") return(NULL)

  input$run_outlier_detection
  input$tab_outlier

  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  years_detected <- run_mapping_values$ft_workflow[is_built_data == 1 &
                                                     is_run_mapping == 1 &
                                                     is_mapped_data == 1 &
                                                     is_run_imputation == 1 &
                                                     is_imputed_data == 1 &
                                                     is_run_outlier == 1, unique(year)]
  updatePickerInput(session = session,
                    inputId = "detected_years",
                    choices = sort(years_detected, decreasing = T)
  )

})
# output$btn_year_detected <- renderUI({
# 
#   input$run_outlier_detection
#   input$tab_outlier
#   run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
#   years_detected <- run_mapping_values$ft_workflow[is_built_data == 1 &
#                                                      is_run_mapping == 1 &
#                                                      is_mapped_data == 1 &
#                                                      is_run_imputation == 1 &
#                                                      is_imputed_data == 1 &
#                                                      is_run_outlier == 1, unique(year)]
# 
#   pickerInput(inputId = "detected_years",
#               multiple = FALSE,
#               label = "Year",
#               selected = NULL,
#               choices = sort(years_detected, decreasing = T),
#               options = list(`actions-box` = TRUE)
#   )
# 
# })

##--+ Button: reporter ----
observe({

  if(input$navbar != "outlier_detection") return(NULL)

  input$run_outlier_detection
  input$tab_outlier

  reporters_detected <- run_mapping_values$ft_workflow[year == input$detected_years &
                                                         is_run_imputation == 1 &
                                                         is_imputed_data == 1 &
                                                         is_run_outlier == 1 &
                                                         is_outlier_validated == 0,
                                                       unique(rep)]
  reporters_detected <- m49_codes[reporter %in% reporters_detected, ]
  rep2impute_list <- reporters_detected$reporter
  names(rep2impute_list) <- reporters_detected$country

  updatePickerInput(session = session,
                    inputId = "reporter_detected",
                    choices = rep2impute_list)

})
# output$btn_reporter_detected <- renderUI({
# 
#   input$run_outlier_detection
#   input$tab_outlier
# 
#   reporters_detected <- run_mapping_values$ft_workflow[year == input$detected_years &
#                                                          is_run_imputation == 1 &
#                                                          is_imputed_data == 1 &
#                                                          is_run_outlier == 1 &
#                                                          is_outlier_validated == 0,
#                                                        unique(rep)]
#   reporters_detected <- m49_codes[reporter %in% reporters_detected, ]
#   rep2impute_list <- reporters_detected$reporter
#   names(rep2impute_list) <- reporters_detected$country
# 
#   pickerInput(inputId = "reporter_detected",
#               label = "Reporter:",
#               selected = NULL,
#               choices = rep2impute_list,
#               options = list(`live-search` = TRUE))
# 
# })

##--+ Table: outliers ----
output$tbl_outliers_detected <- renderDataTable({
  
  validate(need(input$detected_years, "Please select a year."))
  validate(need(input$reporter_detected, "Please select a country."))

  if(input$navbar != "outlier_detection") return(NULL)

  input$tab_outlier

  query <- sprintf("rep = '%s' AND year = %s", input$reporter_detected, input$detected_years)
  df <- ReadDatatable("fishtrade_outliers_detected", where = query)[order(criteria, level_aggregation), ]

  input$checked_outlier
  input$validated_outlier

  df[, `:=`(flow = as.factor(flow),
            level_aggregation = as.factor(level_aggregation),
            criteria = as.factor(criteria),
            code_aggregation = as.factor(code_aggregation),
            method = as.factor(method))]

  df[, row_colors := case_when(validated == TRUE ~ "validated",
                               level_aggregation %in% c("tariff_line", "isscfc_code", "hs6") & criteria == "uv" ~ "checked",
                               TRUE ~ "todo")]

  n_rows <- ifelse(is.null(isolate(input$tbl_outliers_detected_rows_current)), 10, max(isolate(input$tbl_outliers_detected_rows_current)))

  df[, level_aggregation := factor(level_aggregation, levels = c("tariff_line", "isscfc_code", paste0("hs", c(6, 4, 2)), "prt","fao_group", "flow"), ordered = T)]
  df[, criteria := factor(criteria, levels = c("uv", "value", "weight"), ordered = T)]

  df <- df[order(criteria, level_aggregation), ]

  values_od$outliers_detected <- df
  
  datatable(values_od$outliers_detected, 
            colnames = c("Year", "Reporter", "Flow", "Level Agg.", "Code Agg.", "Criteria", "Value", 
                         "Correction", "Method", "Checked", "Validated", "row_colors"),
            rownames = FALSE, 
            filter = 'top',
            selection = list(mode = "single", target = "cell"),
            extensions = 'Buttons', 
            options = list(#dom = 'Bfrtip', 
              buttons = I('colvis'),
              pageLength = n_rows,
              columnDefs = list(list(visible = FALSE, targets = 11)))) %>%
    formatRound(columns = c("value", "correction"), digits = 2) %>%
    formatStyle(1, target = 'row', valueColumns = "row_colors",
                backgroundColor = styleEqual(c("validated", "checked", "todo"), c("#C9E879", "#FFE074", "#C4C4C4")))
  
  
})
##-- Check Outlier ----
observeEvent(input$checked_outlier, {
  
  if(nrow(input$tbl_outliers_detected_cells_selected) == 0) {
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE, 
      title = "Error",
      text = "Please select one outlier checked.",
      type = "warning"
    ) 
    
    return(NULL)
    
  }
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  
  df <- values_od$outliers_detected
  df[outlier_selected[1, 1], checked := ifelse(checked == FALSE, TRUE, FALSE)]
  values_od$outliers_detected <- df
  
  query <- sprintf("rep = '%s' AND year = %s", input$reporter_detected, input$detected_years)
  writeDT(data = df, dt_name = "fishtrade_outliers_detected", sql_filter = query)
  
})

##-- Validate Outlier ----
observeEvent(input$validated_outlier, {
  
  if(nrow(input$tbl_outliers_detected_cells_selected) == 0) {
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE, 
      title = "Error",
      text = "Please select one outlier validated.",
      type = "warning"
    ) 
    
    return(NULL)
    
  }
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  
  df <- values_od$outliers_detected
  df[outlier_selected[1, 1], validated := ifelse(validated == FALSE, TRUE, FALSE)]
  
  values_od$outliers_detected <- df
  
  query <- sprintf("rep = '%s' AND year = %s", input$reporter_detected, input$detected_years)
  writeDT(data = df, dt_name = "fishtrade_outliers_detected", sql_filter = query)
  
})

##--+ Get Full Data ----
get_full_data <- reactive({
  
  # })
  
  # observeEvent(c(input$detected_years, input$reporter_detected), priority = 1, {
  
  validate(need(input$detected_years, "Please select a year."))
  validate(need(input$reporter_detected, "Please select a country."))
  # validate(need(input$tbl_outliers_detected_cells_selected, "Please select an outlier."))
  
  years_od    <- input$detected_years
  reporter_od <- input$reporter_detected
  
  ##-- Get the data corrected for the current year
  sql_data_fixed <- sprintf("rep = '%s' OR rep = '%s' AND year = %s",
                            reporter_od,
                            stringr::str_pad(input$reporter_detected, 3, "left", "0"), years_od)
  data_fixed <- ReadDatatable("fishtrade_outlier_corrected", where = sql_data_fixed)
  
  data_fixed[, c("isscfc_code", "isscfc_id", "scheda") := NULL]
  setnames(data_fixed, c("tariff_line"), c("comm"))
  
  data_fixed[, rep := as.numeric(rep)]
  
  mapping_table <- ReadDatatable(fishtrade_trade_map, where = paste0("rep = ", reporter_od))
  data_fixed <- map_trade(.data = data_fixed, mapping_table)
  
  setnames(data_fixed, c("rep", "prt", "comm"), c("reporter", "partner", "tariff_line"))
  
  ##** Get the historical data ----
  sql_rep_hist <- sprintf("rep = '%s' OR rep = '%s' AND year <> '%s' AND value_usd IS NOT NULL",
                          reporter_od,
                          stringr::str_pad(reporter_od, 3, "left", "0"), years_od)
  
  rep_hist <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist)
  rep_hist[, rep := as.numeric(rep)]
  setnames(rep_hist, "isscfc", "isscfc_code")
  
  mapping_table <- unique(mapping_table[isscfc_code != -1, list(isscfc_code, isscfc_id, isscfc_descr)])
  mapping_table <- mapping_table[, list(isscfc_descr = isscfc_descr[!is.na(isscfc_descr)]), by = c("isscfc_code", "isscfc_id")]
  mapping_table <- mapping_table[, list(isscfc_descr = isscfc_descr[which.max(nchar(isscfc_descr))]), by = c("isscfc_code", "isscfc_id")]
  rep_hist <- merge(rep_hist, mapping_table, by = c("isscfc_code", "isscfc_id"), all.x = T)
  
  rep_hist[, value := NULL]
  setnames(rep_hist, c("rep", "remarks", "qty", "value_usd"), c("reporter", "remark", "weight", "value"))
  setkey(rep_hist,  reporter, flow, year)
  
  rep_hist[, c("qunit", "qty_mt", "qty_flag", "value_unit", "value_flag") := NULL]
  
  rep_hist <- rep_hist[, list( year, reporter, flow, scheda, isscfc_code, tariff_line, tariff_line_descr, isscfc_descr, weight, value)]
  
  data_full <- rbindlist(l = list(rep_hist, data_fixed), fill = TRUE, idcol = "case")
  data_full[is.na(accepted), accepted := FALSE]
  
  values_od$data_fixed_hist <- data_full
  
})

##--+ Updates Buttons ----
observeEvent(c(input$tbl_outliers_detected_cells_selected), {
  
  validate(need(input$tbl_outliers_detected_cells_selected, "Please select a outlier."))
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  ##** Aggregation button
  updatePickerInput(session = session, 
                    inputId = "aggregation_level_viz", 
                    selected = as.character(outlier_selected$level_aggregation))
  
  ##** Criteria button
  updateRadioGroupButtons(session = session, 
                          inputId = "criteria_viz",
                          selected = as.character(outlier_selected$criteria))
  
})

##--+ Compute aggregation ----
observeEvent(c(input$tbl_outliers_detected_cells_selected), {
  
  validate(need(input$tbl_outliers_detected_cells_selected, "Please select a outlier."))
  # validate(need(values_od$data_fixed_hist, "Please select a outlier."))
  # input$save_outlier_correction_confirm
  get_full_data()
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  level_agg     <- ifelse(outlier_selected$level_aggregation == "isscfc", "isscfc_code", outlier_selected$level_aggregation)
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- stringr::str_pad(outlier_selected$rep, 3, "left", "0")
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  data_plot <- values_od$data_fixed_hist[flow == flow_sel & reporter == as.numeric(reporter_sel), ]
  data_plot[, case := NULL]
  
  ##** Make levels to aggregate ----
  data_plot[, `:=`(hs6 = substr(tariff_line, 1, 6),
                   hs4 = substr(tariff_line, 1, 4),
                   hs2 = substr(tariff_line, 1, 2))]
  
  data_plot <- merge(data_plot, fao_group_dt, by = c("isscfc_code"), all.x = T) 
  data_plot[, c("fao_group.x", "isscfc_id.x") := NULL]
  setnames(data_plot, c("fao_group.y", "isscfc_id.y"), c("fao_group", "isscfc_id"))
  
  ##-- Copy old values
  data_plot[is.na(weight_correction), `:=`(weight_correction = weight, weight_method = "copy")]
  data_plot[is.na(value_correction),  `:=`(value_correction = as.numeric(value), value_method = "copy")]
  
  ##** Compute agregation from root to leaf levels ----
  data_plot[, `:=`(uv = as.numeric(value)/as.numeric(weight), 
                   uv_correction = value_correction/weight_correction)]

  data_plot[, partner := as.integer(partner)]
  data_plot <- merge(data_plot, m49_codes[, list(Partner = country, reporter)], 
                     by.x = "partner", by.y = "reporter", all.x = TRUE)
  
  values_od$data_plot <- data_plot
  
})

##--+ Chart: time series ----
output$gg_ts_outlier <- renderPlotly({
  
  validate(need(input$detected_years, "Please select a year."))
  validate(need(input$reporter_detected, "Please select a country."))
  validate(need(input$tbl_outliers_detected_cells_selected, "Please select a outlier."))
  
  input$save_outlier_correction_confirm
  
  years_od    <- input$detected_years
  reporter_od <- input$reporter_detected
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  level_agg     <- outlier_selected$level_aggregation
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- outlier_selected$rep
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  data_plot <- values_od$data_plot
  
  ## Variable to be used in the melt function
  id_vars <- c('partner', 'isscfc_code', 'year', 'reporter', 'flow', 'scheda', 'tariff_line', 
               'weight_method', 'value_method', 'level_agg', 'criteria', 'hs6', 'hs4', 'hs2', 
               'isscfc_id', 'fao_group', 'Partner')
  
  ## Convert variable to numeric to avoid warnings in the melt function
  data_plot[, `:=`(weight = as.numeric(weight),
                   value = as.numeric(value),
                   uv = as.numeric(uv),
                   weight_correction = as.numeric(weight_correction),
                   value_correction = as.numeric(value_correction),
                   uv_correction = as.numeric(uv_correction))]
  
  ts_plot <- data_plot[!is.na(get(input$aggregation_level_viz)), ]
  
  ## Aggregate at the level which the outlier was detected
  ## Filter only level which the outlier was detected
  filter_code_agg <- sprintf("%s == '%s'", level_agg, code_agg)
  ts_plot <- ts_plot[eval(parse(text = filter_code_agg)), 
                     lapply(.SD, sum, na.rm = T), .SDcols = c("value", "weight", "value_correction", "weight_correction"), 
                     by = c("reporter", "flow", "year", input$aggregation_level_viz)]
  
  ## Compute unit value
  ts_plot[, `:=`(uv = value/weight, uv_correction = value_correction/weight_correction)]
  
  ts_plot <- melt(ts_plot, id.vars = 1:4, value.name = "value")
  ## Filter only the variable which the outlier was detected
  ts_plot <- ts_plot[grepl(input$criteria_viz, variable), ]
  
  ## Format the variable names
  ts_plot[, variable := gsub("_correction", " - Proposal", ts_plot$variable)]
  ts_plot[variable == input$criteria_viz, variable := paste0(input$criteria_viz, " - Original")]
  ts_plot[, variable := stringr::str_to_title(variable)]
  ts_plot[, variable := gsub("Uv", "UV", ts_plot$variable)]
  ts_plot <- ts_plot[order(year)]
  
  ## Reshape the data
  ts_plot <- dcast(ts_plot, reporter+flow+year+get(input$aggregation_level_viz)~variable)
  ts_plot[, input := as.factor(input)]
  
  yaxis_title <- ifelse(input$criteria_viz == "uv", toupper(input$criteria_viz), stringr::str_to_title(input$criteria_viz))
  
  if(uniqueN(ts_plot$input) < 3) {
    p <- plot_ly(ts_plot, x = ~year, y = ~get(paste0(yaxis_title," - Original")), 
                 color = ~input, 
                 colors = RColorBrewer::brewer.pal(3, "Set2"),
                 type = 'scatter',
                 # marker = list(size = 10),
                 source = "ts_outlier", 
                 mode = 'lines+markers')  
  } else {
    p <- plot_ly(ts_plot, x = ~year, y = ~get(paste0(yaxis_title," - Original")), 
                 color = ~input, 
                 type = 'scatter',
                 # marker = list(size = 10),
                 source = "ts_outlier", 
                 mode = 'lines+markers')  
  }
  
  p <- p %>%
    add_trace(data = ts_plot[year == year_sel], x = ~year, y = ~get(paste0(yaxis_title," - Proposal")), 
              color=~input, colors = c("#132B43", "#56B1F7"), 
              marker = list(symbol = 'cross', size = 8),
              name = "Correction",
              showlegend = TRUE, 
              mode = "markers") %>%
    # add_trace(data = ts_plot[year == year_sel], x = ~year, y = ~get(paste0(yaxis_title," - Time Series")),
    #           color=~input, 
    #           marker = list(symbol = 'square', size = 8),
    #           name = "Time Series",
    #           showlegend = FALSE,
    #           mode = "markers") %>%
    layout(yaxis = list(title = yaxis_title),
           xaxis = list(title = "Year"),
           showlegend = TRUE)
  
  p$elementId <- NULL
  p
  
})

##--+ Chart: partners correction ----
output$gg_prt_correction <- renderPlotly({
  
  validate(need(input$detected_years, "Please select a year."))
  validate(need(input$reporter_detected, "Please select a country."))
  validate(need(input$tbl_outliers_detected_cells_selected, "Please select a outlier."))
  
  input$save_outlier_correction_confirm
  
  years_od    <- input$detected_years
  reporter_od <- input$reporter_detected
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  level_agg     <- outlier_selected$level_aggregation
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- outlier_selected$rep
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  data_plot <- copy(values_od$data_plot)
  filter_code_agg <- sprintf("%s == '%s'", level_agg, code_agg)
  
  prt_dt <- data_plot[eval(parse(text = filter_code_agg)), ]
  prt_dt <- prt_dt[!is.na(partner) & !is.na(criteria), .SD, 
                   .SDcols = c("reporter", "flow", "Partner", as.character(level_agg), 
                               input$criteria_viz, paste0(input$criteria_viz, "_correction"))]
  prt_dt <- melt(prt_dt, 1:4)
  prt_dt[, variable := gsub("_correction", " - Correction", variable)]
  prt_dt[variable == input$criteria_viz, variable := paste0(input$criteria_viz, " - Original")]
  prt_dt[, variable := stringr::str_to_title(variable)]
  prt_dt[, variable := gsub("Uv", "UV", variable)]
  prt_dt[, variable := gsub(" - ", " <br> ", variable)]
  var_order <- unique(prt_dt$variable)
  
  prt_dt[, variable := factor(variable, levels = var_order)]
  prt_dt[, value := as.numeric(value)]
  
  prt_dt <- prt_dt[, list(value = sum(value)), by = c("reporter", "flow", "Partner", as.character(level_agg), "variable")]
  
  yaxis_title <- ifelse(input$criteria_viz == "uv", toupper(input$criteria_viz), stringr::str_to_title(input$criteria_viz))
  
  if(uniqueN(prt_dt$Partner) < 3) {
    p <- plot_ly(prt_dt, x = ~variable, y = ~value, 
                 color = ~Partner,
                 colors = RColorBrewer::brewer.pal(3, "Set2"),
                 type = 'scatter',
                 source = "ts_imputed", 
                 mode = 'lines+markers')  
  } else {
    p <- plot_ly(prt_dt, x = ~variable, y = ~value, 
                 color = ~Partner,
                 type = 'scatter',
                 source = "ts_imputed", 
                 mode = 'lines+markers')  
  }
  
  p <- p %>%
    layout(yaxis = list(title = yaxis_title),
           xaxis = list(title = "Correction"),
           showlegend = TRUE)
  
  p$elementId <- NULL
  p
  # suppressMessages(suppressWarnings(print(p)))
  
})

##--+ Table: partners ----
##** Filter Partner data ----
observe({
  validate(need(input$detected_years, "Please select a year."))
  validate(need(input$reporter_detected, "Please select a country."))
  validate(need(input$tbl_outliers_detected_cells_selected, "Please select a outlier."))
  validate(need(values_od$data_plot, "Please select a outlier."))
  
  years_od    <- input$detected_years
  reporter_od <- input$reporter_detected
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  level_agg     <- outlier_selected$level_aggregation
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- outlier_selected$rep
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  data_plot <- copy(values_od$data_plot)
  filter_code_agg <- sprintf("%s == '%s'", level_agg, code_agg)
  
  prt_dt <- data_plot[eval(parse(text = filter_code_agg)), ]
  prt_dt[, rowpos := .I] 
  method_correction <- prt_dt[, method_correction := toString(.SD), .SDcols = c("value_method", "weight_method"), by = rowpos]
  
  prt_dt <- prt_dt[, list(year, reporter, flow, Partner, 
                          partner,
                          fao_group, scheda, 
                          tariff_line, tariff_line_descr, 
                          isscfc_id, isscfc_code, isscfc_descr,
                          level_agg, criteria, 
                          value, weight, uv, value_correction, weight_correction, uv_correction, 
                          method_correction, accepted)]
  
  prt_dt[, value := as.numeric(value)]
  prt_dt <- prt_dt[order(-year), ]
  values_od$prt_dt <- prt_dt
  
})
##** Unit Value ----
observeEvent(input$tblPartnerOutlier, {
  
  if(is.null(input$tblPartnerOutlier)) return(NULL)
  if(is.null(input$tblPartnerOutlier$changes$changes)) return(NULL)
  if(!input$tblPartnerOutlier$changes$changes[[1]][[2]] %in% c(17, 18, 19)) return(NULL)
  
  value_old <- input$tblPartnerOutlier$changes$changes[[1]][[3]]
  value_new <- input$tblPartnerOutlier$changes$changes[[1]][[4]]
  idx_row   <- sapply(input$tblPartnerOutlier$changes$changes, "[[", 1) + 1
  idx_col   <- input$tblPartnerOutlier$changes$changes[[1]][[2]]
  
  if(value_new == "") return(NULL)
  
  value_old <- ifelse(is.null(value_old), 0, value_old)
  
  if(!is.null(value_old) & !is.null(value_new)) {
    if(value_old == value_new) return(NULL)
  } 
  
  if(idx_col %in% 17:18) {
    tblPartnerOutlier <- hot_to_r(input$tblPartnerOutlier)
    tblPartnerOutlier$uv_correction <- tblPartnerOutlier$value_correction / tblPartnerOutlier$weight_correction
    tblPartnerOutlier[idx_row, method_correction := "manual_correction, manual_correction"]
    tblPartnerOutlier[idx_row, accepted := TRUE]
    values_od$prt_dt <- tblPartnerOutlier  
  }
  
  if(idx_col == 19) {
    tblPartnerOutlier <- hot_to_r(input$tblPartnerOutlier)
    tblPartnerOutlier$weight_correction <- tblPartnerOutlier$value_correction / tblPartnerOutlier$uv_correction
    tblPartnerOutlier[idx_row, method_correction := "manual_correction, manual_correction"]
    tblPartnerOutlier[idx_row, accepted := TRUE]
    values_od$prt_dt <- tblPartnerOutlier
  }
  
})

##** Output Table ----
output$tblPartnerOutlier <- renderRHandsontable({
  
  validate(need(input$detected_years, "Please select a year."))
  validate(need(input$reporter_detected, "Please select a country."))
  validate(need(input$tbl_outliers_detected_cells_selected, "Please select an outlier 1."))
  validate(need(values_od$prt_dt, "Please select an outlier 2."))
  
  row_index <- which(!is.na(values_od$prt_dt$level_agg)) - 1
  n_col <- ncol(values_od$prt_dt) - 1
  
  rhandsontable(values_od$prt_dt, 
                row_highlight = row_index, 
                col_highlight = NULL) %>%
    hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE) %>%
    hot_col(col = c(1:17, 21), readOnly = T) %>%
    hot_col(col = c("Value" = "value"), readOnly = T) %>%
    hot_col(col = c('value_correction'), readOnly = F, format = "$0,0.0") %>%
    hot_col(col = c('weight_correction'), readOnly = F, format = "0,0") %>%
    hot_col(col = 1:21, renderer = render_highlight_text) %>%
    hot_col(col = 22, renderer = render_highlight_bool, halign = "htCenter") %>%
    hot_row(row = which(values_od$prt_dt$year != input$detected_years), readOnly = TRUE) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
  
})
##--+ Data Aggregation ----
observeEvent(input$run_outlier_aggregation, {
  
  allowed_groups    <- c("year", "Partner", "hs6", "hs4", "hs2", "partner", "isscfc_code", "year", "reporter", "flow", "scheda", "tariff_line")
  allowed_functions <- c("sum", "mean", "median", "sd", "var", "min", "max")
  allowed_values    <- c("weight", "value", "weight_ts_fix", "value_ts_fix", "weight_box_fix", "weight_ts_fix")
  
  # validate(need(input$group_agg, "Select a group"),
  #          need(input$function_agg, "Select a function"),
  #          need(input$value_agg, "Select a value"))
  
  .group_by     <- input$group_agg
  .function_agg <- input$function_agg
  .value_vars   <- input$value_agg
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  level_agg     <- outlier_selected$level_aggregation
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- outlier_selected$rep
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  data_plot <- copy(values_od$data_plot)
  data_plot[, value := as.numeric(value)]
  data_plot[, weight := as.numeric(weight)]
  data_plot[, uv := as.numeric(uv)]
  data_plot[, value_correction := as.numeric(value_correction)]
  data_plot[, weight_correction := as.numeric(weight_correction)]
  data_plot[, uv_correction := as.numeric(uv_correction)]
  
  filter_code_agg <- sprintf("%s == '%s'", level_agg, code_agg)
  
  prt_dt <- data_plot[eval(parse(text = filter_code_agg)), ]
  prt_dt <- prt_dt %>%
    select(year, reporter, flow, Partner, partner, fao_group, scheda,
           hs2, hs4, hs6, tariff_line, isscfc_id, isscfc_code, level_agg, criteria,
           starts_with("weight"), starts_with("value"), uv)
  
  count_fun <- NULL
  if(any(grepl("N",.function_agg))) {
    count_fun <- "Freq = .N"
    .function_agg <- .function_agg[!grepl("N",.function_agg)]
  }
  
  uv_fun <- NULL
  if(any(grepl("uv",.function_agg))) {
    uv_fun <- "uv = value / weight"
    .function_agg <- .function_agg[!grepl("uv", .function_agg)]
  }
  
  if(length(.function_agg) > 0) {
    agg_instruction <- expand.grid(values_var = .value_vars, function_var = .function_agg)
    agg_instruction <- agg_instruction[order(agg_instruction$values_var), ]
    agg_function <- toString(c(sprintf("%s = %s(%s, na.rm = TRUE)", paste0(agg_instruction$function_var, "_", agg_instruction$values_var),
                                       agg_instruction$function_var, agg_instruction$values_var),
                               count_fun))
    
    agg_function <- sprintf("list(%s)", agg_function)
    out_dt <- prt_dt[, eval(parse(text = agg_function)), by = .group_by]
  }
  
  if(!is.null(uv_fun)) {
    uv_dt <- prt_dt[, list(value = sum(value, na.rm = T), 
                           weight = sum(weight, na.rm = T)), 
                    by = .group_by][, uv := value / weight]
    
    uv_dt[, `:=`(value = NULL, weight = NULL)]
    
    if(!exists("out_dt")) {
      
      out_dt <- uv_dt
      
    } else {
      
      out_dt <- merge(out_dt, uv_dt, by = .group_by)
      
    }
    
    
  }
  
  values_od$data_agg <- out_dt
  
  
})

##** Table: data aggregated ----
output$tblAggregation <- renderRHandsontable({
  
  validate(need(input$group_agg, "Select a group"),
           need(input$function_agg, "Select a function"),
           # need(input$value_agg, "Select a value"),
           need(values_od$data_agg, ""))
  
  if(is.null(input$run_outlier_aggregation) | input$run_outlier_aggregation == 0) return(NULL)
  
  
  df <- values_od$data_agg
  
  rhandsontable(data = df) %>%
    hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    # hot_cols(columnSorting = TRUE) %>%
    hot_col(col = 1:ncol(df), readOnly = T) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

##--+ Save Data ----
##** Launch a popup to ask confirmation to the user
observeEvent(input$save_outlier_correction, {
  
  fao_confirmSweetAlert(
    session = session,
    inputId = "save_outlier_correction_confirm",
    type = "warning",
    text = tags$span(tags$h3("Could you please confirm it?")),
    title = "Save the data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})


observeEvent(input$save_outlier_correction_confirm, {
  
  if(input$save_outlier_correction_confirm == FALSE) return(NULL)
  
  ##** Progress bar starts 
  progressSweetAlert(
    session = session, id = "save_outlier_correction_progress",
    title = "Save in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "save_outlier_correction_progress",
    value = 15
  )
  
  outlier_selected <- input$tbl_outliers_detected_cells_selected
  
  outlier_selected <- values_od$outliers_detected[outlier_selected[1, 1], ]
  
  level_agg     <- outlier_selected$level_aggregation
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- outlier_selected$rep
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  ##** updating progress bar 45
  updateProgressBar(
    session = session,
    id = "save_outlier_correction_progress",
    value = 45
  )
  
  ## Get data changed by user
  data_to_upload <- hot_to_r(input$tblPartnerOutlier)
  
  ## update data in cache
  values_od$prt_dt <- copy(data_to_upload)
  
  ## Convert to datatable
  setDT(data_to_upload)
  
  ## Split column "method"
  data_to_upload[, c("value_method", "weight_method") := tstrsplit(method_correction, ",")][, method_correction := NULL]
  
  ## Update data live
  filter_code_agg <- sprintf("%s != '%s'", level_agg, code_agg)
  df_old <- values_od$data_plot[eval(parse(text = filter_code_agg)), ]
  df_updated <- copy(data_to_upload)
  df_updated[, `:=`(hs2 = substr(tariff_line, 1, 2),
                    hs4 = substr(tariff_line, 1, 4),
                    hs6 = substr(tariff_line, 1, 6))]
  
  ## Class variable
  setnames(data_to_upload, c("reporter", "partner"), c("rep", "prt"))
  data_to_upload[, `:=`(year = as.integer(year),
                        prt = as.character(prt))]
  
  data_to_upload <- data_to_upload[year == year_sel & flow == flow_sel, ]
  
  ##** updating progress bar 45
  updateProgressBar(
    session = session,
    id = "save_outlier_correction_progress",
    value = 75
  )
  
  ## Get data stored into SWS 
  query <- sprintf("(rep = '%s' OR rep = '%s') AND year = %s AND flow = %s", 
                   reporter_sel,
                   stringr::str_pad(reporter_sel, 3, "left", "0"), 
                   year_sel,
                   flow_sel)
  
  r_query <- sprintf("(reporter == '%s' | reporter == '%s') & year == %s & flow == %s", 
                     reporter_sel,
                     stringr::str_pad(reporter_sel, 3, "left", "0"), 
                     year_sel,
                     flow_sel)
  
  
  ## SQL query to filter only the levels affected
  sql_level <- NULL
  
  if(level_agg %in% c("hs2", "hs4", "hs6")) {
    
    sql_level <- sprintf("SUBSTRING(tariff_line, 1, %s) = '%s'", substr(level_agg, 3, 3), code_agg)
    r_sql_level <- sprintf("substr(tariff_line, 1, %s) = '%s'", substr(level_agg, 3, 3), code_agg)
    
  }
  
  if(level_agg %in% c("prt", "isscfc", "fao_group", "tariff_line")) {
    
    sql_level <- sprintf("%s = '%s'", level_agg, code_agg)
    r_sql_level <- sprintf("%s == '%s'", level_agg, code_agg)
    
  }
  
  if(!is.null(sql_level)) {
    query <- paste(query, sql_level, sep = " AND ")
    r_query <- paste(r_query, r_sql_level, sep = " & ")
  }
  
  ##** updating progress bar 45
  updateProgressBar(
    session = session,
    id = "save_outlier_correction_progress",
    value = 95
  )
  
  ## retrieve the imputation method
  data_to_upload[, year := as.character(year)]
  data_to_upload <- merge(data_to_upload, 
                          values_od$data_fixed_hist[, list(year, reporter, flow, tariff_line, partner, imputation_method)],
                          by.x = c("year", "rep", "flow", "tariff_line", "prt"),
                          by.y = c("year", "reporter", "flow", "tariff_line", "partner"))
  
  order_cols <- names(ReadDatatable("fishtrade_outlier_corrected", limit = 1))
  writeDT(dt_name = "fishtrade_outlier_corrected", data = data_to_upload[, order_cols, with = F], sql_filter = query)
  
  data_hist <- copy(values_od$data_fixed_hist)
  
  data_to_upload <- data_to_upload[, c("year", "rep", "flow", "prt", "tariff_line", "weight_correction", "value_correction", "weight_method", "value_method", "accepted"), with = F]
  data_to_upload$new <- TRUE
  
  data_hist <- merge(data_hist, data_to_upload, 
                     by.x = c("year", "reporter", "flow", "tariff_line", "partner"),
                     by.y = c("year", "rep", "flow", "tariff_line", "prt"),
                     all.x = TRUE)
  
  currentVar  <- grep("\\.x$", names(data_hist), value = T)
  proposalVar <- grep("\\.y$", names(data_hist), value = T)
  
  updateVar <- sprintf("`:=`(%s)", toString(sprintf("%s = %s", currentVar, proposalVar)))
  data_hist[new == TRUE, eval(parse(text = updateVar))]
  
  data_hist <- data_hist %>%
    dplyr::select(-c(proposalVar, "new"))
  
  setnames(data_hist, currentVar, gsub("\\.x", "", currentVar))
  
  values_od$data_fixed_hist <- data_hist
  
  ##--updating data to show in the charts
  level_agg     <- ifelse(outlier_selected$level_aggregation == "isscfc", "isscfc_code", outlier_selected$level_aggregation)
  code_agg      <- outlier_selected$code_aggregation
  criteria_sel  <- outlier_selected$criteria
  reporter_sel  <- stringr::str_pad(outlier_selected$rep, 3, "left", "0")
  flow_sel      <- outlier_selected$flow
  year_sel      <- outlier_selected$year
  
  data_plot <- values_od$data_fixed_hist[flow == flow_sel & reporter == as.numeric(reporter_sel), ]
  data_plot[, case := NULL]
  
  ##** Make levels to aggregate ----
  data_plot[, `:=`(hs6 = substr(tariff_line, 1, 6),
                   hs4 = substr(tariff_line, 1, 4),
                   hs2 = substr(tariff_line, 1, 2))]
  
  data_plot <- merge(data_plot, fao_group_dt, by = c("isscfc_code"), all.x = T) 
  data_plot[, c("fao_group.x", "isscfc_id.x") := NULL]
  setnames(data_plot, c("fao_group.y", "isscfc_id.y"), c("fao_group", "isscfc_id"))
  
  ##-- Copy old values
  data_plot[is.na(weight_correction), `:=`(weight_correction = weight, weight_method = "copy")]
  data_plot[is.na(value_correction),  `:=`(value_correction = as.numeric(value), value_method = "copy")]
  
  ##** Compute agregation from root to leaf levels ----
  data_plot[, `:=`(uv = as.numeric(value)/as.numeric(weight), 
                   uv_correction = value_correction/weight_correction)]
  
  data_plot[, partner := as.integer(partner)]
  data_plot <- merge(data_plot, m49_codes[, list(Partner = country, reporter)], by.x = "partner", by.y = "reporter", all.x = TRUE)
  
  values_od$data_plot <- data_plot
  
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Great!",
    text = "The outlier data has been saved successfully.",
    type = "success"
  )
  
})

##-- Update data ----
##** Launch a popup to ask confirmation to the user
observeEvent(input$export_outlier_correction, {
  
  ##-- Check if all outliers have been validated successfully ----
  query <- sprintf("rep = '%s' OR rep = '%s' AND year = %s", input$reporter_detected, stringr::str_pad(input$reporter_detected, 3, "left", "0"), input$detected_years)
  outliers_detected <- ReadDatatable(table = "fishtrade_outliers_detected", where = query)
  must_be_validated <- c("tariff_line", "isscfc_code", "hs6")
  
  n_to_be_validated <- outliers_detected[level_aggregation %in% must_be_validated & criteria == "uv", sum(!validated)]
  
  if(n_to_be_validated > 0) {
    
    fao_confirmSweetAlert(
      session = session,
      inputId = "update_outlier_correction_confirm",
      type = "warning",
      text = tags$span(tags$h3("Could you want to confirm?")),
      title = sprintf("There is/are still %s outlier(s) to be validated.", n_to_be_validated),
      danger_mode = TRUE, 
      html = T
    )
    
  } else {
    
    fao_confirmSweetAlert(
      session = session,
      inputId = "update_outlier_correction_confirm",
      type = "warning",
      text = tags$span(tags$h3("Do you want to confirm?")),
      title = "Update the data could take a while.",
      danger_mode = TRUE, 
      html = T
    )  
    
  }
  
  
  
  
})


observeEvent(input$update_outlier_correction_confirm, {
  
  if(input$update_outlier_correction_confirm == FALSE) return(NULL)
  
  ##** Progress bar starts 
  progressSweetAlert(
    session = session, id = "update_outlier_correction_progress",
    title = "Save in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "update_outlier_correction_progress",
    value = 15
  )
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "update_outlier_correction_progress",
    value = 35
  )
  
  ## load data to be exported as validated data  
  query <- sprintf("rep = '%s' OR rep = '%s' AND year = %s", input$reporter_detected, stringr::str_pad(input$reporter_detected, 3, "left", "0"), input$detected_years)
  dt_to_updated <- ReadDatatable(table = "fishtrade_outlier_corrected", where = query)
  
  updateProgressBar(
    session = session,
    id = "update_outlier_correction_progress",
    value = 55
  )
  
  ## copy the original values for those do not accepted
  dt_to_updated[accepted == FALSE, 
                `:=`(weight_correction = weight, 
                     value_correction = value,
                     weight_method = NA,
                     value_method = NA)]
  
  updateProgressBar(
    session = session,
    id = "update_outlier_correction_progress",
    value = 85
  )
  
  dt_to_updated[, rep := as.numeric(rep)]
  
  order_cols <- names(ReadDatatable(table = "fishtrade_outlier_validated", limit = 1))
  writeDT(data = dt_to_updated[, order_cols, with = F], dt_name = "fishtrade_outlier_validated", sql_filter = query)
  
  
  updateProgressBar(
    session = session,
    id = "update_outlier_correction_progress",
    value = 95
  )
  
  updateReporterStatus(.year = input$detected_years, 
                       .reporter = input$reporter_detected, 
                       .process = c("is_outlier_validated", "is_run_mirroring", "is_mirroring_validated"), 
                       .value = c(1, 0, 0))
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session, 
    closeOnClickOutside = FALSE, 
    title = "Great!",
    text = "The data has been saved successfully.",
    type = "success"
  ) 
  
  
  
  
})

















