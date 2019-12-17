datavis_imputation <- reactiveValues(imputed_data = data.table(), 
                                     # full_hist_data = data_hist,
                                     comm_data = data.table(),
                                     comm_code = data.table())

##-- Get imputed data ----
observeEvent(priority = 1, c(input$reporter_imputed_viz, input$imputed_years_viz), {
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country.")
  )
  
  query <- paste0('rep = ', input$reporter_imputed_viz, " AND ", "year = ", input$imputed_years_viz)
  
  ## Get imputed data
  imputed_data_sws <- ReadDatatable(table = fishtrade_data_imputed, where = query)
  imputed_data_sws$unit_value <- imputed_data_sws$value / imputed_data_sws$weight
  
  ## Get trade map table
  query <- paste0('rep = ', input$reporter_imputed_viz)
  trademap_rep <- ReadDatatable(table = fishtrade_trade_map, where = query)
  
  ## mapping trade data
  imputed_data_sws <- map_trade(.data = imputed_data_sws, .map = trademap_rep)
  
  setnames(imputed_data_sws,
           old = c("rep", "prt", 'comm', 'percvanna'),
           new = c("reporter", "partner", 'tariff_line', 'perc_non_na'))
  
  setkey(imputed_data_sws, imputation_method, flow, isscfc_code)  
  datavis_imputation$imputed_data <- imputed_data_sws
  
})

##-- Button ----
##** Years ----
observe({
  
  if(input$navbar != "imputation") return(NULL)
  
  input$run_imputation
  input$outlier_detection
  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  years_imputed <- run_mapping_values$ft_workflow[is_built_data == 1 & 
                                                    is_run_mapping == 1 & 
                                                    is_mapped_data == 1 & 
                                                    is_run_imputation == 1 & 
                                                    is_imputed_data == 0, unique(year)]
  
  updatePickerInput(session = session,
                    inputId = "imputed_years_viz", 
                    choices = sort(years_imputed, decreasing = T)
  )
  
})

##** Reporter ----
observeEvent(c(input$run_imputation, input$outlier_detection, input$imputed_years_viz), {
  
  if(input$navbar != "imputation") return(NULL)
  
  input$run_imputation
  input$outlier_detection
  reporters_imputed <- run_mapping_values$ft_workflow[year == input$imputed_years_viz & is_run_imputation == 1 & is_imputed_data == 0, unique(rep)]
  reporters_imputed <- m49_codes[reporter %in% reporters_imputed, ]
  rep2impute_list <- reporters_imputed$reporter
  names(rep2impute_list) <- reporters_imputed$country
  
  updatePickerInput(session = session, 
                    inputId = "reporter_imputed_viz", 
                    choices = rep2impute_list)
  
})
##** Flow ----
observeEvent(c(input$run_imputation, input$outlier_detection, input$imputed_years_viz, input$reporter_imputed_viz), {
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country.")
  )
  
  setkey(datavis_imputation$imputed_data, imputation_method, flow, isscfc_code)  
  
  imputation_methods <- setdiff(datavis_imputation$imputed_data[, unique(imputation_method)],  c('not_missing', 'blocked_to_impute'))
  
  flow_list <- datavis_imputation$imputed_data[list(imputation_methods), unique(flow)]
  flow_list <- flow_labels_code[as.numeric(flow_list)]
  
  updatePickerInput(session = session,
                    inputId = "flow_imputed_viz",
                    choices = flow_list)
  
})
##** Commodity ----
output$tblCommodityImputed <- DT::renderDataTable({
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country."),
    need(input$flow_imputed_viz, "Please select at least one flow.")
  )
  
  if(nrow(datavis_imputation$imputed_data) == 0) return(NULL)
  
  imputed_data <- copy(isolate(datavis_imputation$imputed_data))
  
  imputation_methods <- setdiff(imputed_data[, unique(imputation_method)],  c('not_missing', 'blocked_to_impute'))
  comm_code <- imputed_data[list(imputation_methods, input$flow_imputed_viz), ][isscfc_code != -1, ]
  comm_code <- unique(comm_code[, 
                                list(
                                  year,
                                  flow,
                                  tariff_line, 
                                  tariff_line_descr,
                                  isscfc_code,
                                  isscfc_descr,
                                  scheda
                                )])
  
  
  comm_code$flow <- flow_labels[comm_code$flow]
  
  datavis_imputation$comm_code <- comm_code
  # browser()
  datatable(
    comm_code, 
    colnames = c("Year", "Flow", "Tariff Line", "Tariff Line descr.", "ISSCFC", "ISSCFC desc.", "Scheda"),
    rownames = FALSE, 
    filter = 'top',
    selection = list(mode = "single", target = "cell"),
    # extensions = 'Buttons', 
    options = list(#dom = 'Bfrtip', buttons = I('colvis'), 
                   pageLength = 5)
  ) 
  
})



##-- Time series: commodity ----
output$gg_ts_commodity <- renderPlotly({
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country."),
    need(input$flow_imputed_viz, "Please select at least one flow."),
    need(input$tblCommodityImputed_cells_selected, "Please select one commodity.")
  )
  
  comm_type <- names(datavis_imputation$comm_code)[input$tblCommodityImputed_cells_selected[1, 2] + 1]
  comm_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], comm_type, with = F]
  flow_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], 'flow', with = F]
  flow_selected <- flow_labels_code[as.character(flow_selected)]
  filter_expression <- sprintf("%s == '%s' & flow == %s", comm_type, comm_selected, flow_selected)
  
  if(!comm_type %in% c("scheda", "isscfc_code")) {
    
    sendSweetAlert(
      session = session,
      title = "Invalid column.",
      text = "Please, select the scheda or ISSCFC columns.",
      type = "error"
    )
    
    return(NULL)
    
  }
  
  reporter_selected <- stringr::str_pad(input$reporter_imputed_viz, width = 3, side = "left", pad = "0")
  ##-- Load historical data ----
  
  # rep_hist <- datavis_imputation$full_hist_data[list(reporter_selected, as.numeric(input$flow_imputed_viz)), ][year != input$imputed_years_viz, ]
  # rep_hist <- datavis_imputation$full_hist_data[year != input$imputed_years_viz & reporter == reporter_selected, ]
  if(any(is.na(c(comm_selected, flow_selected)))) return(NULL)
  sql_rep_hist <- sprintf("rep = '%s' AND year <> '%s' AND %s = '%s' AND flow = %s", 
                          reporter_selected, 
                          input$imputed_years_viz, 
                          gsub("_code", "", comm_type), 
                          comm_selected, 
                          flow_selected)
  
  rep_hist <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist)
  setnames(rep_hist, c("rep", "remarks", "isscfc"), c("reporter", "remark", "isscfc_code"))
  setkey(rep_hist,  reporter, flow, year)
  rep_hist <- rep_hist[, list( year, reporter, flow, scheda, isscfc_code, qty, value = value_usd)]
  
  imputed_data_sws <- copy(datavis_imputation$imputed_data)
  imputed_data_sws <- imputed_data_sws[, list(year, reporter, flow, scheda, isscfc_code, qty = weight, value)]
  
  
  data_full <- rbindlist(l = list(rep_hist, imputed_data_sws), fill = TRUE, idcol = "case")
  
  data_plot <- data_full[eval(parse(text = filter_expression)), 
                         list(
                           qty = sum(as.numeric(qty), na.rm = T),
                           value = sum(as.numeric(value), na.rm = T)
                         ),
                         by = c('year', 'reporter', 'flow', comm_type)]
  
  data_plot[, unit_value := value / qty]
  data_plot <- data_plot[order(year),]
  
  y_var <- ifelse(input$impute_target_variable == "weight", "qty", input$impute_target_variable)
  
  title_list <- c("qty" = "Weight", "value" = "Value", "unit_value" = "Unit Value")
  
  p <- plot_ly(data_plot, x = ~year, y = ~get(y_var), 
               name = 'Historical', 
               type = 'scatter',
               source = "ts_imputed", 
               mode = 'lines+markers') %>%
    add_trace(data = data_plot[year == input$imputed_years_viz, ], x = ~year, y = ~get(y_var), name = 'Imputed', mode = 'markers') %>%
    layout(yaxis = list(title = title_list[y_var]),
           xaxis = list(title = "Year"),
           legend = list(orientation = 'h')
    )
  
  p$elementId <- NULL
  p
  
  
})

##-- Bar Chart: methods ----
output$gg_bc_perc_nna_comm <- renderPlotly({
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country."),
    need(input$flow_imputed_viz, "Please select at least one flow."),
    need(input$tblCommodityImputed_cells_selected, "Please select one commodity.")
  )
  
  
  comm_type <- names(datavis_imputation$comm_code)[input$tblCommodityImputed_cells_selected[1, 2] + 1]
  comm_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], comm_type, with = F]
  flow_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], 'flow', with = F]
  flow_selected <- flow_labels_code[as.character(flow_selected)]
  filter_expression <- sprintf("%s == '%s' & flow == %s", comm_type, comm_selected, flow_selected)
  
  if(!comm_type %in% c("scheda", "isscfc_code")) {
    
    return(NULL)
    
  }
  
  imputed_data_sws <- copy(datavis_imputation$imputed_data)
  imputed_data_sws[imputation_method %in% c("not_missing", "blocked_to_impute"), perc_non_na := 1]
  
  comm_data <- imputed_data_sws[eval(parse(text = filter_expression)), ]
  
  method_data <- comm_data[,
                           list(
                             wt_na = sum(weight, na.rm = T),                        # total of weights imputed
                             value_na = sum(value *  (1 - perc_non_na), na.rm = T)  # total of monetary value which represent the "missing" weights
                           ),
                           by = c('year', 'reporter', 'flow', comm_type, 'imputation_method')]
  
  total_data <- comm_data[, 
                          list(
                            weight = sum(weight, na.rm = T),  # sum total of weights
                            value = sum(value, na.rm = T)  # sum total of values
                          ),
                          by = c('year', 'reporter', 'flow', comm_type)]
  
  method_data <- merge(method_data, total_data, by = c('year', 'reporter', 'flow', comm_type))
  
  method_data[!imputation_method %in% c("not_missing", "blocked_to_impute"), 
              `:=`(weight_na_tot = sum(wt_na), 
                   value_na_tot = sum(value_na))]
  
  method_data[, `:=`(`Weight*` =  wt_na / weight_na_tot, 
                     Weight = wt_na / weight,
                     `Value*` =  value_na / value_na_tot,
                     Value = value_na/ value)]
  
  
  method_data_tot <- method_data[, list(imputation_method, Weight, Value)]
  # method_data_tot[, perc_wt_tot := ifelse(imputation_method == "not_missing", 1 - sum(perc_wt_tot), perc_wt_tot)]
  method_data_tot[, Value := ifelse(imputation_method == "not_missing", 1 - sum(Value), Value)]
  method_data_tot <- melt(method_data_tot, 1)
  
  method_data_na <- method_data[imputation_method != "not_missing", 
                                list(imputation_method, `Weight*` = `Weight*`, `Value*` = `Value*`)]
  method_data_na <- melt(method_data_na, 1)
  
  p <- plot_ly(method_data_tot, 
               x = ~variable, 
               y = ~value, 
               type = 'bar', 
               name = ~imputation_method, 
               color = ~imputation_method) %>%
    add_trace(data = method_data_na, 
              x = ~variable, 
              y = ~value, 
              type = 'bar', 
              showlegend = FALSE,
              name = ~imputation_method, 
              color = ~imputation_method) %>%
    layout(yaxis = list(title = '% by method', tickformat = "%"), 
           xaxis = list(title = ""),
           annotations = 
             list(x = 1.5, y = -0.1, text = "* percentage computed considering only the values imputed.", 
                  showarrow = F, xref = 'paper', yref = 'paper', 
                  xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                  font=list(size = 10, color = "indianred")),
           barmode = 'stack')
  
  p$elementId <- NULL
  p
})

##-- Histogram: partners ----
output$gg_histogram_parterns <- renderPlotly({
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country."),
    need(input$flow_imputed_viz, "Please select at least one flow."),
    need(input$tblCommodityImputed_cells_selected, "Please select one commodity.")
  )
  
  comm_type <- names(datavis_imputation$comm_code)[input$tblCommodityImputed_cells_selected[1, 2] + 1]
  comm_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], comm_type, with = F]
  flow_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], 'flow', with = F]
  flow_selected <- flow_labels_code[as.character(flow_selected)]
  filter_expression <- sprintf("%s == '%s' & flow == %s", comm_type, comm_selected, flow_selected)
  
  if(!comm_type %in% c("scheda", "isscfc_code")) {
    
    return(NULL)
    
  }
  
  imputed_data_sws <- copy(datavis_imputation$imputed_data)
  imputed_data_sws[imputation_method %in% c("not_missing", "blocked_to_impute"), perc_non_na := 1]
  
  comm_data <- imputed_data_sws[eval(parse(text = filter_expression)), ]
  y_var <- input$impute_target_variable
  
  all_is_na <- comm_data[, all(is.na(get(y_var)))]
  if(all_is_na) return(NULL)
  
  title_list <- c("weight" = "Weight", "value" = "Value", "unit_value" = "Unit Value")
  
  if(nrow(comm_data) == 1) return(NULL)
  
  hist_base <- hist(comm_data[, get(y_var)], plot = F)
  
  p <- plot_ly(data = comm_data, 
               x = ~get(y_var),
               xbins = list(start = min(hist_base$breaks), 
                            end = max(hist_base$breaks), 
                            size = nth(hist_base$breaks, 2) - nth(hist_base$breaks, 1)),
               marker = list(line = list(color = "darkgray",
                                         width = 1)),
               type = "histogram",
               source = "hist_partners") %>%
    config(displayModeBar = F, showLink = F) %>%
    layout(showlegend = F, 
           dragmode = "select",
           barmode = "overlay", 
           yaxis = list(title = "Frequency", showticklabels = T),
           xaxis = list(title = title_list[y_var]))
  
  p$elementId <- NULL
  p
  
})


##-- Unit Value ----
observeEvent(input$tblPartners, {
  
  if(is.null(input$tblPartners)) return(NULL)
  if(is.null(input$tblPartners$changes$changes)) return(NULL)
  if(!input$tblPartners$changes$changes[[1]][[2]] %in% 7:8) return(NULL)
  
  tblPartners <- hot_to_r(input$tblPartners)
  tblPartners$unit_value <- tblPartners$value / tblPartners$weight
  datavis_imputation$comm_data <- tblPartners
  
})
##-- Table: partners ----
##** Reactive Table ----
observeEvent(c(input$tblCommodityImputed_cells_selected), {
  
  validate(
    need(input$tblCommodityImputed_cells_selected, "Please select one commodity.")
  )
  
  comm_type <- names(datavis_imputation$comm_code)[input$tblCommodityImputed_cells_selected[1, 2] + 1]
  comm_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], comm_type, with = F]
  flow_selected <- datavis_imputation$comm_code[input$tblCommodityImputed_cells_selected[1, 1], 'flow', with = F]
  flow_selected <- flow_labels_code[as.character(flow_selected)]
  filter_expression <- sprintf("%s == '%s' & flow == %s", comm_type, comm_selected, flow_selected)
  
  if(!comm_type %in% c("scheda", "isscfc_code")) {
    
    return(NULL)
    
  }
  
  imputed_data_sws <- copy(datavis_imputation$imputed_data)
  imputed_data_sws[imputation_method %in% c("not_missing", "blocked_to_impute"), perc_non_na := 1]
  
  comm_data <- imputed_data_sws[eval(parse(text = filter_expression)), ]
  
  # obtain plotlyjs selection
  
  comm_data <- merge(comm_data, m49_codes, by.x = c("partner"), by.y = "reporter", all.x = TRUE)
  comm_data[, prt_lab := country.y][, country.y := NULL]
  comm_data[, rep_lab := country.x][, country.x := NULL]
  comm_data[, imputation_method := factor(imputation_method)]
  comm_data[, flow := flow_labels[flow]]
  
  setcolorder(comm_data, 
              c('year', 'reporter', 'rep_lab', 'partner', 'prt_lab','flow', 
                'tariff_line', 'tariff_line_descr',
                'isscfc_code', 'isscfc_id', 'isscfc_descr', 
                'scheda', 'remark',
                'value', 'weight', 'qty', 'unit_value', 'qunit', 
                'hslength', 'perc_non_na', 'imputation_method'))
  
  # comm_data <- comm_data[, c('year', 'reporter', 'rep_lab', 'partner', 'prt_lab','flow', 
  #                            'tariff_line', 
  #                            'value', 'weight', 'qty', 'unit_value', 'qunit', 
  #                            'hslength', 'perc_non_na', 'imputation_method'), with = F]
  
  datavis_imputation$comm_data <- comm_data
  
})

output$tblPartners <- renderRHandsontable({
  
  validate(
    need(input$reporter_imputed_viz, "Please select at least one year."),
    need(input$imputed_years_viz, "Please select at least one country."),
    need(input$flow_imputed_viz, "Please select at least one flow."),
    need(input$tblCommodityImputed_cells_selected, "Please select one commodity.")
  )
  
  s <- event_data("plotly_selected", source = "hist_partners")
  
  if(length(s$x) > 0) {
    
    y_var <- input$impute_target_variable
    
    comm_data <- datavis_imputation$comm_data[get(y_var) >= min(s$x) | get(y_var) >= max(s$x), ]
    
    rhandsontable(comm_data, selectCallback = TRUE) %>%
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE) %>%
      hot_col(col = 1:13, readOnly = T) %>%
      hot_col(col = 'year', readOnly = T) %>%
      hot_col(col = 'reporter', readOnly = T) %>%
      hot_col(col = 'partner', readOnly = T) %>%
      hot_col(col = 'flow', readOnly = T) %>%
      hot_col(col = 'tariff_line', readOnly = T) %>%
      hot_col(col = "weight", format = "0.0") %>%
      hot_col(col = "rep_lab", readOnly = T, width = 130) %>%
      hot_col(col = "prt_lab", readOnly = T, width = 130) %>%
      hot_col(col = 'value', readOnly = F, format = "$0,0.0") %>%
      hot_col(col = 'unit_value', readOnly = T, format = "$0,0.00") %>%
      hot_col(col = 'perc_non_na', readOnly = T, format = '0.00%') %>%
      hot_col(col = 'qunit', readOnly = T) %>%
      hot_col(col = 'hslength', readOnly = T) %>%
      hot_col(col = 'imputation_method', readOnly = T) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
  } else {
    
    if(nrow(datavis_imputation$comm_data) == 0) return(NULL)
    
    rhandsontable(datavis_imputation$comm_data, selectCallback = TRUE) %>%
      hot_cols(columnSorting = TRUE, manualColumnMove = TRUE, manualColumnResize = TRUE) %>%
      hot_col(col = 1:13, readOnly = T) %>%
      hot_col(col = 'year', readOnly = T) %>%
      hot_col(col = 'reporter', readOnly = T) %>%
      hot_col(col = 'partner', readOnly = T) %>%
      hot_col(col = 'flow', readOnly = T) %>%
      hot_col(col = 'tariff_line', readOnly = T) %>%
      hot_col(col = "weight", format = "0.0") %>%
      hot_col(col = "rep_lab", readOnly = T, width = 130) %>%
      hot_col(col = "prt_lab", readOnly = T, width = 130) %>%
      hot_col(col = 'value', readOnly = F, format = "$0,0.0") %>%
      hot_col(col = 'unit_value', readOnly = T, format = "$0,0.00") %>%
      hot_col(col = 'perc_non_na', readOnly = T, format = '0.00%') %>%
      hot_col(col = 'qunit', readOnly = T) %>%
      hot_col(col = 'hslength', readOnly = T) %>%
      hot_col(col = 'imputation_method', readOnly = T) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    
    
  }
  
  
  
})

##-- Save Data ----
observeEvent(input$save_data_imputed_prt, {
  
  confirmSweetAlert(
    session = session,
    inputId = "save_imputed_prt_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Save the data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

observeEvent(input$save_imputed_prt_confirmation, {
  
  if(input$save_imputed_prt_confirmation == FALSE) return(NULL)
  
  validate(need(input$tblPartners, "Please select at least a country and year."))
  
  ##** Progress bar starts ----
  progressSweetAlert(
    session = session, id = "save_imputation_progress",
    title = "Save in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 15 ----
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 15
  )
  
  query <- paste0('rep = ', input$reporter_imputed_viz, " AND ", "year = ", input$imputed_years_viz)
  imputed_data_sws <- ReadDatatable(table = fishtrade_data_imputed, where = query)
  
  ##** updating progress bar 35 ----
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 35
  )
  
  ## Get data changed by user
  imputed_data_to_upload <- hot_to_r(input$tblPartners)
  
  ## Convert to datatable
  setDT(imputed_data_to_upload)
  
  ## Setting the names to standardize with SWS names
  setnames(imputed_data_to_upload, 
           c("reporter", "partner", "tariff_line", "perc_non_na"), 
           c("rep", "prt", "comm", "percvanna"))
  
  ##-- Remove the reporter and partner labels, and unit value
  imputed_data_to_upload[,
                         `:=`(unit_value = NULL,
                              rep_lab = NULL,
                              prt_lab = NULL
                         )
                         ]
  
  ##-- Recode the flow labels
  imputed_data_to_upload[, flow := flow_labels_code[flow]]
  imputed_data_to_upload[, rep := as.integer(rep)]
  
  data_tobe_save <- merge(imputed_data_sws, 
                          imputed_data_to_upload, 
                          by = c('year', 
                                 'rep', 
                                 'prt', 
                                 'flow', 
                                 'comm', 
                                 'hslength',
                                 'imputation_method'), 
                          all.x = TRUE, 
                          suffixes = c('_old', '_new'))
  
  ##** updating progress bar 55 ----
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 55
  )
  
  ##** Update method ----
  data_tobe_save[!is.na(value_new)  & (value_new != value_old)   | (!is.na(value_new)  & is.na(value_old))  | 
                   !is.na(weight_new) & (weight_new != weight_old) | (!is.na(weight_new) & is.na(weight_old)) | 
                   !is.na(qty_new)    & (qty_new != qty_old)       | (!is.na(qty_new)    & is.na(qty_old))    |  
                   !is.na(qunit_new)  & (qunit_new != qunit_old)   | (!is.na(qunit_new)  & is.na(qunit_old)),  
                 imputation_method := "manual_imputation"]
  
  ##** Update new value ----
  data_tobe_save[!is.na(value_new) & (value_new != value_old), value_old := value_new]
  
  ##** Update new weight ----
  data_tobe_save[!is.na(weight_new), weight_old := weight_new]
  
  ##** Update new qty ----
  data_tobe_save[!is.na(qty_new), qty_old := qty_new]
  
  ##** Update new qunit ----
  data_tobe_save[!is.na(qunit_new), qunit_old := qunit_new]
  
  setnames(data_tobe_save, 
           old = c('value_old', 'weight_old', 'qty_old', 'qunit_old', 'percvanna_old'), 
           new = c('value', 'weight', 'qty', 'qunit', 'percvanna'))
  
  ## Labeling the countries
  data_tobe_save <- merge(data_tobe_save, m49_codes, by.x = "rep", by.y = "reporter", all.x = TRUE)
  data_tobe_save[, rep_lab := country][, country := NULL]
  data_tobe_save <- merge(data_tobe_save, m49_codes, by.x = "prt", by.y = "reporter", all.x = TRUE)
  data_tobe_save[, prt_lab := country][, country := NULL]
  
  ## Labeling flows
  data_tobe_save[, flow := flow_labels[flow]]
  
  ## Removing the variable with suffixe "new"
  data_tobe_save <- data_tobe_save[, which(!grepl("new", names(data_tobe_save))), with = F]
  imputation_module$imputed_data_sws <- copy(data_tobe_save)
  
  data_tobe_save <- data_tobe_save[, names(imputed_data_sws), with = F]
  setnames(imputation_module$imputed_data_sws, c("rep", "prt", "comm", "percvanna"), 
           c("reporter", "partner", "tariff_line", "perc_non_na"))
  
  ##** updating progress bar 75 ----
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 75
  )
  
  ##-- Update buttom to select imputation
  imputation_method_choices <- c(imputation_module$imputed_data_sws[, as.character(unique(imputation_method))])
  updatePickerInput(session = session, 
                    inputId = "filter_imputed", 
                    selected = imputation_method_choices[!imputation_method_choices %in% c('no_imputation')],
                    choices = imputation_method_choices)
  
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 85
  )
  
  ## Get trade map table
  query <- paste0('rep = ', input$reporter_imputed_viz)
  trademap_rep <- ReadDatatable(table = fishtrade_trade_map, where = query)
  data_tobe_save[, flow := flow_labels_code[flow]]
  
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 97
  )
  
  writeDT(dt_name = fishtrade_data_imputed, data = data_tobe_save, sql_filter = query)
  
  ## mapping trade data
  data_tobe_save <- map_trade(.data = data_tobe_save, .map = trademap_rep)
  data_tobe_save$unit_value <- data_tobe_save$value / data_tobe_save$weight
  
  setnames(data_tobe_save,
           old = c("rep", "prt", 'comm', 'percvanna'),
           new = c("reporter", "partner", 'tariff_line', 'perc_non_na'))
  
  setkey(data_tobe_save, imputation_method, flow, isscfc_code)  
  
  datavis_imputation$imputed_data <- copy(data_tobe_save)
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Great!",
    text = "The imputed data has been saved successfully.",
    type = "success"
  )
  
})


##-- Update/Export/Validated Data ----
observeEvent(input$export_data_imputation_prt, {
  
  confirmSweetAlert(
    session = session,
    inputId = "export_imputed_data_confirmation_prt",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Update the data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

observeEvent(input$export_imputed_data_confirmation_prt, {
  
  if(input$export_imputed_data_confirmation_prt == FALSE) return(NULL)
  
  validate(need(input$tblPartners, "Please select at least a country and year."))
  
  ##** Progress bar starts ----
  progressSweetAlert(
    session = session, id = "update_imputation_progress_prt",
    title = "Update in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 25 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress_prt",
    value = 25
  )
  
  query <- paste0('rep = ', input$reporter_imputed_viz, " AND ", "year = ", input$imputed_years_viz)
  data_tobe_save <- ReadDatatable(table = fishtrade_data_imputed, where = query)
  
  ##** updating progress bar 50 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress_prt",
    value = 50
  )
  
  ##** data to be imputed manually ----
  #' Test if there is still data to be imputed
  n_missing_data <- data_tobe_save[, sum(is.na(weight) | is.na(value) | imputation_method == "not_imputed")]
  
  ##** updating progress bar 70 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress_prt",
    value = 70
  )
  
  if (n_missing_data > 0) {
    
    closeSweetAlert(session = session)
    
    sendSweetAlert(
      session = session,
      title = "There are still missing data.",
      text = "Please, check if you have filled all missing data or saved the data before to export.",
      type = "error"
    )
    
    return(NULL)
  }  
  
  ##** updating progress bar 90 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress_prt",
    value = 90
  )
  
  #** saving the final data imputed into SWS ----
  writeDT(dt_name = fishtrade_data_imputed_validated, data = data_tobe_save, sql_filter = query)
  
  ##** updating reporter status ----
  updateReporterStatus(.year = input$mapping_years, 
                       .reporter = input$reporter_to_map, 
                       .process = c("is_imputed_data", "is_run_outlier", "is_outlier_validated", 
                                    "is_run_mirroring", "is_mirroring_validated"), 
                       .value = c(1, rep(0, 4)))
  
  ##** updating progress bar 90 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress_prt",
    value = 95
  )
  
  ##** close progress bar ----
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Great!",
    text = "The imputed data has been updated successfully.",
    type = "success"
  )
  
})