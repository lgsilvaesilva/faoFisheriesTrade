imputation_module <- reactiveValues(imputed_data_sws = data.table(), 
                                    imputed_data_show = data.table(), 
                                    years_imputed = year_reporter)

##-- Button: Years ----
observe({
  
  if(input$navbar != "imputation") return(NULL)
  
  input$run_imputation
  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  years_imputed <- run_mapping_values$ft_workflow[is_built_data == 1 & 
                                                    is_run_mapping == 1 & 
                                                    is_mapped_data == 1 & 
                                                    is_run_imputation == 1 & 
                                                    is_imputed_data == 0, unique(year)]
  
  updatePickerInput(session = session,
                    inputId = "imputed_years", 
                    choices = sort(years_imputed, decreasing = T))
  
})

##-- Button: reporter ----
observeEvent(c(input$run_imputation, input$imputed_years), {
  
  if(input$navbar != "imputation") return(NULL)
  
  
  reporters_imputed <- run_mapping_values$ft_workflow[year == input$imputed_years & is_run_imputation == 1 & is_imputed_data == 0, unique(rep)]
  reporters_imputed <- m49_codes[reporter %in% reporters_imputed, ]
  rep2impute_list <- reporters_imputed$reporter
  names(rep2impute_list) <- reporters_imputed$country
  
  updatePickerInput(session = session,
                    inputId = "reporter_imputed", 
                    choices = rep2impute_list)
  
})

##-- Get imputed data ----
observeEvent(priority = 1, c(input$reporter_imputed, input$imputed_years), {
  
  validate(
    need(input$reporter_imputed, "Please select at least one year."),
    need(input$imputed_years, "Please select at least one country.")
  )
  
  table <- "fishtrade_data_imputed"
  query <- paste0('rep = ', input$reporter_imputed, " AND ", "year = ", input$imputed_years)
  imputed_data_sws <- ReadDatatable(table = table, where = query)
  imputed_data_sws$unit_value <- imputed_data_sws$value / imputed_data_sws$weight
  imputation_module$imputed_data_sws <- imputed_data_sws
  
  mapping_table <- ReadDatatable(fishtrade_trade_map, where = paste0("rep = ", input$reporter_imputed))
  imputed_data_sws <- map_trade(.data = imputed_data_sws, mapping_table)
  
  setnames(imputed_data_sws, 
           old = c("rep", "prt", 'comm', 'percvanna'),
           new = c("reporter", "partner", 'tariff_line', 'perc_non_na'))
  
  # imputed_data_sws <- merge(imputed_data_sws, m49_codes, by = "reporter", all.x = TRUE)
  imputed_data_sws[, rep_lab := country][, country := NULL]
  
  imputed_data_sws <- merge(imputed_data_sws, m49_codes, by.x = "partner", by.y = "reporter", all.x = TRUE)
  imputed_data_sws[, prt_lab := country][, country := NULL]
  imputed_data_sws[, flow := flow_labels[flow]]
  
  setcolorder(imputed_data_sws, 
              c('year', 'reporter', 'rep_lab', 'partner', 'prt_lab','flow', 
                'tariff_line', 'tariff_line_descr',
                'isscfc_code', 'isscfc_id', 'isscfc_descr', 
                'scheda', 'remark',
                'value', 'weight', 'qty', 'unit_value', 'qunit', 
                'hslength', 'perc_non_na', 'imputation_method'))
  
  imputation_module$imputed_data_sws <- imputed_data_sws
  
  imputed_data_sws$unit_value <- imputed_data_sws$value / imputed_data_sws$weight
  imputation_module$imputed_data_show <- imputed_data_sws
  
  ##-- Update buttom to select imputation
  imputation_method_choices <- c(imputation_module$imputed_data_sws[, unique(imputation_method)])
  updatePickerInput(session = session, 
                    inputId = "filter_imputed", 
                    selected = imputation_method_choices[!imputation_method_choices %in% c('no_imputation')],
                    choices = imputation_method_choices)
  
})

##-- Unit Value ----
observeEvent(input$imputed_data, {
  
  if(is.null(input$imputed_data)) return(NULL)
  if(is.null(input$imputed_data$changes$changes)) return(NULL)
  if(!input$imputed_data$changes$changes[[1]][[2]] %in% 7:8) return(NULL)
  
  imputed_data <- hot_to_r(input$imputed_data)
  imputed_data$unit_value <- imputed_data$value / imputed_data$weight
  imputation_module$imputed_data_show <- imputed_data
  
})

##-- Filter Methods ----
observeEvent(input$filter_imputed, {
  
  imputed_data_sws <- imputation_module$imputed_data_sws
  
  if(!"unit_value" %in% names(imputed_data_sws)) {
    
    imputed_data_sws$unit_value <- imputed_data_sws$value / imputed_data_sws$weight
    
  }
  
  setDT(imputed_data_sws)
  
  if(!any(input$filter_imputed %in% "all")) {
    
    imputed_data_sws <- imputed_data_sws[imputation_method %in% input$filter_imputed, ]
    
  }
  
  setcolorder(imputed_data_sws, 
              c('year', 'reporter', 'rep_lab', 'partner', 'prt_lab','flow', 
                'tariff_line', 'tariff_line_descr',
                'isscfc_code', 'isscfc_id', 'isscfc_descr', 
                'scheda', 'remark',
                'value', 'weight', 'qty', 'unit_value', 'qunit', 
                'hslength', 'perc_non_na', 'imputation_method'))
  
  # setcolorder(imputed_data_sws, 
  #             c('year', 'reporter', 'rep_lab', 'partner', 'prt_lab','flow', 'tariff_line', 
  #               'value', 'weight', 'qty', 'unit_value', 'qunit', 
  #               'hslength', 'perc_non_na', 'imputation_method'))
  
  imputation_module$imputed_data_show <- as.data.frame(imputed_data_sws)
  
})

##-- Table: imputed data ----
output$imputed_data <- renderRHandsontable({
  
  validate(
    need(input$reporter_imputed, "Please select at least one year."),
    need(input$imputed_years, "Please select at least one country."),
    need(input$filter_imputed, "Please select at least one method.")
  )
  
  rhandsontable(imputation_module$imputed_data_show, selectCallback = TRUE) %>%
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
  
})


##-- Save Data ----
observeEvent(input$save_data_imputed, {
  
  confirmSweetAlert(
    session = session,
    inputId = "save_imputed_data_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Save the data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

observeEvent(input$save_imputed_data_confirmation, {
  
  if(input$save_imputed_data_confirmation == FALSE) return(NULL)
  
  validate(need(input$imputed_data, "Please select at least a country and year."))
  
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
  
  query <- paste0('rep = ', input$reporter_imputed, " AND ", "year = ", input$imputed_years)
  imputed_data_sws <- ReadDatatable(table = fishtrade_data_imputed, where = query)
  
  ##** updating progress bar 35 ----
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 35
  )
  
  ## Get data changed by user
  imputed_data_to_upload <- hot_to_r(input$imputed_data)
  
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
  imputed_data_to_upload[, `:=`(rep = as.integer(rep), flow = as.integer(flow))]
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
  imputation_method_choices <- c(imputation_module$imputed_data_sws[, unique(imputation_method)])
  updatePickerInput(session = session, 
                    inputId = "filter_imputed", 
                    selected = imputation_method_choices[!imputation_method_choices %in% c('no_imputation')],
                    choices = imputation_method_choices)
  
  updateProgressBar(
    session = session,
    id = "save_imputation_progress",
    value = 85
  )
  
  data_tobe_save[, flow := flow_labels_code[flow]]
  writeDT(dt_name = fishtrade_data_imputed, data = data_tobe_save, sql_filter = query)
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Great!",
    text = "The imputed data has been saved successfully.",
    type = "success"
  )
  
})

##-- Update/Export/Validated Data ----
observeEvent(input$export_data_imputation, {
  
  confirmSweetAlert(
    session = session,
    inputId = "export_imputed_data_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Update the data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

observeEvent(input$export_imputed_data_confirmation, {
  
  if(input$export_imputed_data_confirmation == FALSE) return(NULL)
  
  validate(need(input$imputed_data, "Please select at least a country and year."))
  
  ##** Progress bar starts ----
  progressSweetAlert(
    session = session, id = "update_imputation_progress",
    title = "Update in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 25 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress",
    value = 25
  )
  
  query <- paste0('rep = ', input$reporter_imputed, " AND ", "year = ", input$imputed_years)
  data_tobe_save <- ReadDatatable(table = fishtrade_data_imputed, where = query)
  
  ##** updating progress bar 50 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress",
    value = 50
  )
  
  ##** data to be imputed manually ----
  #' Test if there is still data to be imputed
  n_missing_data <- data_tobe_save[, sum(is.na(weight) | is.na(value) | imputation_method == "not_imputed")]
  
  ##** updating progress bar 70 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress",
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
    id = "update_imputation_progress",
    value = 90
  )
  
  #** saving the final data imputed into SWS ----
  writeDT(dt_name = fishtrade_data_imputed_validated, data = data_tobe_save, sql_filter = query)
  
  ##** updating reporter status ----
  updateReporterStatus(.year = input$imputed_years, 
                       .reporter = input$reporter_imputed, 
                       .process = c("is_imputed_data", "is_run_outlier", "is_outlier_validated", 
                                    "is_run_mirroring", "is_mirroring_validated"), 
                       .value = c(1, rep(0, 4)))
  
  ##** updating progress bar 90 ----
  updateProgressBar(
    session = session,
    id = "update_imputation_progress",
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
##-- Donuts: TL ----
output$donut_imports_general_tl <- renderPlotly({
  
  validate(
    need(input$reporter_imputed, "Please select at least one year."),
    need(input$imputed_years, "Please select at least one country.")
  )
  
  rep_data_curr <- copy(imputation_module$imputed_data_sws)
  rep_data_curr[, flow := flow_labels_code[flow]]
  setDT(rep_data_curr)
  
  rep_data_curr[, percvanna := perc_non_na]
  rep_data_curr[imputation_method %in% c("not_missing", "blocked_to_impute"), percvanna := 1]
  rep_data_curr[!imputation_method %in% c("not_missing", "blocked_to_impute"), qunit := 8]
  
  ##** Aggregation ----
  ##** Tariff Line level ----
  data_agg_tl <- rep_data_curr[, list(value_na = sum(value * (1 - percvanna), na.rm = T),
                                      value_nna = sum(value * percvanna, na.rm = T),
                                      weight_na = sum(weight * (1 - percvanna), na.rm = T),
                                      weight_nna = sum(weight * percvanna, na.rm = T),
                                      n_partners = .N,
                                      imputation_method = toString(unique(imputation_method))
  ), 
  by = list(year, reporter, flow, tariff_line, qunit)]
  
  ##** percentage of imputation
  data_agg_tl[, 
              `:=`(
                perc_v_imputed = value_na/(value_na + value_nna),
                perc_w_imputed = weight_na/(weight_na + weight_nna),
                value = value_na + value_nna,
                weight = weight_na + weight_nna
              )]
  
  #' commodities are considered as imputed when more than 50% of the monetary value is imputed.
  #' Note that the imputation process is done under the variable 'weight'
  ##** unit value 
  data_agg_tl[, `:=`(uv = value / weight,
                     uv_nna = value_nna / weight_nna)]
  
  donuts_general_tl <- data_agg_tl[, list(perc_v_nna = sum(value_nna) / sum(value), perc_w_nna = sum(weight_nna) / sum(weight)), by = flow]
  donuts_general_tl[, `:=`(perc_v_na = 1 - perc_v_nna, perc_w_na = 1 - perc_w_nna)]
  donuts_general_tl <- melt(donuts_general_tl, 1)
  donuts_general_tl[, label :=  ifelse(grepl("_nna", variable), "% non missing", "% missing")]
  
  ##*** Flow: 1
  gg1_donuts_general_tl <- plot_ly() %>%
    add_pie(data = donuts_general_tl[grepl("perc_v", variable) & flow == 1,], 
            labels = ~label, 
            values = ~value, 
            hole = 0.6,
            name = "Value", 
            # domain = list(row = 0, column = 0),
            domain = list(x = c(0.0, 0.22), y = c(0, 1))
    ) %>%
    add_pie(data = donuts_general_tl[grepl("perc_w", variable) & flow == 1,],
            labels = ~label,
            values = ~value, hole = 0.6,
            name = "Weight",
            # domain = list(row = 0, column = 1)
            domain = list(x = c(0.0, 0.22) + 0.25, y = c(0, 1))
    ) %>%
    ##*** Flow: 2
    add_pie(data = donuts_general_tl[grepl("perc_v", variable) & flow == 2,],
            labels = ~label,
            values = ~value,
            hole = 0.6,
            name = "Value",
            # domain = list(row = 0, column = 2)
            domain = list(x = c(0.0, 0.22) + 2*0.25, y = c(0, 1))
    ) %>%
    add_pie(data = donuts_general_tl[grepl("perc_w", variable) & flow == 2,],
            labels = ~label,
            values = ~value, hole = 0.6,
            name = "Weight",
            # domain = list(row = 0, column = 3)
            domain = list(x = c(0.0, 0.22) + 3*0.25, y = c(0, 1))
    ) %>%
    layout(title = "",  
           showlegend = F,
           # grid = list(rows = 1, columns = 4),
           # plot_bgcolor = 'transparent',
           # paper_bgcolor='transparent',
           annotations = list(
             list(x = 0.075 , y = 0.5, text = "Imports\nValue", showarrow = F, xref='paper', yref='paper', font = list(size = 25)),
             list(x = 0.360 , y = 0.5, text = "Imports\nWeight", showarrow = F, xref='paper', yref='paper', font = list(size = 25)),
             list(x = 0.610 , y = 0.5, text = "Exports\nValue", showarrow = F, xref='paper', yref='paper', font = list(size = 25)),
             list(x = 0.895 , y = 0.5, text = "Exports\nWeight", showarrow = F, xref='paper', yref='paper', font = list(size = 25))
           ),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
  
  gg1_donuts_general_tl$elementId <- NULL
  
  gg1_donuts_general_tl
  
})

##-- Bar chart: imputation methods ----
##** Imports ----
output$bar_chart_methods_imports <- renderPlotly({
  
  validate(
    need(input$reporter_imputed, "Please select at least one year."),
    need(input$imputed_years, "Please select at least one country.")
  )
  
  data_imputed <- copy(imputation_module$imputed_data_sws)
  data_imputed[, flow := flow_labels_code[flow]]
  
  setDT(data_imputed)
  
  data_imputed[imputation_method %in% c("not_missing", "blocked_to_impute"), perc_non_na := 1]
  
  data_plot <- data_imputed[!imputation_method %in% c("not_missing", "blocked_to_impute") & flow == 1, 
                            list(
                              freq = .N,
                              value = sum(value * (1 - perc_non_na), na.rm = TRUE),
                              weight = sum(weight * (1 - perc_non_na), na.rm = TRUE)
                            ), 
                            by = list(imputation_method)]
  
  data_plot[, `:=`(value_perc = -value/sum(value), 
                   weight_perc = weight/sum(weight),
                   abs_v = value/sum(value))]
  
  tick_value <- seq(-1, 1, .1)
  tick_label <- scales::percent(abs(tick_value))
  
  if(nrow(data_plot) == 0) return(NULL)
  
  p <- plot_ly(data_plot) %>%
    add_bars(x = ~value_perc, y = ~imputation_method, 
             color = I("steelblue"),
             name = "Value",
             orientation = "h", 
             hoverinfo = "y+text+name", 
             text = ~scales::percent(abs_v)) %>%
    add_bars(x = ~weight_perc, y = ~imputation_method, 
             color = I("darkorange1"),
             name = "Weight",
             orientation = "h",
             hoverinfo = "y+text+name", 
             colors = "Set1",
             text = ~scales::percent(weight_perc)) %>%
    layout(bargap = 0.1, barmode = "overlay",
           yaxis = list(title = ""),
           xaxis = list(title = "% imputed",
                        tickmode = 'array', 
                        tickvals = tick_value,
                        ticktext = tick_label))
  
  p$elementId <- NULL
  
  p
  
  
})
##** Exports ----
output$bar_chart_methods_exports <- renderPlotly({
  
  validate(
    need(input$reporter_imputed, "Please select at least one year."),
    need(input$imputed_years, "Please select at least one country.")
  )
  
  data_imputed <- copy(imputation_module$imputed_data_sws)
  setDT(data_imputed)
  data_imputed[, flow := flow_labels_code[flow]]
  
  data_imputed[is.na(perc_non_na), perc_non_na := 1]
  
  data_plot <- data_imputed[!imputation_method %in% c("not_missing", "blocked_to_impute") & flow == 2, 
                            list(
                              freq = .N,
                              value = sum(value * (1 - perc_non_na), na.rm = TRUE),
                              weight = sum(weight * (1 - perc_non_na), na.rm = TRUE)
                            ), 
                            by = list(imputation_method)]
  
  data_plot[, `:=`(value_perc = -value/sum(value), 
                   weight_perc = weight/sum(weight),
                   abs_v = value/sum(value))]
  
  tick_value <- seq(-1, 1, .1)
  tick_label <- scales::percent(abs(tick_value))
  
  if(nrow(data_plot) == 0) return(NULL)
  
  p <- plot_ly(data_plot) %>%
    add_bars(x = ~value_perc, y = ~imputation_method, 
             color = I("steelblue"),
             name = "Value",
             orientation = "h", 
             hoverinfo = "y+text+name", 
             text = ~scales::percent(abs_v)) %>%
    add_bars(x = ~weight_perc, y = ~imputation_method, 
             color = I("darkorange1"),
             name = "Weight",
             orientation = "h",
             hoverinfo = "y+text+name", 
             colors = "Set1",
             text = ~scales::percent(weight_perc)) %>%
    layout(bargap = 0.1, barmode = "overlay",
           yaxis = list(title = ""),
           xaxis = list(title = "% imputed",
                        tickmode = 'array', 
                        tickvals = tick_value,
                        ticktext = tick_label))
  
  p$elementId <- NULL
  p
  
  
})
