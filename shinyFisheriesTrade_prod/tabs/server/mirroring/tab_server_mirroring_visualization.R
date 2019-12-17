mirroring_values <- reactiveValues()

##-- Year ----
observe({
  
  if(input$navbar != "tab_mirroring") return(NULL)
  
  mirroring_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  trade_years <- mirroring_values$ft_workflow[is_built_data == 1 & 
                                                is_run_mapping == 1 & 
                                                is_mapped_data == 1 & 
                                                is_run_imputation == 1 &
                                                is_imputed_data == 1 &
                                                is_run_outlier == 1 &
                                                is_outlier_validated == 1 & 
                                                is_run_mirroring == 1, unique(year)]
  
  updatePickerInput(session = session,
                    inputId = "year_mirrored",
                    selected = trade_years[1],
                    choices = trade_years)
  
})

getRepMirrored <- reactive({
  rep_mirror <- ReadDatatable(table = "fishtrade_mirrored", where = paste0("year = '", input$year_mirrored, "'"), columns = c("rep", "mirrored"))
  rep_mirror
})

##-- Reporter ----
observe({
  
  if(input$navbar != "tab_mirroring") return(NULL)
  validate(need(input$year_mirrored, ""))
  
  rep_mirror <- getRepMirrored()
  
  if(input$is_mirrored == "all") {
    
    reporter_list <- rep_mirror[, unique(rep)]  
    
  } else {
    
    reporter_list <- rep_mirror[mirrored %in% input$is_mirrored, unique(rep)]
    
  }
  
  
  reporter_list <- m49_codes[reporter %in% reporter_list, ][order(country), ]
  rep2map_list <- reporter_list$reporter
  names(rep2map_list) <- reporter_list$country
  
  updatePickerInput(session = session,
                    inputId = "reporter_mirrored",
                    choices = rep2map_list)
  
  
})

##-- Show Data ----
##** Confirmation dialog box to show data ----
observeEvent(input$show_data, {
  
  fao_confirmSweetAlert(
    session = session,
    inputId = "show_data_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Could you please confirm it?")),
    # title = "Outlier detection process could take a while.",
    danger_mode = TRUE, 
    html = T, 
    p_selector = "#placeholder_mirroring"
  )
  
})
##** Get Data ----
observeEvent(input$show_data_confirmation, {
  
  if(input$show_data_confirmation == FALSE) return(NULL)
  
  validate(need(input$year_mirrored, "Please select a year."),
           need(input$reporter_mirrored, "Please select a reporter"))
  
  
  ##** Progress bar starts 
  progressSweetAlert(
    session = session, id = "getting_mirror_data_progress",
    title = "Getting data in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "getting_mirror_data_progress",
    value = 25
  )
  
  p_year <- input$year_mirrored
  p_reporter <- input$reporter_mirrored
  
  ## getting data mirrored
  sql_filter <- sprintf("rep = '%s' OR rep = '%s' AND year = '%s'", p_reporter, stringr::str_pad(p_reporter, 3, "left", 0), p_year)
  rep_data <- ReadDatatable("fishtrade_mirrored", where = sql_filter)
  setnames(rep_data, c("rep", "prt"), c("reporter", "partner"))
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "getting_mirror_data_progress",
    value = 65
  )
  
  ##  get historical data
  sql_rep_hist <- sprintf("rep = '%s' AND year <> '%s' AND value_usd IS NOT NULL", p_reporter, p_year)
  rep_hist <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist)
  setnames(rep_hist, c("rep", "remarks", "isscfc"), c("reporter", "remark", "isscfc_code"))
  setkey(rep_hist,  reporter, flow, year)
  rep_hist[, value := NULL]
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "getting_mirror_data_progress",
    value = 85
  )
  
  rep_hist <- rep_hist[, list( year, reporter, flow, scheda, 
                               isscfc_code, 
                               tariff_line, tariff_line_descr,
                               remark,
                               weight = qty, value = value_usd)]
  
  rep_data[, reporter := as.numeric(reporter)]
  
  ## combine historical and the target year
  data_full <- rbindlist(l = list(rep_hist, rep_data), fill = TRUE, idcol = "case")
  
  data_full[, value := as.numeric(value)]
  data_full[is.na(value_correction), value_correction := as.numeric(value)]
  data_full[is.na(weight_correction), weight_correction := weight]
  data_full[accepted == FALSE, `:=`(value_correction = value, weight_correction = weight)]
  
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "getting_mirror_data_progress",
    value = 100
  )
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Getting data has been finished!",
    type = "success"
  )
  
  mirroring_values$data_full <- data_full
  
  
})

##-- Flow ----
output$flow_ui <- renderUI({
  
  validate(need(input$year_mirrored, "Please select a year."),
           need(input$reporter_mirrored, "Please select a reporter"),
           need(mirroring_values$data_full, ""))
  
  flow_list <- mirroring_values$data_full[, unique(flow)]
  flow_list <- flow_labels_code[flow_labels_code %in% flow_list]
  
  pickerInput(inputId = "flow_mirrored", 
              label = "Flow", 
              choices = flow_list)
  
})

##-- ISSCFC ----
output$commodity_ui <- renderUI({
  
  validate(need(input$year_mirrored, "Please select a year."),
           need(input$reporter_mirrored, "Please select a reporter"),
           need(input$flow_mirrored, "Please select a flow"),
           need(mirroring_values$data_full, ""))
  
  if(input$commodity_source == "isscfc_code") {
    
    comm_list <- mirroring_values$data_full[year == input$year_mirrored & flow == input$flow_mirrored, unique(isscfc_code)]
    
    isscfc_dt <- GetCodeList(domain = 'FisheriesCommodities', 
                             dataset = 'commodities_total',
                             dimension = 'measuredItemISSCFC',
                             codes = comm_list)[, list(code, description)]
    
    comm_list <- isscfc_dt$code
    names(comm_list) <- isscfc_dt$description  
  }
  
  if(input$commodity_source == "tariff_line") {
    
    comm_dt <- mirroring_values$data_full[year == input$year_mirrored & flow == input$flow_mirrored, list(tariff_line, tariff_line_descr)]
    comm_dt <- unique(comm_dt)
    comm_dt[is.na(tariff_line_descr), tariff_line_descr := ""]
    
    comm_dt <- comm_dt[!is.na(tariff_line)]
    
    comm_list <- comm_dt$tariff_line
    names(comm_list) <- comm_dt$tariff_line_descr  
    
  }
  
  if(input$commodity_source == "scheda") {
    comm_dt <- mirroring_values$data_full[year == input$year_mirrored & flow == input$flow_mirrored, list(scheda, remark)]
    comm_dt <- unique(comm_dt)
    comm_dt[is.na(remark), remark := ""]
    
    comm_dt <- comm_dt[!is.na(scheda)]
    
    comm_list <- comm_dt$scheda
    names(comm_list) <- comm_dt$remark
    
  }
  
  pickerInput(inputId = "commodity_mirrored", 
              label = "Commodity", 
              choices = unname(comm_list), 
              options = list(`live-search` = TRUE),
              choicesOpt = list(subtext = paste(" ", names(comm_list))))
  
})

dataPlot <- reactive({
  
  validate(need(input$year_mirrored, "Please select a year."),
           # need(input$reporter_mirrored, "Please select a reporter"),
           need(mirroring_values$data_full, ""),
           need(input$flow_mirrored, ""),
           need(input$commodity_mirrored, ""))
  
  
  p_year     <- isolate(input$year_mirrored)
  # p_reporter <- isolate(input$reporter_mirrored)
  p_comm     <- isolate(input$commodity_mirrored)
  p_flow     <- isolate(input$flow_mirrored)
  p_commodity_source <- isolate(input$commodity_source)
  
  data_plot <- mirroring_values$data_full[flow == p_flow & get(p_commodity_source) == p_comm, ]
  
  if(nrow(data_plot) == 0) return(NULL)
  
  data_plot[is.na(weight_correction), weight_correction := weight]
  data_plot[is.na(value_correction), value_correction := as.numeric(value)]
  
  data_plot <- data_plot[, list(value = sum(value_correction),
                                weight = sum(weight_correction)), 
                         by = c("year", p_commodity_source)]
  
  data_plot <- data_plot %>%
    dplyr::mutate(uv = value / weight, na_flag = is.na(value) & is.na(weight)) %>%
    tidyr::complete(year = as.character(min(c(year, 2000)):max(year)), fill = list(value = 0, weight = 0, uv = 0, na_flag = T)) %>%
    tidyr::fill_(fill_cols = p_commodity_source, .direction = "down") %>%
    tidyr::fill_(fill_cols = p_commodity_source, .direction = "up") %>%
    setDT()
  
  data_plot
})

##-- Time Series ----
output$gg_ts_mirrored <- renderPlotly({
  
  validate(need(input$year_mirrored, "Please select a year."),
           need(input$reporter_mirrored, "Please select a reporter"),
           need(mirroring_values$data_full, ""),
           need(input$flow_mirrored, ""),
           need(input$commodity_mirrored, ""))
  
  p_year     <- isolate(input$year_mirrored)
  # p_reporter <- isolate(input$reporter_mirrored)
  p_comm     <- isolate(input$commodity_mirrored)
  p_flow     <- isolate(input$flow_mirrored)
  p_commodity_source <- isolate(input$commodity_source)
  
  data_plot <- dataPlot()
  
  if(is.null(data_plot)) return(NULL)
  
  criteria_viz <- input$criteria_viz_mirror
  yaxis_title <- ifelse(criteria_viz == "uv", toupper(criteria_viz), stringr::str_to_title(criteria_viz))
  
  if(p_commodity_source == "isscfc_code") {
    chartTitle <- GetCodeList(domain = 'FisheriesCommodities', 
                              dataset = 'commodities_total',
                              dimension = 'measuredItemISSCFC',
                              codes = p_comm)[, list(code, description)]  
    
    chartTitle <- paste0(chartTitle$code, ": <b>", chartTitle$description, "</b>")
  } 
  
  if(p_commodity_source == "tariff_line"){
    
    chartTitle <- mirroring_values$data_full[flow == p_flow & tariff_line == p_comm, list(tariff_line, tariff_line_descr)]
    chartTitle <- paste0(chartTitle$tariff_line[1], ": <b>", chartTitle$tariff_line_descr[1], "</b>")
    
  }
  
  if(p_commodity_source == "scheda"){
    
    chartTitle <- mirroring_values$data_full[flow == p_flow & scheda == p_comm, list(scheda, remark)]
    chartTitle <- paste0(chartTitle$scheda[1], ": <b>", chartTitle$remark[1], "</b>")
    
  }
  
  if(uniqueN(data_plot[[p_commodity_source]]) < 3) {
    p <- plot_ly(data_plot, x = ~year, y = ~get(criteria_viz), 
                 colors = RColorBrewer::brewer.pal(3, "Set2"),
                 name = p_comm,
                 type = 'scatter',
                 source = "ts_mirror", 
                 mode = 'lines+markers')  
  } else {
    p <- plot_ly(ts_plot, x = ~year, y = ~get(paste0(yaxis_title," - Original")), 
                 color = ~input, 
                 type = 'scatter',
                 source = "ts_outlier", 
                 mode = 'lines+markers')  
  }
  
  p <- p %>%
    add_trace(data = data_plot[na_flag == TRUE], 
              marker = list(symbol = 'cross', size = 8),
              name = "NA converted to zero",
              showlegend = T, 
              mode = "markers") %>%
    layout(annotations = list(text = chartTitle,
                              font = list(size = 14),
                              showarrow = FALSE,
                              xref = 'paper', x = 0,
                              yref = 'paper', y = 1.05),
           yaxis = list(title = yaxis_title),
           xaxis = list(title = "Year"),
           showlegend = T)
  
  p$elementId <- NULL
  p
  
})

##-- Table ----
output$tblMirror <- renderRHandsontable({
  
  validate(need(input$year_mirrored, "Please select a year."),
           # need(input$reporter_mirrored, "Please select a reporter"),
           need(mirroring_values$data_full, ""),
           need(input$flow_mirrored, ""),
           need(input$commodity_mirrored, ""))
  
  p_year     <- isolate(input$year_mirrored)
  p_comm     <- isolate(input$commodity_mirrored)
  p_flow     <- isolate(input$flow_mirrored)
  p_commodity_source <- isolate(input$commodity_source)
  
  data_show <- mirroring_values$data_full[flow == p_flow & get(p_commodity_source) == p_comm, ]
  data_show <- data_show[order(-year), ]
  
  data_show[, case := NULL]
  
  measuredItemISSCFC <- GetCodeList(domain = 'FisheriesCommodities', 
                                    dataset = 'commodities_total',
                                    dimension = 'measuredItemISSCFC')[, list(isscfc_code = code, isscfc_descr = description)]  
  
  data_show <- merge(data_show, measuredItemISSCFC, by = "isscfc_code", all.x = T)
  
  data_show <- setDT(faoswsTrade::add_area_names(data_show, code_class = "m49"))
  
  data_show <- data_show[order(-year), 
                         list(year, reporter, reporter_name, flow, partner, partner_name, 
                              tariff_line, tariff_line_descr, 
                              isscfc_code, isscfc_descr,
                              scheda, remark, 
                              value, weight, value_correction, weight_correction, 
                              imputation_method, mirrored)]
  
  mirroring_values$tblMirror <- data_show
  
  rhandsontable(mirroring_values$tblMirror) %>%
    hot_cols(columnSorting = F) %>%
    hot_row(row = which(data_show$year != p_year), readOnly = TRUE) %>%
    hot_col(col = c(1:11, 13:14, 17:ncol(data_show)), readOnly = T) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

##--+ Save Data ----
##** Launch a popup to ask confirmation to the user
observeEvent(input$save_mirroring_correction, {
  
  fao_confirmSweetAlert(
    session = session,
    inputId = "save_mirroring_correction_confirm",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Save the data may take a while.",
    danger_mode = TRUE, 
    html = T, 
    p_selector = "#placeholder_mirroring"
  )
  
})

observeEvent(input$save_mirroring_correction_confirm, {
  
  if(input$save_mirroring_correction_confirm == FALSE) return(NULL)
  
  if(is.null(input$tblMirror)) {
    
    sendSweetAlert(
      session = session,
      title = "Error!",
      text = "There is no data to be sabed.",
      type = "error"
    )
    
  }
  
  ##** Progress bar starts 
  progressSweetAlert(
    session = session, id = "save_mirroring_correction_progress",
    title = "Save in progress",
    display_pct = TRUE, value = 0
  )
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "save_mirroring_correction_progress",
    value = 15
  )
  ## get current table that is showing to the user
  tblMirror <- hot_to_r(input$tblMirror)
  
  p_reporter <- tblMirror[, unique(reporter)]
  p_commodity_source <- input$commodity_source
  p_commodity_code <- input$commodity_mirrored
  p_flow <- input$flow_mirrored
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "save_mirroring_correction_progress",
    value = 34
  )
  
  ## query to get that stored in the  SWS
  query <- sprintf("rep = '%s' AND %s = '%s' AND flow = %s", p_reporter, p_commodity_source, p_commodity_code, p_flow)
  dt_saved <- ReadDatatable("fishtrade_mirrored", where = query)
  setnames(dt_saved, c("rep", "prt"), c("reporter", "partner"))
  
  ## original column order
  col_order <- names(dt_saved)
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "save_mirroring_correction_progress",
    value = 68
  )
  
  ## updating data to be saved
  dt_saved <- merge(dt_saved, tblMirror, by = c("reporter", "partner", p_commodity_source, "flow"), suffixes = c("_saved", "_new"), all.x = TRUE)
  
  dt_saved[, `:=`(value_correction_saved = value_correction_new,
                  weight_correction_saved = weight_correction_new,
                  remark_saved = remark_new,
                  imputation_method_saved = imputation_method_new,
                  mirrored_saved = mirrored_new,
                  accepted = TRUE)]
  
  setnames(dt_saved, names(dt_saved), gsub("_saved", "", names(dt_saved)))
  dt_saved <- dt_saved[, col_order, with = FALSE]
  
  ## updating current data
  data_full <- copy(mirroring_values$data_full)
  cols_data_full <- names(data_full)

  dt_saved$toSave <- TRUE
  data_full <- merge(data_full, dt_saved, c("reporter", "partner", p_commodity_source, "flow"), suffixes = c("_saved", "_new"), all.x = TRUE)
  
  data_full[toSave == TRUE, `:=`(value_correction_saved = value_correction_new,
                                 weight_correction_saved = weight_correction_new,
                                 remark_saved = remark_new,
                                 imputation_method_saved = imputation_method_new,
                                 mirrored_saved = mirrored_new,
                                 accepted = TRUE)]
  
  data_full[, toSave := NULL]
  setnames(data_full, names(data_full), gsub("_saved", "", names(data_full)))
  mirroring_values$data_full <- data_full[, cols_data_full, with = F]
  rm(data_full)
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "save_mirroring_correction_progress",
    value = 85
  )
  
  ## saving back to SWS the data updated
  setnames(dt_saved, c("reporter", "partner"), c("rep", "prt"))
  writeDT(dt_name = "fishtrade_mirrored", data = dt_saved, sql_filter = query)
  
  ##** updating progress bar 15
  updateProgressBar(
    session = session,
    id = "save_mirroring_correction_progress",
    value = 100
  )
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Great!",
    text = "The data has been saved successfully.",
    type = "success"
  )
  
})


##-- update changes on table ----
observeEvent(input$tblMirror, {
  
  if(is.null(input$tblMirror)) return(NULL)
  if(is.null(input$tblMirror$changes$changes)) return(NULL)
  
  tblMirror <- hot_to_r(input$tblMirror)
  
  col_check_changes <- which(names(tblMirror) %in% c("value_correction", "weight_correction")) - 1
  if(!input$tblMirror$changes$changes[[1]][[2]] %in% col_check_changes) return(NULL)
  
  idx_row   <- sapply(input$tblMirror$changes$changes, "[[", 1) + 1
  idx_col   <- input$tblMirror$changes$changes[[1]][[2]]
  
  ## updating the imputation method and remark fields
  tblMirror[idx_row, `:=`(imputation_method = c("manual_imputation"), mirrored = FALSE)]
  # tblMirror[idx_row, remark := ifelse(mirrored == TRUE, "", remark)]
  
  mirroring_values$tblMirror <- tblMirror
  
  ## updating current data
  data_full <- copy(mirroring_values$data_full)
  cols_data_full <- names(data_full)
  
  tblMirror$toSave <- TRUE
  data_full <- merge(data_full, tblMirror, c("year", "reporter", "partner", input$commodity_source, "flow"), suffixes = c("_saved", "_new"), all.x = TRUE)
  
  ## updating the imputation method, remark, and mirrored fields at full table
  data_full[toSave == TRUE, `:=`(value_correction_saved = value_correction_new,
                                 weight_correction_saved = weight_correction_new,
                                 remark_saved = remark_new,
                                 imputation_method_saved = imputation_method_new,
                                 mirrored_saved = mirrored_new,
                                 accepted = TRUE)]
  
  data_full[, toSave := NULL]
  setnames(data_full, names(data_full), gsub("_saved", "", names(data_full)))
  mirroring_values$data_full <- data_full[, cols_data_full, with = F]
  rm(data_full)
  
})

##'TODO: update imputation method when the user changes the value or weight, as well as the flag mirrored 













