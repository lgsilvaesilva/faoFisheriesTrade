data_viz_values <- reactiveValues()

observe({
  
  list_country <- ReadDatatable('fishtrade_reporter_workflow')
  stages_list <- c(names(list_country)[-c(1:2)])
  
  if(nrow(list_country) == 0) return(NULL)
  
  tables_stages <- data.table(stages = stages_list, 
                              tables = c(NA_character_, 
                                         NA_character_, 
                                         "fishtrade_built_data", 
                                         "fishtrade_data_imputed", 
                                         "fishtrade_data_imputed_validated", 
                                         "fishtrade_outlier_corrected", 
                                         "fishtrade_outlier_validated",
                                         "fishtrade_mirrored", 
                                         NA_character_), 
                              tables_label = c(NA_character_,
                                               NA_character_,
                                               "Data mapped",
                                               "Data imputed (not validated)",
                                               "Data imputed (validated)",
                                               "Outlier detection was run (not validated)",
                                               "Outlier detection was run (validated)",
                                               "Mirroring process was run (not validated)",
                                               "Mirroring process was run (validated)"))
  
  rep_status <- melt(list_country, 1:2, variable.name = "stages")
  rep_status <- rep_status[value == 1]
  rep_status[, stages := factor(stages, levels = stages_list, ordered = TRUE)]
  rep_status <- rep_status[stages >= "is_mapped_data"] # only those countries mapped
  
  if(nrow(rep_status) == 0) return(NULL)
  
  rep_status <- rep_status[order(year, rep, stages), list(stages = last(stages)), by = c("year", "rep") ]
  
  rep_status <- merge(rep_status, tables_stages, by = "stages")
  
  rep_mirrored <- unique(ReadDatatable("fishtrade_mirrored", where = "mirrored = 't'", columns = c("year", "rep", "mirrored")))
  
  rep_mirrored <- rep_mirrored[, `:=`(stages = "is_run_mirroring",
                                      tables = "fishtrade_mirrored",
                                      tables_label = "Mirroring process was run (not validated)")]
  
  rep_status <- rbindlist(list(rep_status, rep_mirrored), fill = TRUE)
  rep_status[is.na(mirrored), mirrored := FALSE]
  
  data_viz_values$rep_status <- rep_status
  
})

##-- Year ----
observe({
  
  if(input$navbar != "data_viz") return(NULL)
  
  validate(need(data_viz_values$rep_status, "There is no data to be shown."))
  
  year_choices <- data_viz_values$rep_status[, unique(year)]
  
  updatePickerInput(session = session, 
                    inputId = "btn_year_viz",
                    choices = sort(year_choices, decreasing = T))
  
})

##-- Reporter ----
# output$btn_country_table <- renderUI({
observeEvent(c(input$btn_year_viz, input$is_mirrored_data_viz), {
  
  if(input$navbar != "data_viz") return(NULL)
  
  # validate(need(data_viz_values$rep_status, "There is no data to be shown."),
  #          need(input$btn_year_viz, ""))
  
  if(input$is_mirrored_data_viz == "all") {
    
    reporter_choices <- data_viz_values$rep_status[year == input$btn_year_viz, unique(rep)]
    names(reporter_choices) <- m49_codes[reporter %in% reporter_choices, country]
    
  } else {
    
    reporter_choices <- data_viz_values$rep_status[year == input$btn_year_viz & mirrored == input$is_mirrored_data_viz, unique(rep)]
    names(reporter_choices) <- m49_codes[reporter %in% reporter_choices, country]
    
  }
  
  updatePickerInput(session = session,
                    inputId = "btn_reporter_viz", 
                    choices = reporter_choices)
  
  # pickerInput(inputId = "btn_reporter_viz", 
  #             label = "Reporter", 
  #             choices = reporter_choices, options = list(`live-search` = TRUE))
  
})

##-- Build full data ----
observeEvent(input$run_summary_data_table, {
  
  confirmSweetAlert(
    session = session,
    inputId = "run_summary_data_table_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Aggregation process could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

observeEvent(input$run_summary_data_table_confirmation, {
  
  validate(need(input$btn_reporter_viz, "Please select a reporter."),
           need(input$btn_year_viz, "Please select a year"))
  
  if(input$run_summary_data_table_confirmation == FALSE) return(FALSE)
  
  p_country <- input$btn_reporter_viz
  p_year    <- input$btn_year_viz
  p_source  <- data_viz_values$rep_status[rep == p_country & year == p_year, tables]
  
  ##** Progress bar starts 
  progressSweetAlert(
    session = session, id = "run_summary_progress",
    title = "Aggregation in progress",
    display_pct = TRUE, value = 0
  )
  
  updateProgressBar(
    session = session,
    id = "run_summary_progress",
    value = 15
  )
  
  ##-- Get the data for the current year
  sql_data <- sprintf("rep = '%s' OR rep = '%s' AND year = '%s'", p_country, 
                      stringr::str_pad(p_country, 3, "left", "0"), p_year)
  
  data_stage <- ReadDatatable(p_source, where = sql_data)
  
  if("tariff_line" %in% names(data_stage)) {
    data_stage <- data_stage[, list(year, rep, flow, prt, comm = tariff_line, value, weight)]  
  } else {
    data_stage <- data_stage[, list(year, rep, flow, prt, comm, value, weight)]
  }
  
  data_stage[, rep := as.integer(rep)]
  data_stage[, prt := as.integer(prt)]
  
  updateProgressBar(
    session = session,
    id = "run_summary_progress",
    value = 55
  )
  
  ##-- Mapping commodities
  mapping_table <- ReadDatatable('fishtrade_trade_map', where = paste0("rep = ", p_country))
  data_stage <- map_trade(.data = data_stage, mapping_table)
  
  setnames(data_stage, c("rep", "prt", "comm"), c("reporter", "partner", "tariff_line"))
  data_stage[, fao_group := substr(scheda, 1, 1)]
  
  ##** Get the historical data ----
  sql_rep_hist <- sprintf("rep = '%s' AND year <> '%s' AND value_usd IS NOT NULL", 
                          p_country, p_year)
  
  rep_hist <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist)
  setnames(rep_hist, "isscfc", "isscfc_code")
  
  updateProgressBar(
    session = session,
    id = "run_summary_progress",
    value = 85
  )
  
  ##** Mapping historical data 
  isscfc_table <- unique(mapping_table[isscfc_code != -1, list(isscfc_code, isscfc_id, isscfc_descr)])
  isscfc_table <- isscfc_table[, list(isscfc_descr = isscfc_descr[!is.na(isscfc_descr)]), by = c("isscfc_code", "isscfc_id")]
  rep_hist <- merge(rep_hist, isscfc_table, by = c("isscfc_code", "isscfc_id"), all.x = T)
  setnames(rep_hist, c("rep", "remarks", "qty"), c("reporter", "remark", "weight"))
  
  rep_hist[, c("qunit", "qty_mt", "qty_flag", "value_unit", "value_flag") := NULL]
  rep_hist <- rep_hist[, list( year, reporter, flow, fao_group, scheda, isscfc_code, tariff_line, tariff_line_descr, isscfc_descr, weight, value = value_usd)]
  
  ##-- Combine current and historical data
  data_stage <- data_stage[, list( year, reporter, flow, partner, fao_group, scheda, isscfc_code, tariff_line, tariff_line_descr, isscfc_descr, weight, value)]
  data_full <- rbindlist(l = list(rep_hist, data_stage), fill = TRUE, idcol = "case")
  data_full <- data_full[isscfc_code != -1]
  
  data_full[, `:=`(weight = as.numeric(weight),
                   value = as.numeric(value),
                   reporter = as.integer(reporter))]
  
  data_full[, `:=`(
    hs6 = substr(tariff_line, 1, 6),
    hs4 = substr(tariff_line, 1, 4),
    hs2 = substr(tariff_line, 1, 2)
  )]
  
  updateProgressBar(
    session = session,
    id = "run_summary_progress",
    value = 95
  )
  
  ##-- Partners label ----
  data_full <- merge(data_full, m49_codes, by.x = "partner", by.y = "reporter", all.x = TRUE)
  setnames(data_full, "country", "Partner")
  
  ##-- Reporter label ----
  data_full <- merge(data_full, m49_codes, by.x = "reporter", by.y = "reporter", all.x = TRUE)
  setnames(data_full, "country", "Reporter")
  
  updateProgressBar(
    session = session,
    id = "run_summary_progress",
    value = 100
  )
  
  closeSweetAlert(session = session)
  
  data_viz_values$data_full <- data_full
  
})

##-- Table ----
output$tblYearBook <- renderDataTable({
  
  validate(#need(input$btn_reporter_viz, "Please select a reporter."),
    #need(input$btn_year_viz, "Please select a year"), 
    need(data_viz_values$data_full, ""))
  
  group_by <- unique(c("year", "flow", input$group_agg_data_viz))
  group_summary <- data_viz_values$data_full[, list(weight = sum(weight, na.rm = TRUE), 
                                                    value = sum(value, na.rm = TRUE), 
                                                    na_weight = sum(is.na(weight)), 
                                                    na_value = sum(is.na(value)), N = .N), 
                                             by = group_by][, `:=`(uv, value/weight)]
  
  group_summary <- group_summary[year >= (max(as.numeric(year)) - 8), ]
  group <- input$group_agg_data_viz
  
  if (group == "fao_group") {
    yearbook_groups <- 1:7
    yearbook_total <- group_summary[fao_group %in% yearbook_groups, list(fao_group = "YB Total", value = sum(value), weight = sum(weight)),
                                    by = c("year", "flow")]
    yearbook_total[, `:=`(uv, value/weight)]
    group_summary <- rbindlist(list(yearbook_total, group_summary), fill = T)
  }
  
  if(group == "flow") {
    
    group_summary[, group := "Total"]
    
    group_summary[, `:=`(value = scales::comma(round(value)), 
                         weight = scales::comma(round(weight)), 
                         uv = round(uv, 2))]
    
    group_summary[, `:=`(flow, flow_labels[flow])]
    
    uv_group <- dcast(group_summary, flow+group~year, value.var = "uv")
    uv_group[, `:=`(variable, "uv")]
    
    vl_group <- dcast(group_summary, flow+group~year, value.var = "value")
    vl_group[, `:=`(variable, "value")]
    
    wt_group <- dcast(group_summary, flow+group~year, value.var = "weight")
    wt_group[, `:=`(variable, "weight")]
    
    group_table <- rbindlist(list(uv_group, vl_group, wt_group))
    group_table <- group_table[order(flow, group)]
    
    group_labels <- c(Partner = "Partner", HS6 = "hs6", HS4 = "hs4", 
                      HS2 = "hs2", `Partner Code` = "partner", `FAO Group` = "fao_group", 
                      ISSCFC = "isscfc_code", Reporter = "reporter", Flow = "flow", 
                      Scheda = "scheda", `Tariff Line` = "tariff_line")
    
    group_table[, `:=`(flow = as.factor(flow), variable = as.factor(variable))]
    
    group_table$id_grp <- rleidv(group_table, cols = "flow")
    group_table$id_grp <- group_table$id_grp %% 2
    
    group_table %>% 
      select(Flow = flow, 1, Criteria = variable, Group = group, everything()) %>% 
      datatable(#extensions = "RowGroup", 
        rownames = FALSE, 
        filter = "top", 
        class = "table-bordered table-condensed", 
        options = list(pageLength = 10, 
                       columnDefs = list(list(visible = FALSE, targets = ncol(group_table) - 1))
                       # rowGroup = list(dataSrc = 0)
        )
      ) %>%
      formatStyle(columns = "id_grp", 
                  target = "row", 
                  backgroundColor = styleEqual(c(0, 1), c('#F2F2F2', 'white')))
    
  } else {
    
    group_total <- group_summary[, list(group = "Grand Total", value = sum(value), weight = sum(weight)), by = c("year", "flow")]
    group_total[, `:=`(uv, value/weight)]
    
    setnames(group_summary, input$group_agg_data_viz, "group")
    
    group_summary <- rbindlist(list(group_total, group_summary), fill = T)
    group_summary[, `:=`(value = scales::comma(round(value)), weight = scales::comma(round(weight)), uv = round(uv, 2))]
    
    group_summary[, `:=`(flow, flow_labels[flow])]
    
    uv_group <- dcast(group_summary, group + flow ~ year, value.var = "uv")
    uv_group[, `:=`(variable, "uv")]
    
    vl_group <- dcast(group_summary, group + flow ~ year, value.var = "value")
    vl_group[, `:=`(variable, "value")]
    
    wt_group <- dcast(group_summary, group + flow ~ year, value.var = "weight")
    wt_group[, `:=`(variable, "weight")]
    
    group_table <- rbindlist(list(uv_group, vl_group, wt_group))
    group_table <- group_table[order(flow, group)]
    
    group_labels <- c(Partner = "Partner", 
                      `Partner Code` = "partner", 
                      Reporter = "Reporter", 
                      `Reporter Code` = "reporter", 
                      `FAO Group` = "fao_group", 
                      ISSCFC = "isscfc_code", 
                      Scheda = "scheda", 
                      Flow = "flow", 
                      `Tariff Line` = "tariff_line",
                      HS6 = "hs6", HS4 = "hs4", HS2 = "hs2")
    
    group_table <- group_table[!is.na(group)]
    names(group_table)[1] <- names(group_labels[group_labels == input$group_agg_data_viz])
    
    group_table[, `:=`(flow = as.factor(flow), variable = as.factor(variable))]
    
    group_table$id_grp <- rleidv(group_table, cols = c(names(group_labels[group_labels == input$group_agg_data_viz]), "flow"))
    group_table$id_grp <- group_table$id_grp %% 2
    
    col2hide <- which(names(group_table) %in% "id_grp")
    
    group_table %>% 
      select(Flow = flow, 1, Criteria = variable, everything()) %>% 
      datatable(#extensions = "RowGroup", 
        rownames = FALSE, 
        filter = "top", 
        class = "table-bordered table-condensed", 
        options = list(pageLength = 100,
                       columnDefs = list(list(visible = FALSE, targets = col2hide - 1))
                       # rowGroup = list(dataSrc = 1)
        )
      ) %>%
      formatStyle(columns = "id_grp",
                  target = "row",
                  backgroundColor = styleEqual(c(0, 1), c('#F2F2F2', 'white')))
  }
  
})

##-- Reporter Status ----

output$textReporterStatus <- renderText({
  
  validate(need(data_viz_values$rep_status, "There is no data to be shown."))
  input$btn_reporter_viz
  msg <- data_viz_values$rep_status[rep == input$btn_reporter_viz & year == input$btn_year_viz, tables_label]
  
})












