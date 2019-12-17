
observeEvent(c(input$reporter_map), priority = 1, {
  input$tab_mapping
  sql_filter <- sprintf("rep = '%s'", input$reporter_map)
  dt <- ReadDatatable(fishtrade_table_to_map, where = sql_filter)
  setnames(dt, "rep", "reporter")
  dt[, id := 1:.N]
  values$data_full <- dt
  
})


##-- Reporter button ----
observe({
  
  input$run_map
  reporters_built <- run_mapping_values$ft_workflow[
    is_built_data == 1 &  # data is already built, and
      is_run_mapping == 1, #& # the mapping process is already run, but
    # is_mapped_data %in% c(0, 1),  # once the country-year was mapped, should I show it in the list of countries to be mapped?
    unique(rep)]
  
  reporters_built <- m49_codes[reporter %in% reporters_built, ]
  rep2map_list <- reporters_built$reporter
  names(rep2map_list) <- reporters_built$country
  
  updatePickerInput(session = session,
                    inputId = "reporter_map", 
                    choices = rep2map_list)
})
# output$btn_reporter_map <- renderUI({
#   # input$run_map
#   reporters_built <- run_mapping_values$ft_workflow[
#     is_built_data == 1 &  # data is already built, and
#       is_run_mapping == 1, #& # the mapping process is already run, but
#     # is_mapped_data %in% c(0, 1),  # once the country-year was mapped, should I show it in the list of countries to be mapped?
#     unique(rep)]
#   
#   reporters_built <- m49_codes[reporter %in% reporters_built, ]
#   rep2map_list <- reporters_built$reporter
#   names(rep2map_list) <- reporters_built$country
#   
#   pickerInput(inputId = "reporter_map", 
#               label = "Reporter:", 
#               choices = rep2map_list,
#               options = list(`live-search` = TRUE))
# })

##-- Trade Map table ----
observeEvent(c(input$reporter_map, input$codes_map, input$save_map), {
  
  validate(need(input$reporter_map, "Please select at least a country."))
  if(nrow(values$data_full) == 0) return(NULL)
  
  reporter_input <- input$reporter_map
  
  if (input$reporter_map != "all") {
    
    data_tm_descr <- values$data_full[reporter == reporter_input, ]
    
  } else {
    
    data_tm_descr <- values$data_full
    
  }
  
  if (input$codes_map == "unmapped") {
    
    data_tm_descr <- data_tm_descr[unmapped == 1, ]
  }
  
  # if (input$codes_map == "mapped") {
  #   
  #   data_tm_descr <- data_tm_descr[unmapped == 0]
  #   
  # }
  
  if (input$codes_map == "to_improve") {
    
    data_tm_descr <- data_tm_descr[toimprove == 1]
    
  }
  
  if (input$codes_map == "hs6_use") {
    
    data_tm_descr <- data_tm_descr[hs6_use == 1 & toimprove != 1]
    
  }
  
  data_tm_descr[, fao_group := substr(scheda, 1, 1)]
  data_tm_descr <- as.data.frame(data_tm_descr[, list(id,  
                                                      reporter, 
                                                      country, 
                                                      tariff_line, 
                                                      flow, 
                                                      isscfc_code, 
                                                      isscfc_id,
                                                      isscfc_descr,
                                                      remark,
                                                      fao_group,
                                                      scheda,
                                                      tariff_line_descr,
                                                      startyear, 
                                                      endyear,
                                                      hs6_use)])
  values$data_mapping <- data_tm_descr
})

##** Show trade mapping table ----
output$trademap <- renderRHandsontable({
  
  validate(need(input$reporter_map, "Please select at least a country and/or click on the button 'Refresh list'."))
  
  if(length(values$data_mapping) == 0) return(NULL)
  input$save_map
  data_mapping <- as.data.table(values$data_mapping)
  data_mapping <- data_mapping[order(reporter, tariff_line, flow, isscfc_code), ]
  data_mapping[, flow := flow_labels[flow]]
  data_mapping <- as.data.frame(data_mapping)
  # data_mapping[, isscfc_id := stringr::str_pad(string = isscfc_id, width = 4, side = 'left', pad = '0')]
  
  rhandsontable(data_mapping, selectCallback = TRUE) %>%
    hot_cols(columnSorting = TRUE, manualColumnMove = FALSE, manualColumnResize = TRUE) %>%
    hot_col(col = 'id', readOnly = T) %>%
    hot_col(col = 'reporter', readOnly = T) %>%
    hot_col(col = 'country', readOnly = T) %>%
    hot_col(col = 'tariff_line', readOnly = T) %>%
    hot_col(col = 'flow', readOnly = T) %>%
    hot_col(col = 'isscfc_descr', colWidths = 500) %>%
    hot_col(col = 'remark', colWidths = 250) %>%
    hot_col(col = 'fao_group', readOnly = T) %>%
    hot_col(col = 'scheda', readOnly = T) %>%
    hot_col(col = 'tariff_line_descr', colWidths = 500) %>%
    hot_col(col = 'isscfc_code', colWidths = 100) %>%
    hot_col(col = 'isscfc_id', 
            type = "autocomplete", 
            source = unique(inter_code_map$internal_code), 
            strict = FALSE) %>%
    hot_col(col = 'hs6_use', readOnly = T) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

##** Graphics to summary the missing codes ----
gg_df_check <- reactive({
  
  gg_df <- values$data_full
  
  if(nrow(values$data_full) == 0) return(NULL)
  
  gg_df <- gg_df[, .(missing_rows = sum(isscfc_code == "" | is.na(isscfc_code)),
                     missing_commodity = uniqueN(tariff_line[isscfc_code == "" | is.na(isscfc_code)]),
                     tot_rows = .N,
                     tot_commodity = uniqueN(tariff_line)), country]
  
  gg_df$country <- as.character(gg_df$country)
  
  gg_missing_rows_perc <- gg_df[, list(Missing = missing_rows/tot_rows), country][, Filled := 1 - Missing]
  gg_missing_rows_perc <- melt.data.table(gg_missing_rows_perc, 1)
  gg_missing_commodity_perc <- gg_df[, list(Missing = missing_commodity/tot_commodity), country][, Filled := 1 - Missing]
  gg_missing_commodity_perc <- melt.data.table(gg_missing_commodity_perc, 1)
  
  list(gg_df = gg_df, 
       gg_missing_rows_perc = gg_missing_rows_perc, 
       gg_missing_commodity_perc = gg_missing_commodity_perc)
})

output$gg_bar_missing_rows <- renderPlot({
  
  if(nrow(values$data_full) == 0) return(NULL)
  
  ggplot(data = gg_df_check()$gg_df, aes(x = country, missing_rows)) +
    geom_bar(stat = "identity", fill = "#313945") +
    geom_text(aes(label = missing_rows), 
              hjust = 1.2, 
              vjust = 1.1,
              color = "gray75") +
    labs(y = "# of missing rows", x = "") +
    theme_minimal() +
    coord_flip()
  
})

output$gg_bar_missing_commotidy <- renderPlot({
  
  if(nrow(values$data_full) == 0) return(NULL)
  
  ggplot(data = gg_df_check()$gg_df, aes(x = country, missing_commodity)) +
    geom_bar(stat = "identity", fill = "#313945") +
    geom_text(aes(label = missing_commodity), 
              hjust = 1.2, 
              vjust = 1.1,
              color = "gray75") +
    labs(y = "# of missing commodities", x = "") +
    theme_minimal() +
    coord_flip()
  
})  

output$gg_bar_missing_rows_perc <- renderPlot({
  
  if(nrow(values$data_full) == 0) return(NULL)
  
  ggplot(data = gg_df_check()$gg_missing_rows_perc, aes(x = country, value, group = variable, fill = variable)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#BF3945", "#313945")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "% of missing rows", x = "", fill = "") +
    theme_minimal() +
    theme(legend.position = "right") +
    coord_flip()
})

output$gg_bar_missing_commodity_perc <- renderPlot({
  
  if(nrow(values$data_full) == 0) return(NULL)
  
  ggplot(data = gg_df_check()$gg_missing_commodity_perc, aes(x = country, value, group = variable, fill = variable)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#BF3945", "#313945")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "% of missing commodities", x = "", fill = "") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip()
  
})

##-- Add new mapping ----
output$add_row_table <- renderRHandsontable({
  
  data_row <- values$data_mapping[1, ]
  data_row <- melt(data_row, "id", variable.name = "Fields")[, -1]
  data_row$value[!data_row$Fields %in% c("reporter", "country")] <- ""
  
  rhandsontable(data_row, selectCallback = TRUE, height = '600px', width = '100%') %>%
    hot_cols(columnSorting = TRUE) %>%
    hot_col(col = 'Fields', readOnly = T, colWidths = 100) %>%
    hot_col(col = 'value', readOnly = F, colWidths = 500) %>%
    hot_cell(row = 1, col = 2, readOnly = T) %>%
    hot_cell(row = 2, col = 2, readOnly = T) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  
})

add_row_modal <- function(failed = FALSE) {
  
  modalDialog(
    
    rHandsontableOutput("add_row_table", height = '400px', width = '100%'),
    
    if (failed == TRUE) div(tags$b("Invalid mapping.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok_add_row", "OK")
    ),
    size = "l")
  
}

# Show modal when button is clicked.
observeEvent(input$add_mapping, {
  
  showModal(add_row_modal())
  
})

observeEvent(input$ok_add_row, {
  
  added_row <- hot_to_r(input$add_row_table)
  setDT(added_row)
  
  # Check fields
  comm_check <- added_row[Fields == "tariff_line", nchar(value) >= 6 & nchar(value) <= 12 & grepl(pattern = "^\\d+$", value)]
  flow_check <- added_row[Fields == "flow", nchar(value) == 1 & grepl(pattern = "^\\d+$", value)]
  isscfc_code_check <- added_row[Fields == "isscfc_code", nchar(value) > 1]
  isscfc_id_check   <- added_row[Fields == "isscfc_id", nchar(value) > 1]
  startyear_check   <- added_row[Fields == "startyear", nchar(value) == 4 & grepl(pattern = "^\\d+$", value)]
  endyear_check     <- added_row[Fields == "endyear", nchar(value) == 4 & grepl(pattern = "^\\d+$", value)]
  hs6_use_check     <- added_row[Fields == "hs6_use", value == 0 | value == 1]
  
  all_checks <- c(comm_check, 
                  flow_check, 
                  isscfc_code_check | isscfc_id_check,
                  startyear_check, 
                  endyear_check, 
                  hs6_use_check)
  
  if (all(all_checks)) {
    
    id_new_mapping <- max(as.numeric(values$data_full$id)) + 1
    id_new_mapping_dt <- data.table(Fields = "id", value = id_new_mapping)
    added_row <- rbind(id_new_mapping_dt, added_row)
    added_row <- dcast(added_row, 1~Fields)
    added_row[, . := NULL]
    ##-- Fill the isscfc code 
    isscfc_dt <- inter_code_map[isscfc_id == added_row$isscfc_id | isscfc_input == added_row$isscfc_code, ]
    
    if(nrow(isscfc_dt) != 0) {
      added_row[, `:=`(isscfc_code = isscfc_dt$isscfc_input, isscfc_id = isscfc_dt$isscfc_id, isscfc_descr = isscfc_dt$isscfc_descr_input)]
      fao_group_add <- fao_group_dt[isscfc_id == added_row$isscfc_id, fao_group]
      added_row[, fao_group := fao_group_add]
    } else {
      showModal(add_row_modal(failed = TRUE))
    }

    ##-- Scheda
    added_row[, scheda := NA_character_]
    added_row[, reporter := as.integer(reporter)]
    added_row[, flow := as.integer(flow)]
    added_row <- generate_scheda(.data = added_row)
    
    data_full <- rbind(values$data_full, added_row, fill = TRUE)
    data_full[id == id_new_mapping, `:=`(unmapped = 0, toimprove = 0)]
    
    values$data_full <- data_full
    
    removeModal()
    
  } else {
    showModal(add_row_modal(failed = TRUE))
  }
  
})

##-- Discard code ----
observeEvent(input$discard_code, {
  
  validate(need(input$trademap, "There is no data."))
  
  newtrademap <- hot_to_r(input$trademap)
  newtrademap$flow <- flow_labels_code[newtrademap$flow]
  
  ##-- Identify columns index
  isscfc_code_col <- which('isscfc_code' == unlist(input$trademap_select$params$rColnames))
  isscfc_id_col   <- which('isscfc_id' == unlist(input$trademap_select$params$rColnames))
  scheda_id_col   <- which('scheda' == unlist(input$trademap_select$params$rColnames))
  
  row_selected <- input$trademap_select$select$rAll
  
  ##-- Assign 0 to discarded codes 
  newtrademap[row_selected, c(isscfc_code_col, isscfc_id_col, scheda_id_col)] <- "-1"
  
  values$data_mapping <- newtrademap
  
})

##-- Internal Code TO ISSCFC + Description ----
observeEvent(input$use_isscfc, {
  
  validate(need(input$trademap, "There is no data."))
  
  newtrademap <- hot_to_r(input$trademap) %>%
    mutate(selected = 0) %>%
    setDT()
  
  newtrademap$flow <- flow_labels_code[newtrademap$flow]
  
  row_sel <- input$trademap_select$select$rAll
  ic_code_selected <- newtrademap[row_sel, isscfc_id]
  ic_code <- stringr::str_pad(string = ic_code_selected, width = 4, side = 'left', pad = '0')
  
  if (length(ic_code) == 0) {
    sendSweetAlert(
      session = session,
      title = "The internal code column is empty.",
      text = "Please fill out it and click on the button 'Use ISSCFC'.",
      type = "warning"
    )
    
    return(NULL)
  }
  
  #' keep the orignal order of columns that will be modified by the merge function
  order_names <- names(newtrademap)[-ncol(newtrademap)]
  order_rows  <- newtrademap$id
  
  #' flag variable to indicate which rows were selected by the user
  newtrademap[row_sel, selected := 1] 
  #' merge between the current table and the mapping table stored in SWS
  newtrademap <- merge(newtrademap, inter_code_map, by = c("isscfc_id"), all.x = T)
  
  #' Test if there are matches between the code typed by user and the mapping table
  n_matches <- newtrademap[selected == 1, all(!is.na(isscfc_input))]
  
  if (n_matches == 0) {
    
    code_not_match <- newtrademap[selected == 1 & is.na(isscfc_input), isscfc_id]
    
    sendSweetAlert(
      session = session,
      title = "A manual insertion is required",
      text = sprintf("No ISSCFC code is available for %s code(s)", toString(code_not_match)),
      type = "warning"
    )
    
  } else if (sum(n_matches) >= 1) {
    
    newtrademap[, `:=`(isscfc_code = ifelse(selected == 1, isscfc_input, isscfc_code),
                       isscfc_descr = ifelse(selected == 1, isscfc_descr_input, isscfc_descr))]
    
    #** get fao group ----
    isscfc_code_selected <- newtrademap[selected == 1, isscfc_code]
    fao_group_selected <- fao_group_dt[isscfc_code %in% isscfc_code_selected, fao_group]
    
    toGenScheda  <- copy(newtrademap)
    data_mapping <- setDT(copy(values$data_full))
    data_mapping <- merge(data_mapping, fao_group_dt, by = c("isscfc_id", "isscfc_code"), all.x = T)
    data_mapping <- data_mapping[, c("reporter", "flow", "tariff_line", "isscfc_code", "fao_group", "scheda"), with = F]
    
    toGenScheda <- merge(toGenScheda, 
                         data_mapping, 
                         by = c("reporter", "flow", "tariff_line", "isscfc_code", "fao_group"), 
                         suffixes = c("_new", "_old"),
                         all.x = T)
    
    if(toGenScheda[, any(is.na(scheda_old))] == TRUE | toGenScheda[, any(is.na(scheda_new))] == TRUE) {
      
      newtrademap[selected == 1, `:=`(fao_group = fao_group_selected, scheda = NA_character_)]
      newtrademap <- generate_scheda(.data = newtrademap)  
      
    } else {
      
      toGenScheda[scheda_new != scheda_old, scheda_new := scheda_old]
      toGenScheda[, scheda := scheda_new][, `:=`(scheda_new = NULL, scheda_old = NULL)]
      newtrademap <- copy(toGenScheda)
      rm(toGenScheda)
      rm(data_mapping)
    }
    
    ##** Update the whole table ----
    
    tradedata_mapped_descr <- merge(x  = values$data_full, 
                                    y  = newtrademap[id %in% order_rows[row_sel], 
                                                     list(id, isscfc_id, isscfc_code, isscfc_descr)], 
                                    by = 'id', all.x = T, suffixes = c("_old", "_new"))
    
    tradedata_mapped_descr[  is.na(isscfc_id_old) & 
                               (isscfc_code_old == "" | is.na(isscfc_code_old)) & 
                               is.na(isscfc_descr_old) & 
                               
                               !is.na(isscfc_id_new) & 
                               !is.na(isscfc_code_new) & 
                               !is.na(isscfc_descr_new), 
                             `:=`(isscfc_id_old = isscfc_id_new, 
                                  isscfc_code_old = isscfc_code_new,
                                  isscfc_descr_old = isscfc_descr_new)]
    
    old_var <- grep(pattern = "_old$", names(tradedata_mapped_descr), value = TRUE)
    new_var <- grep(pattern = "_new$", names(tradedata_mapped_descr), value = TRUE)
    without_old <- gsub("_old", "", old_var)
    setnames(tradedata_mapped_descr, old_var, without_old)
    
    tradedata_mapped_descr[, eval(parse(text = sprintf("`:=`(%s)", toString(sprintf("%s = NULL", new_var)))))]
    
    newtrademap <- newtrademap %>%
      select(-selected, -isscfc_input, -isscfc_descr_input) %>%
      arrange(id) %>%
      setDT()
    
    newtrademap <- setcolorder(newtrademap, order_names)
    
    values$data_mapping <- newtrademap
    
  } else {
    
    sendSweetAlert(
      session = session,
      title = "A manual insertion is required",
      text = "There is not a single HS6 code, thus you will need to add the code manually",
      type = "warning"
    )
    
  }
})


##-- Save Map ----
observeEvent(input$save_map, {
  # browser()
  validate(need(input$trademap, "There is no data."))
  
  map_sel <- hot_to_r(input$trademap) 
  map_sel$flow <- flow_labels_code[map_sel$flow]
  #'test if the dataset is empty
  n_rows <- nrow(map_sel)

  if(n_rows == 0) {
    
    save_msg <- sprintf("There is no data to be saved")
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE,
      title = save_msg,
      text = "Please try to select all codes using the button 'Codes'.",
      type = "info"
    )
    
    return(NULL)
    
  }
  
  progress_save <- shiny::Progress$new()
  progress_save$set(message = "Saving mapping table: ", value = 0)
  
  ##** Update values proposed by the user ----
  
  data_to_change <- merge(map_sel, values$data_full, 
                          by = c("reporter", "tariff_line", "flow"), 
                          suffixes = c("_proposed", "_current"), 
                          all = TRUE)
  
  #' Recode NA to ""
  data_to_change[is.na(data_to_change)] <- ""
  setDT(data_to_change)
  
  discarded_dt <- data_to_change[isscfc_code_proposed == -1]
  id_discarded <- discarded_dt$id_current
  
  discarded_dt <- discarded_dt[, 
                               list(id = max(id_current), 
                                    isscfc_code = -1,
                                    isscfc_id = -1,
                                    remark = min(remark_proposed),
                                    scheda =-1,
                                    startyear = min(c(startyear_proposed, startyear_current)),
                                    endyear = max(c(endyear_proposed, endyear_current)),
                                    hs6_use = 0,
                                    toimprove = 0,
                                    unmapped = 0), 
                               by = list(reporter = reporter, country = country_current, tariff_line = tariff_line, flow = flow)]
  
  
  #' Building data updated manually by user 
  data_to_change <- data_to_change[isscfc_code_proposed != -1, ]
  data_to_change[, 
                 `:=`(
                   id = id_current, reporter = reporter, country = country_current, tariff_line = tariff_line, flow = flow, 
                   isscfc_code = ifelse(!(is.na(isscfc_code_proposed) | isscfc_code_proposed == ""), 
                                        isscfc_code_proposed, 
                                        isscfc_code_current),
                   isscfc_id = ifelse(!(is.na(isscfc_id_proposed) | isscfc_id_proposed == ""), 
                                      isscfc_id_proposed, 
                                      isscfc_id_current),
                   isscfc_descr = ifelse(!(is.na(isscfc_descr_proposed) | isscfc_descr_proposed == ""), 
                                         isscfc_descr_proposed, 
                                         isscfc_descr_current),
                   remark = ifelse(!(is.na(remark_proposed) | remark_proposed == ""), 
                                   remark_proposed, 
                                   remark_current),
                   scheda = ifelse(!(is.na(scheda_proposed) | scheda_proposed == ""), 
                                   scheda_proposed, 
                                   scheda_current),
                   tariff_line_descr = ifelse(!(is.na(tariff_line_descr_proposed) | tariff_line_descr_proposed == ""), 
                                              tariff_line_descr_proposed, 
                                              tariff_line_descr_current),
                   startyear = ifelse(!(is.na(startyear_proposed) | startyear_proposed == ""), 
                                      startyear_proposed, 
                                      startyear_current),
                   endyear = ifelse(!(is.na(endyear_proposed) | endyear_proposed == ""), 
                                    endyear_proposed, 
                                    endyear_current),
                   hs6_use = ifelse(isscfc_code_proposed != isscfc_code_current & !(is.na(isscfc_code_proposed) | isscfc_code_proposed == ""), 
                                    0, 
                                    hs6_use_current),
                   unmapped = ifelse(!(is.na(isscfc_id_proposed) | isscfc_id_proposed == ""), 
                                     0, 
                                     unmapped),
                   toimprove = ifelse(isscfc_code_proposed != isscfc_code_current & !(is.na(isscfc_code_proposed) | isscfc_code_proposed == ""), 
                                      0, 
                                      toimprove)
                 )]
  
  #' Remove helper columns
  select_columns <- which(!grepl("_current|_proposed", names(data_to_change)))
  data_to_change <- data_to_change[, select_columns, with = F]
  
  #' Recode "" to NA
  data_to_change <- as.data.frame(data_to_change)
  # data_to_change[data_to_change == ""] <- NA
  setDT(data_to_change)
  
  data_to_change <- rbindlist(list(data_to_change, discarded_dt), fill = TRUE)
  data_to_change$fao_group <- NULL
  data_to_change <- merge(data_to_change, fao_group_dt, by = c("isscfc_code", "isscfc_id"), all.x = TRUE)
  
  data_back <- values$data_full[!id %in% c(data_to_change$id, id_discarded)]
  
  #' Adding the data updated manually by user into the full data
  trademap_manually_mapped <- rbindlist(list(data_back, # remove previous data
                                             data_to_change),                              # add data updated
                                        fill =  TRUE)
  
  #' sorting data table by id
  trademap_manually_mapped <- trademap_manually_mapped[order(as.numeric(id)), ]
  trademap_manually_mapped[, `:=`(fao_group = NULL)]
  
  sql_filter <- sprintf("rep = '%s'", input$reporter_map)
  columns_order <- ReadDatatable(fishtrade_table_to_map, where = sql_filter, limit = 1)
  setnames(columns_order, "rep", "reporter")
  columns_order <- names(columns_order)
  
  trademap_manually_mapped <- trademap_manually_mapped[, c("id", columns_order), with = F]  
  trademap_manually_mapped[isscfc_code == -1, `:=`(toimprove = 0, hs6_use = 0, unmmaped = 0)]
  
  values$data_full <- trademap_manually_mapped
  
  progress_save$inc(.3, detail = " counting missing codes.")
  n_missing_mapping <- trademap_manually_mapped[reporter == input$reporter_map & (is.na(isscfc_code) | isscfc_code == ""), .N]
  
  progress_save$inc(.6, detail = " loading table from SWS.")
  
  #' Saving new data to SWS
  if(input$reporter_map != "all") {
    trademap_manually_mapped <- as.data.frame(trademap_manually_mapped)
    trademap_manually_mapped[trademap_manually_mapped == ""] <- NA
    setDT(trademap_manually_mapped)
    
    setnames(trademap_manually_mapped, "reporter", "rep")
    trademap_manually_mapped[, id := NULL]
    writeDT(dt_name = fishtrade_table_to_map, data = trademap_manually_mapped, sql_filter = paste0("rep = ", input$reporter_map))
    setnames(trademap_manually_mapped, "rep", "reporter")
    
  } else {
    
    trademap_manually_mapped <- as.data.frame(trademap_manually_mapped)
    trademap_manually_mapped[trademap_manually_mapped == ""] <- NA
    setDT(trademap_manually_mapped)
    
    setnames(trademap_manually_mapped, "reporter", "rep")
    writeDT(dt_name = fishtrade_table_to_map, data = trademap_manually_mapped, sql_filter = NULL)
    setnames(trademap_manually_mapped, "rep", "reporter")
    
  }
  
  progress_save$close()
  
  
  if (n_missing_mapping == 0) {
    #'Show the user a message that all missing code have been mapped.
    sendSweetAlert(
      session = session,
      title = "Great!",
      text = "The map has been saved successfully.",
      type = "success"
    )
    
  } else {
    #'Show the user the remaining codes to be mapped  
    save_msg <- sprintf("There are still %s code(s) to be mapped.", n_missing_mapping)
    
    sendSweetAlert(
      session = session,
      title = save_msg,
      text = "The map has been saved successfully.",
      type = "info"
    )
    
  }
  
  
})
##-- Export Map ----
observeEvent(input$export_map, {
  #'Get the data updated
  # map_sel <- hot_to_r(input$trademap)
  validate(need(input$reporter_map, "Please select at least a country."))
  
  progress_update <- shiny::Progress$new()
  progress_update$set(message = "Updating mapping table: ", value = 0)
  progress_update$inc(.21, detail = "\n reading saved mappings.")
  
  map_sel <- ReadDatatable(table = fishtrade_table_to_map,
                           where = paste0('rep = ', input$reporter_map))
  
  setnames(map_sel, "rep", "reporter")
  
  #'test if the dataset is empty
  n_rows <- nrow(map_sel)
  
  if(n_rows == 0) {
    
    progress_update$close()
    
    save_msg <- sprintf("There is no data to be update")
    
    sendSweetAlert(
      session = session, 
      closeOnClickOutside = FALSE,
      title = save_msg,
      text = "Please try to run the mapping function and then select a valid country.",
      type = "info"
    )
    
    return(NULL)
    
  }
  
  #'*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*
  #' It does not save into trade mapping table codes that were mapped at 6 digits level
  #' hs6_use == 1: mapped at 6 digits level
  #' toimprove == 1: mapped at 6 digits level, but there are more the 6 digits
  # map_sel <- map_sel[!(hs6_use == 1 | toimprove == 1), ]
  #'*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*
  
  #** checks to updated ----
  #'check if it still missing mapping
  no_empty_fields <- c('reporter', 'country', 'tariff_line', 'flow', 'isscfc_code', 'isscfc_id', 'scheda', 'startyear', 'endyear')
  duplicates_key <- c('reporter', 'tariff_line', 'flow', 'startyear', 'endyear')
  checks_mapping <- checks_to_update_mapping(.data = map_sel, .no_empty = no_empty_fields, .duplicates_key = duplicates_key)
  
  if(checks_mapping$status == FALSE) {
    
    progress_update$close()
    
    missing_msg <- ""
    duplicated_msg <- ""
    
    if(length(checks_mapping$var_missing) != 0) {
      
      missing_msg <- sprintf("There are still missing values at column(s): %s", toString(names(checks_mapping$var_missing)))
      
    } 
    
    if(checks_mapping$n_duplicates > 0) {
      
      duplicated_msg <- sprintf("There are duplicates mappings. Please, check the rows: %s", toString(checks_mapping$row_duplicates))
      
    }
    
    
    checks_msg <- paste(missing_msg, duplicated_msg, sep = "\n", collapse = "\n")
    
    sendSweetAlert(
      session = session,
      closeOnClickOutside = FALSE,
      title = checks_msg,
      text = "Before to update the trade mapping, you must fill all missing mappings.",
      type = "error"
    )
    
    return(NULL)
    
  }
  
  #' standardize the variables
  map_sel[isscfc_id != -1, isscfc_id := stringr::str_pad(string = isscfc_id, width = 4, side = 'left', pad = '0')]
  map_sel[, isscfc_code := gsub("\\s+", "", isscfc_code)]
  
  ##** Read table from SWS ----
  progress_update$inc(.65, detail = "\n reading saved mappings.")
  
  # trademap_reporter <- ReadDatatable(table = fishtrade_trade_map,
  #                                    where = paste0('rep = ', input$reporter_map),
  #                                    readOnly = TRUE)
  # 
  # trademap_reporter[isscfc_id != -1, isscfc_id := stringr::str_pad(string = isscfc_id, width = 4, side = 'left', pad = '0')]
  # 
  setnames(map_sel, c('reporter'), c('rep'))
  # data_to_update <- map_sel[, names(trademap_reporter), with = F]
  # 
  # data_to_update[, hs6_use := as.character(hs6_use)]
  # 
  # tradedata_mapped_final <- merge(trademap_reporter, 
  #                                 data_to_update, 
  #                                 by = c("rep", "tariff_line", "flow", "isscfc_code", "isscfc_id"), 
  #                                 all = TRUE)
  
  progress_update$inc(.65, detail = "\n updating mappings.")
  
  #' #' update the national description 
  #' tradedata_mapped_final[(gsub("\\s+", "", tariff_line_descr.x) == "") & !is.na(tariff_line_descr.y), tariff_line_descr.x := tariff_line_descr.y]
  #' tradedata_mapped_final[, tariff_line_descr.y := NULL]
  #' 
  #' #' update start year 
  #' tradedata_mapped_final[startyear.x != startyear.y | is.na(startyear.x) & !is.na(startyear.y), startyear.x := startyear.y]
  #' tradedata_mapped_final[, startyear.y := NULL]
  #' 
  #' #' update end year 
  #' tradedata_mapped_final[endyear.x != endyear.y | is.na(endyear.x) & !is.na(endyear.y), endyear.x := as.integer(endyear.y)]
  #' tradedata_mapped_final[, endyear.y := NULL]
  #' 
  #' #' update hs6_use
  #' tradedata_mapped_final[is.na(hs6_use.x) & !is.na(hs6_use.y), hs6_use.x := as.character(hs6_use.y)]
  #' tradedata_mapped_final[, hs6_use.y := NULL]
  #' 
  #' #' update country
  #' tradedata_mapped_final[is.na(scheda.x) & !is.na(country.y), country.x := as.character(country.y)]
  #' tradedata_mapped_final[, country.y := NULL]
  #' 
  #' #' update scheda
  #' tradedata_mapped_final[is.na(scheda.x) & !is.na(scheda.y), scheda.x := as.character(scheda.y)]
  #' tradedata_mapped_final[, scheda.y := NULL]
  #' 
  #' setnames(tradedata_mapped_final, old = names(tradedata_mapped_final), new = gsub("\\.x", "", names(tradedata_mapped_final)))
  #' tradedata_mapped_final <- tradedata_mapped_final[, !grepl("\\.y$", names(tradedata_mapped_final)), with = F]
  #' 
  #' ##** Checks the final table ----
  #' checks_mapping <- checks_to_update_mapping(.data = tradedata_mapped_final, 
  #'                                            .no_empty = c('rep', no_empty_fields[-1]), 
  #'                                            .duplicates_key = c('rep', duplicates_key[-1]))
  #' 
  #' if(checks_mapping$status == FALSE) {
  #'   
  #'   progress_update$close()
  #'   
  #'   
  #'   duplicated_msg <- ""
  #'   
  #'   if(checks_mapping$n_duplicates > 0) {
  #'     
  #'     duplicated_msg <- sprintf("There are duplicates mappings. Please, check the rows: %s", toString(checks_mapping$row_duplicates))
  #'     
  #'   }
  #'   
  #'   
  #'   checks_msg <- paste(duplicated_msg, sep = "\n", collapse = "\n")
  #'   
  #'   sendSweetAlert(
  #'     session = session,
  #'     closeOnClickOutside = FALSE,
  #'     title = checks_msg,
  #'     text = "Check your mapping prposal, you are trying to insert duplicates mappings.",
  #'     type = "error"
  #'   )
  #'   
  #'   return(NULL)
  #'   
  #' }
  
  ##** Upload to SWS ----
  progress_update$inc(.85, detail = "\n updating mappings.")
  
  tradedata_mapped_final <- copy(map_sel)
  
  writeDT(dt_name = fishtrade_trade_map, data = tradedata_mapped_final, sql_filter = paste0('rep = ', input$reporter_map))
  
  ##** Remove country from table to be mapped ----
  clean_dt <- ReadDatatable(fishtrade_table_to_map, limit = 1)[-1]
  writeDT(dt_name = fishtrade_table_to_map, sql_filter = paste0('rep = ', input$reporter_map), data = clean_dt)
  
  progress_update$inc(1, detail = "\n updating mappings.")
  progress_update$close()
  
  n_missing_national_descr <- tradedata_mapped_final[, sum(is.na(tariff_line_descr))]
  
  if(n_missing_national_descr > 0) {
    
    msg_updated_trademap <- sprintf("However, there are still %s empty national description.", n_missing_national_descr)
    
    sendSweetAlert(
      session = session,
      html = TRUE,
      closeOnClickOutside = FALSE,
      title = "Great!",
      text = tags$span(
        tags$h3("The mapping has been UPDATED successfully in SWS."),
        tags$br(),
        tags$h3(tags$b(msg_updated_trademap))
      ), 
      type = "info"
    )
    
  } else {
    
    msg_updated_trademap <- "The map has been UPDATED successfully in SWS."
    
    sendSweetAlert(
      session = session,
      title = "Great!",
      text = msg_updated_trademap, 
      type = "success"
    )
    
  }
  
  
  
})
##-- Add Scheda ----
observeEvent(input$gen_scheda, {
  
  validate(need(input$trademap, "There is no data."))
  
  newtrademap <- hot_to_r(input$trademap) %>%
    mutate(selected = 0) %>%
    setDT()
  
  newtrademap$flow <- flow_labels_code[newtrademap$flow]
  
  row_sel <- input$trademap_select$select$rAll
  reporter_selected  <- newtrademap[row_sel, reporter]
  flow_selected      <- newtrademap[row_sel, flow]
  isscfc_code_selected <- newtrademap[row_sel, isscfc_code]
  
  if (length(isscfc_code_selected) == 0) {
    
    sendSweetAlert(
      session = session,
      title = "There are some ISSCFC empty.",
      text = "Please fill out it and click on the button 'Gen. Scheda'.",
      type = "warning"
    )
    
    return(NULL)
  }
  
  #' keep the orignal order of columns that will be modified by the merge function
  order_names <- names(newtrademap)[-ncol(newtrademap)]
  order_rows  <- newtrademap$id
  
  #** get fao group ----
  fao_group <- get_mapping_isscfc_fao_group(.local = .local)
  fao_group <- fao_group[isscfc_code %in% isscfc_code_selected, fao_group]
  
  if(length(fao_group) != length(isscfc_code_selected)) {
    
    sendSweetAlert(
      session = session,
      title = "There is no FAO group for at least one of the ISSCFC.",
      text = "Please fill out it with a valid ISSCFC code.",
      type = "warning"
    )
    
    return(NULL)
  }
  
  dt_selected <- data.table(reporter = reporter_selected,
                            flow = flow_selected,
                            fao_group = fao_group,
                            scheda = NA_character_)
  
  #** get max scheda ----
  dt_selected <- generate_scheda(.data = dt_selected)
  
  # test if the fao group is valid or not.
  if(any(dt_selected == -1)) {
    
    sendSweetAlert(
      session = session,
      title = "The FAO group typed is not valid.",
      type = "error"
    )
    
    return(NULL)
  }
  
  ## assign new scheda
  newtrademap[row_sel, scheda := as.character(dt_selected$scheda)]
  newtrademap[row_sel, fao_group := as.character(dt_selected$fao_group)]
  
  tradedata_mapped_descr <- merge(x  = values$data_full, 
                                  y  = newtrademap[id %in% order_rows[row_sel], list(id, scheda)], 
                                  by = 'id', all.x = T, suffixes = c("_old", "_new"))
  
  tradedata_mapped_descr[is.na(scheda_old) & !is.na(scheda_new), `:=`(scheda_old = scheda_new)]
  
  old_var <- grep(pattern = "_old$", names(tradedata_mapped_descr), value = TRUE)
  new_var <- grep(pattern = "_new$", names(tradedata_mapped_descr), value = TRUE)
  without_old <- gsub("_old", "", old_var)
  setnames(tradedata_mapped_descr, old_var, without_old)
  
  tradedata_mapped_descr[, eval(parse(text = sprintf("`:=`(%s)", toString(sprintf("%s = NULL", new_var)))))]
  
  setDT(newtrademap)
  newtrademap[, selected := NULL][order(id), ]
  
  newtrademap <- setcolorder(newtrademap, order_names)
  
  values$data_mapping <- newtrademap
  
  
})

















