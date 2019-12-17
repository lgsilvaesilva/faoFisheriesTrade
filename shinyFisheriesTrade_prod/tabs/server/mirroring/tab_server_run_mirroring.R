mirror_values <- reactiveValues()

##-- Years ----
observe({
  
  if(input$navbar != "tab_mirroring") return(NULL)
  
  run_mapping_values$ft_workflow <- ReadDatatable("fishtrade_reporter_workflow")
  trade_years <- run_mapping_values$ft_workflow[is_built_data == 1 & 
                                                  is_run_mapping == 1 & 
                                                  is_mapped_data == 1 & 
                                                  is_run_imputation == 1 &
                                                  is_imputed_data == 1 &
                                                  is_run_outlier == 1 &
                                                  is_outlier_validated == 1, unique(year)]
  
  updatePickerInput(session = session,
                    inputId = "mirroring_years",
                    choices = trade_years)
  
})

##-- Check missing reporters ----
##** Confirmation dialog box to check partner ----
observeEvent(input$check_partners, {
  
  validate(need(input$mirroring_years, "There is no data available."))
  
  fao_confirmSweetAlert(
    session = session,
    inputId = "check_partners_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Could you please confirm it?")),
    title = "Check for non-reporters can take a while.",
    danger_mode = TRUE, 
    html = T, 
    p_selector = "#placeholder_run_mirror"
  )
  
})
##** Flows and countries to mirror ----
observeEvent(input$check_partners_confirmation, {
  
  if(input$check_partners_confirmation == FALSE) return(NULL)
  
  
  ##-- Progress bar 
  #' progress bar to be shown to user while the process is running
  progressSweetAlert(
    session = session, 
    id = "check_partners_progress",
    title = "Checking non-reporters.",
    display_pct = F,
    value = 0
  )
  
  ##** updating progress bar 30
  updateProgressBar(
    title = "Reading validated data.",    
    session = session,
    id = "check_partners_progress",
    value = 60
  )
  ##---
  
  p_year <- input$mirroring_years
  query <- paste0("year = ", p_year)

  data_to_mirror <- ReadDatatable("fishtrade_outlier_validated", where = query)
  setnames(data_to_mirror, c("prt", "rep"), c("partner", "reporter"))
  
  # data_to_mirror <- readRDS("data/ex_data_to_mirror.rds")
  # data_to_mirror <- readRDS("data/ex_data_to_mirror_2017.rds")

  ##** updating progress bar 30
  updateProgressBar(
    title = "Checking non-reporters.",    
    session = session,
    id = "check_partners_progress",
    value = 80
  )
  ##---
  
  mirror_values$data_to_mirror <- copy(data_to_mirror)
  to_mirror_dt <- setDT(flowsToMirror(data = data_to_mirror, names = FALSE))
  
  mirror_values$toMirror <- to_mirror_dt
  
  ##** updating progress bar 30
  updateProgressBar(
    title = "Checking non-reporters.",    
    session = session,
    id = "check_partners_progress",
    value = 90
  )
  ##---
  
  ##** updating progress bar 30
  updateProgressBar(
    title = "Checking non-reporters.",    
    session = session,
    id = "check_partners_progress",
    value = 100
  )
  ##---
  
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Checking non-reporters has been finished!",
    type = "success"
  )
  
})

##-- Update: non reporter ----
# observe({
#   
#   validate(need(mirror_values$toMirror, ""))
#   
#   non_reporter <- data.table(area = mirror_values$toMirror[, unique(area)])
#   non_reporter <- merge(non_reporter, m49_codes, by.x = "area", by.y = "reporter", all.x = TRUE)
#   
#   non_reporter_list <- non_reporter$area
#   names(non_reporter_list) <- non_reporter$country
#   
#   updatePickerInput(session = session,
#                     inputId = "non_reporter",
#                     choices = non_reporter_list)
# })

##-- Update: flow ----
# observe({
#   
#   validate(need(mirror_values$toMirror, ""))
#   validate(need(input$non_reporter, ""))
#   
#   flows <- mirror_values$toMirror[area == input$non_reporter, unique(flow)]
#   
#   flows <- flow_labels_code[flows]
#   
#   updatePickerInput(session = session,
#                     inputId = "flow_to_mirror",
#                     choices = flows)
# })

##-- Update: Donors ----
observe({
  
  validate(need(mirror_values$toMirror, ""))

  toMirror <- copy(mirror_values$toMirror)
  
  donors_list <- data.table(area = mirror_values$data_to_mirror[partner %in% unique(toMirror$area), unique(reporter)])
  donors_list[, area := as.integer(area)]
  donors_dt <- merge(donors_list, m49_codes, by.x = "area", by.y = "reporter", all.x = TRUE)
  
  donors_list <- donors_dt$area
  names(donors_list) <- donors_dt$country
  
  updatePickerInput(session = session,
                    inputId = "donors_not_mirror",
                    choices = donors_list)
  
})

##-- Run Mirroring process ----
##** Confirmation dialog box to check partner ----
observeEvent(input$run_mirror, {
  
  validate(need(input$mirroring_years, "There is no data available."))
  
  fao_confirmSweetAlert(
    session = session,
    inputId = "run_mirror_confirmation",
    type = "warning",
    text = tags$span(tags$h3("Could you please confirm it?")),
    title = "Mirrroring process can take a while.",
    danger_mode = TRUE, 
    html = T, 
    p_selector = "#placeholder_run_mirror"
  )
  
})
##-- Run mirroring process ----
observeEvent(input$run_mirror_confirmation, priority = 1, {
  
  if(input$run_mirror_confirmation == FALSE) return(NULL)
  
  p_year         <- input$mirroring_years
  p_non_reporter <- input$non_reporter
  p_flow         <- input$flow_to_mirror
  
  data_full <- copy(mirror_values$data_to_mirror) # data comes from the data table: outlier validated
  toMirror  <- copy(mirror_values$toMirror)

  #' #'*=============================================== REMOVE ME ====================================================*
  #' data_full <- data_full[, list(value = sum(value, na.rm = T),
  #'                               weight = sum(weight, na.rm = T),
  #'                               qty = sum(qty, na.rm = T),
  #'                               src = first(src)),
  #'                        by = list(year, reporter, partner, flow, comm)]
  #' #'*=============================================== REMOVE ME ====================================================*
  
  ##-- Progress bar 
  #' progress bar to be shown to user while the process is running
  progressSweetAlert(
    session = session, id = "mirroring_progress",
    title = "Mirroring in progress.",
    display_pct = F,
    value = 0
  )
  
  ##** updating progress bar 30
  updateProgressBar(
    title = "Mirroring in progress",
    session = session,
    id = "mirroring_progress",
    value = 60
  )
  ##---
  # toMirror[, mirrored := NULL]
  
  ## mapping the data to be mirrored 
  # trademap <- ReadDatatable("fishtrade_trade_map")
  # setnames(data_full, c("tariff_line", "partner", "reporter"), c("comm", "prt", "rep"))
  # data_full <- map_trade(.data = data_full, .map = trademap)
  
  #' #'*=============================================== REMOVE ME ====================================================*
  #' 
  #' data_full[, hs6 := substr(comm, 1, 6)]
  #' hs_masterfile_full[, hs6 := substr(hs, 1, 6)]
  #' 
  #' hs_masterfile <- hs_masterfile_full[, list(hs6 = hs6[max(hsrep) == hsrep],
  #'                           NameEn = NameEn[max(hsrep) == hsrep],
  #'                           ISSCFC = ISSCFC[max(hsrep) == hsrep])]
  #' 
  #' data_full <- merge(data_full, hs_masterfile, by = "hs6", all.x = TRUE)
  #' data_full[is.na(isscfc_code) & !is.na(ISSCFC), isscfc_code := ISSCFC]
  #' data_full[is.na(isscfc_descr) & !is.na(NameEn), isscfc_descr := NameEn]
  #' data_full[, c("ISSCFC", "NameEn", "hs6") := NULL]
  #' data_full <- data_full[!is.na(isscfc_code)]
  #' 
  #' data_full <- data_full[flow != 3]
  #' toMirror <- toMirror[flow != 3]
  #' #'*=============================================== REMOVE ME ====================================================*
  
  ## mirroring 
  # setnames(data_full, c("prt", "rep"), c("partner", "reporter"))
  data_full[, flow := as.integer(flow)]
  data_full[, reporter := as.character(reporter)]
  data_mirrored <- mirrorNonReporters_fi(tradedata = data_full, mirror = toMirror)
  data_mirrored[, c("reporterM49", "partnerM49") := NULL]
  
  ## removing donors
  data_mirrored <- data_mirrored[!(mirrored == 1 & partner %in% input$donors_not_mirror), ]
  
  ## selecting the data reportes estimated through mirroring process
  data_non_mirrored <- data_mirrored[mirrored == 0]
  data_mirrored     <- data_mirrored[mirrored == 1]
  
  ## creating remark field for those reporters estimated
  data_mirrored[, remark := sprintf("Partnership Data | %s | %s | %s | %s | %s", 
                                    stringr::str_pad(partner, 3, "left", pad = "0"), 
                                    tariff_line, 
                                    stringr::str_pad(isscfc_code, 16, "left", " "), 
                                    isscfc_id, 
                                    scheda)]
  
  ## retrieving scheda code for those countires estimated through the mirroring process
  query <- sprintf("est_reporter_code IN (%s)", toString(sprintf("'%s'", data_mirrored[, unique(reporter)])))
  partnership_map <- ReadDatatable("fishtrade_partnership_map", where = query)
  partnership_map[, id := 1:.N]
  
  partnership_map[, `:=`(startyear = as.numeric(startyear), 
                         endyear = as.numeric(endyear),
                         donor_code = as.character(donor_code),
                         est_reporter_code = as.character(est_reporter_code))]
  data_mirrored[, `:=`(startyear = as.numeric(year), endyear = as.numeric(year))]
  
  ## mapping the mirrored data for retrieving the scheda
  setkey(partnership_map, est_reporter_code, est_reporter_flow, donor_code, donor_tariff_line, startyear, endyear)
  setkey(data_mirrored, reporter, flow, partner, tariff_line, startyear, endyear)
  
  data_mirrored <- foverlaps(data_mirrored, partnership_map[, list(est_reporter_code, est_reporter_flow, donor_code, donor_tariff_line, 
                                                                   startyear, endyear, est_reporter_scheda, donor_scheda, id)])
  ## assgning the scheda codes
  data_mirrored[, `:=`(donor_scheda = scheda,
                       scheda = est_reporter_scheda
                       )]
  
  # data_mirrored <- merge(data_mirrored, fao_group_dt, by = c("isscfc_code", "isscfc_id"), all.x = TRUE)
  
  data_mirrored[, new_mirror := 0L]
  data_mirrored[is.na(scheda), new_mirror := 1L]
  
  ## generate new schedas 
  data_mirrored[, reporter := as.integer(reporter)]
  
  if(data_mirrored[, any(is.na(scheda))]) {
    
    data_mirrored <- generate_scheda(.data = data_mirrored)  
    
  }
  
  ## Updating the partnership map
  new_partnership_map <- unique(data_mirrored[new_mirror == 1L, list(est_reporter_code = reporter, 
                                                                     est_reporter_flow = flow, 
                                                                     est_reporter_scheda = scheda,
                                                                     est_reporter_remarks = remark,
                                                                     donor_code = partner,
                                                                     donor_tariff_line = tariff_line,
                                                                     donor_tariff_line_desc = tariff_line_descr,
                                                                     donor_isscfc_code = isscfc_code,
                                                                     donor_isscfc_id = isscfc_id,
                                                                     donor_scheda = donor_scheda,
                                                                     donor_flow = recode(flow, '2' = 1, "1" = 2),
                                                                     startyear = min(year),
                                                                     endyear = min(year))])
  
  ## Updating the partnership map
  old_partnership_map <- unique(data_mirrored[new_mirror == 0L, list(est_reporter_code = reporter, 
                                                                     est_reporter_flow = flow, 
                                                                     est_reporter_scheda = scheda,
                                                                     est_reporter_remarks = remark,
                                                                     donor_code = partner,
                                                                     donor_tariff_line = tariff_line,
                                                                     donor_tariff_line_desc = tariff_line_descr,
                                                                     donor_isscfc_code = isscfc_code,
                                                                     donor_isscfc_id = isscfc_id,
                                                                     donor_scheda = donor_scheda,
                                                                     donor_flow = recode(flow, '2' = 1, "1" = 2),
                                                                     startyear = ifelse(i.startyear < startyear, i.startyear, startyear),
                                                                     endyear = ifelse(i.endyear > endyear, i.endyear, endyear),
                                                                     id = id)])
  
  ## saving back the partnership map
  partnership_map <- rbindlist(list(partnership_map[!id %in% old_partnership_map$id], old_partnership_map, new_partnership_map), fill = TRUE)
  partnership_map[, id := NULL]
  writeDT(dt_name = "fishtrade_partnership_map", data = partnership_map, sql_filter = query)
  
  
  ## assigning NA to the tariff line variable
  data_mirrored[, tariff_line := NA_character_]
  data_mirrored[, c("new_mirror", "donor_scheda", "est_reporter_scheda", "startyear", "endyear", "i.startyear", "i.endyear") := NULL]
  
  data_out <- rbindlist(list(data_non_mirrored, data_mirrored), fill = TRUE)
  data_out[, c("id") := NULL]
  # setcolorder(data_out, 
  #             c("year", 'reporter', 'flow', 'partner', "comm", 'tariff_line_descr', 
  #               'remark','fao_group', 'scheda', 'isscfc_code', 'isscfc_id', 'isscfc_descr', 
  #               'value', 'weight', 'qty', 'src', "mirrored")
  #             )
  
  data_out[is.na(fao_group), fao_group := substr(scheda, 1, 1)]

  ## updating imputation method
  data_out[mirrored == 1, imputation_method := "mirroring"]
  mirror_values$data_mirrored <- data_mirrored
  
  ## saving back the mirrored data
  col_order <- names(ReadDatatable("fishtrade_mirrored", limit = 1))
  setnames(data_out, c("partner", "reporter"), c("prt", "rep"))
  data_out <- data_out[, col_order, with = FALSE]
  writeDT(dt_name = "fishtrade_mirrored", data_out)
  
  ## updating reporter status
  ## updading all countries at once
  query <- sprintf("year = '%s'", p_year)
  fishtrade_reporter_workflow <- ReadDatatable("fishtrade_reporter_workflow", where = query)
  fishtrade_reporter_workflow[is_outlier_validated == 1, is_run_mirroring := 1]
  writeDT(dt_name = "fishtrade_reporter_workflow", data = fishtrade_reporter_workflow, sql_filter = query)
  
  ##** updating progress bar 30
  updateProgressBar(
    title = "Mirroring in progress",
    session = session,
    id = "mirroring_progress",
    value = 100
  )
  ##---
  closeSweetAlert(session = session)
  
  sendSweetAlert(
    session = session,
    title = "Mirroring process has been finished!",
    type = "success"
  )
  
})

##-- Table mirrored ----
output$tblMirrored <- renderDataTable({
  
  validate(need(input$run_mirror_confirmation, ""),
           need(mirror_values$data_mirrored, ""))
  
  if(input$run_mirror_confirmation == FALSE) return(NULL)
  
  if(!is.null(input$donors_not_mirror)) {
    
    dt <- mirror_values$data_mirrored[!(mirrored == 1 & partner %in% input$donors_not_mirror), ]
    
  } else {
    
    dt <- copy(mirror_values$data_mirrored)
    
  }
  
  dt[mirrored == 0, `:=`(value = value_correction, weight = weight_correction)]
  columns2hide <- c("id", "value_correction", "weight_correction", "imputation_method", "weight_method", "value_method", "level_agg", "criteria", "accepted")
  columns2hide <- which(names(dt) %in% columns2hide)
  
  dt %>%
    select(year, reporter, flow, partner, tariff_line, tariff_line_descr, isscfc_code, fao_group, scheda, remark, everything(), -id) %>%
    datatable(options = list(columnDefs = list(list(visible = FALSE, targets = columns2hide))))
  
  
})





























