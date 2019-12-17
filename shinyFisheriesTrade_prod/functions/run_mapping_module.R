run_mapping_module <- function(.data, .eucountry, .local = F) {
  
  tradedata <- copy(.data)
  tradedata[, year := as.integer(year)]
  setnames(tradedata, c('rep', 'comm'), c('reporter', 'tariff_line'))
  
  .reporter <- tradedata[, unique(reporter)]
  .year <- tradedata[, unique(year)]
  
  ##-- Trade Mapping table ----  
  trademap_full <- ReadDatatable(table = fishtrade_trade_map, where = paste0('rep = ', .reporter))
  setnames(trademap_full, 'rep', 'reporter')
  
  ##-- Extending mappings ----
  #' If there are no links between Tarrif Line -> ISSCFC before a given link, so this link will be valid for all previous year
  #' If there are no links between Tarrif Line -> ISSCFC after a given link, so this link will be valid onward
  trademap_full[, earliest_year := min(startyear), by = list(reporter, tariff_line, flow)]
  trademap_full[, latest_year := max(endyear), by = list(reporter, tariff_line, flow)]
  
  trademap_full[, `:=`(startyear_orig = startyear, endyear_orig = endyear)]
  
  # trademap_full[, startyear := ifelse(earliest_year == startyear, 1900, startyear)]
  trademap_full[, endyear   := ifelse(latest_year == endyear, 9999, endyear)]
  trademap_full[, `:=`(earliest_year = NULL, latest_year = NULL)]
  ##--
  
  # remove the codes discarded previously by user
  trademap <- copy(trademap_full)
  
  # trademap[, `:=`(isscfc_descr = NULL, isscfc_id = NULL, hs6_use = NULL)]
  # trademap[, `:=`(hs6_use = NULL)]
  
  # For foverlaps()
  #'setting key for using in the fovelarps function
  trademap[, `:=`(startyear = as.integer(startyear), endyear = as.integer(endyear))]
  trademap[, reporter := as.integer(reporter)]
  setkey(trademap, reporter, tariff_line, flow, startyear, endyear)
  
  if(missing(.eucountry)) {
    stop("Please the .eucountry argument is missing.")
  }
  
  if (.eucountry == FALSE) {
    ##-- Load the HS masterfile from EBX5 ----
    hs_masterfile_full <- get_hs_masterfile(local = .local)
    setnames(hs_masterfile_full, names(hs_masterfile_full), tolower(names(hs_masterfile_full)))
    hs_masterfile <- hs_masterfile_full[, list(hs, hsrep, 
                                               isscfc, 
                                               isscfc_id = stringr::str_pad(member, 4, "0", side = "left"), 
                                               isscfc_descr = nameen
                                               )]
    hs_masterfile[, isscfc := as.character(isscfc)]
    
    hs_masterfile <- hs_masterfile[, list(isscfc = isscfc[hsrep == max(hsrep)],
                                          isscfc_id = isscfc_id[hsrep == max(hsrep)],
                                          isscfc_descr = isscfc_descr[hsrep == max(hsrep)]), by = hs]
    
    ##-- Unique combinations of reporters, flows, and HS for non-EU countries ----
    # `endyear` and `startyear` will be required for mapping with `trademap`
    tldata_reporter_flow_hs <- unique(tradedata[, list(year, reporter, flow, tariff_line, hslength, startyear = year, endyear = year)])
    tldata_reporter_flow_hs[, `:=`(reporter = as.integer(reporter), flow = as.integer(flow))]
    setkey(tldata_reporter_flow_hs, reporter, tariff_line, flow, startyear, endyear)
    
    ## Map code using the trade map table
    tldata_mapped <- foverlaps(tldata_reporter_flow_hs, trademap)
    tldata_mapped[, `:=`(startyear = startyear_orig, endyear = endyear_orig, startyear_orig = NULL, endyear_orig = NULL)]
    tldata_mapped[, hs6_use := as.numeric(hs6_use)]
    tldata_mapped[is.na(hs6_use), hs6_use := 0]
    ##XXX: added
    tldata_mapped[!is.na(isscfc_code) & (hs6_use == 0 | is.na(hs6_use)), mapping := 'trade_map']
    
    ##XXX: added
    tldata_mapped[, `:=`(i.startyear = NULL,
                         i.endyear = NULL)]
    
    # HS6: this is always OK, if there is an exception, trademap should be valid
    hs_masterfile[, hs6 := substr(hs, 1, 6)]
    tldata_mapped[, hs6 := substr(tariff_line, 1, 6)]
    
    # tldata_mapped <- hs_masterfile[tldata_mapped, on = c("hs6", "hsrep"), ]
    tldata_mapped <- hs_masterfile[tldata_mapped, on = c("hs6"), ]
    tldata_mapped[hs6_use == 1 & !is.na(scheda), mapping := "hs_masterfile"]
    
    
    ##-- Use HS masterfile to map unmmaped codes ----
    tldata_mapped[!is.na(isscfc) & is.na(isscfc_code), `:=`(isscfc_code = isscfc, 
                                                            mapping = "hs_masterfile",
                                                            hs6_use = 1)]
    
    tldata_mapped[mapping == "trade_map", `:=`(isscfc_id = i.isscfc_id, 
                                               isscfc_descr = i.isscfc_descr)]
    
    tldata_mapped[, `:=`(i.isscfc_id = NULL,
                         i.isscfc_descr = NULL,
                         isscfc = NULL)]
    
    ##-- Non-fish product at HS6 level ----
    ##-- EMAIL: 22/03/2019
    # Egger, Adrienne (FIAS) Adrienne.Egger@fao.org
    # Some HS6 codes refer to non-fish only products (210390, 230990, 051199).
    # For these 3 HS6 codes, we want them to be mapped only at the tariff line 
    # level (using the trade map), i.e. when we are sure the code only refers to fish products. 
    # We don’t want them to be mapped at the HS6 level because otherwise 
    # we would include non-fish products.
    
    non_fish <- ReadDatatable(fishtrade_param_hs_non_fish)$hs_start
    non_fish <- paste0("^(", paste(non_fish, collapse = '|'), ")")
    
    tldata_mapped[grepl(non_fish, hs) & hs6_use == 1, `:=`(isscfc_code = NA_character_, 
                                                           isscfc_id = NA_character_,
                                                           isscfc_descr = NA_character_, 
                                                           hs6_use = 0, 
                                                           mapping = NA_character_)]
    
    ##++ mappings to be improved 
    tldata_mapped[, toimprove := ifelse(hs6_use == 1 & as.numeric(substr(tariff_line, 7, 12)) != 0, 1, 0)]
    ##--
    
    ##-- end: LG ----
    
  }
  

  if (.eucountry == TRUE) {
    eucn_masterfile_full <- get_cn_masterfile()
    setnames(eucn_masterfile_full, names(eucn_masterfile_full), tolower(names(eucn_masterfile_full)))
    eucn_masterfile_full <- eucn_masterfile_full[, list(tariff_line = hs, hsrep, 
                                               isscfc, 
                                               isscfc_id = stringr::str_pad(member, 4, "0", side = "left"), 
                                               isscfc_descr = nameen
    )]
    
    eucn_masterfile_full <- eucn_masterfile_full[, list(isscfc = isscfc[hsrep == max(hsrep)],
                                          isscfc_id = isscfc_id[hsrep == max(hsrep)],
                                          isscfc_descr = isscfc_descr[hsrep == max(hsrep)]), by = tariff_line]
    
    ##-- Unique combinations of reporters, flows, and HS for non-EU countries ----
    # Unique combinations of reporters and HS for EU countries
    # NOT USING YEAR AS IN THE MAPPING FILE ALL CN20xx ARE VALID
    # `endyear` and `startyear` will be required for mapping with `trademap`
    esdata_reporter_flow_hs <- unique(tradedata[, list(year, reporter, flow, tariff_line, startyear = year, endyear = year)])
    esdata_reporter_flow_hs[, `:=`(reporter = as.integer(reporter), flow = as.integer(flow))]
    setkey(esdata_reporter_flow_hs, reporter, tariff_line, flow, startyear, endyear)
    
    ##### EU
    ## Map code using the trade map table
    esdata_mapped <- foverlaps(esdata_reporter_flow_hs, trademap)
    esdata_mapped[, `:=`(startyear = startyear_orig, endyear = endyear_orig, startyear_orig = NULL, endyear_orig = NULL)]
    esdata_mapped[, hs6_use := as.numeric(hs6_use)]
    esdata_mapped[is.na(hs6_use), hs6_use := 0]
    
    ##XXX: added
    esdata_mapped[!is.na(isscfc_code) & (hs6_use == 0 | is.na(hs6_use)), mapping := 'trade_map']
    
    ##XXX: added
    esdata_mapped[, `:=`(i.startyear = NULL,
                         i.endyear = NULL)]
    
    eucn_masterfile_full[, cn8 := substr(tariff_line, 1, 8)]
    esdata_mapped[, cn8 := substr(tariff_line, 1, 8)]
    eucn_masterfile_full[, tariff_line := NULL]
    esdata_mapped <- eucn_masterfile_full[esdata_mapped, on = "cn8"]
    
    esdata_mapped[mapping == "trade_map", `:=`(isscfc_id = i.isscfc_id, 
                                               isscfc_descr = i.isscfc_descr)]
    
    ##-- Use cn masterfile to map unmmaped codes ----
    esdata_mapped[!is.na(isscfc) & is.na(isscfc_code), `:=`(isscfc_code = isscfc, 
                                                            mapping = "cn_masterfile",
                                                            hs6_use = 1)]
    
    esdata_mapped[, `:=`(i.isscfc_id = NULL,
                         i.isscfc_descr = NULL,
                         isscfc = NULL)]
    
    ##-- Non-fish product at HS6 level ----
    ##-- EMAIL: 22/03/2019
    # Egger, Adrienne (FIAS) Adrienne.Egger@fao.org
    # Some HS6 codes refer to non-fish only products (210390, 230990, 051199).
    # For these 3 HS6 codes, we want them to be mapped only at the tariff line 
    # level (using the trade map), i.e. when we are sure the code only refers to fish products. 
    # We don’t want them to be mapped at the HS6 level because otherwise 
    # we would include non-fish products.
    
    non_fish <- ReadDatatable(fishtrade_param_hs_non_fish)$hs_start
    non_fish <- paste0("^(", paste(non_fish, collapse = '|'), ")")
    
    esdata_mapped[grepl(non_fish, tariff_line) & hs6_use == 1, `:=`(isscfc_code = NA_character_, 
                                                           isscfc_id = NA_character_,
                                                           isscfc_descr = NA_character_, 
                                                           hs6_use = 0, 
                                                           mapping = NA_character_)]
    
    ##++ mappings to be improved 
    esdata_mapped[, toimprove := ifelse(hs6_use == 1 & as.numeric(substr(tariff_line, 9, 12)) != 0, 1, 0)]
    ##--

  }
  
  tradedata_mapped <-
    rbind(
      if (!.eucountry) copy(tldata_mapped) else NULL,
      if (.eucountry) copy(esdata_mapped) else NULL,
      fill = TRUE)
  ##-- Filters
  ## remove the discarded codes
  # tradedata_mapped <- tradedata_mapped[isscfc_code != -1 | is.na(isscfc_code), ]
  
  ##++ unmapped filter
  tradedata_mapped[, unmapped := ifelse(is.na(isscfc_code), 1, 0)]
  
  
  ##-- Generate Scheda ----
  is_generate_scheda <- tradedata_mapped[!is.na(isscfc_code), any(is.na(scheda))]
  
  if(is_generate_scheda) {
    fao_group <- get_mapping_isscfc_fao_group(.local = .local)
    tradedata_mapped[, isscfc_id := stringr::str_pad(isscfc_id, 4, "0", side = "left")]
    tradedata_mapped <- merge(tradedata_mapped, fao_group, all.x = T, by = c('isscfc_code', 'isscfc_id'))
    tradedata_mapped <- generate_scheda(tradedata_mapped)
  }

  ##--
  
  
  if (tradedata_mapped[, any(unmapped == 1)] | tradedata_mapped[, any(toimprove == 1)]) {
    
    tradedata_mapped <-
      tradedata_mapped[, list(startyear = ifelse(is.na(isscfc_code) | hs6_use == 1, as.integer(min(year)), as.integer(min(startyear))), 
                              endyear   = ifelse(is.na(isscfc_code) | hs6_use == 1, .year, as.integer(max(c(endyear, .year)))), 
                              hs6_use   = mean(hs6_use),
                              unmapped  = mean(unmapped),
                              toimprove = mean(toimprove)),
                       by = list(reporter, tariff_line, flow, isscfc_code, isscfc_id, isscfc_descr, remark, scheda)]
    
    ## Prepare data to be saved in SWS
    #' National Description 
    trademap_descr <- unique(trademap_full[, list(reporter, flow, isscfc_id, tariff_line_descr, tariff_line, isscfc_code, isscfc_descr)])
    tradedata_mapped <- merge(tradedata_mapped, trademap_descr, by = c('reporter', 'flow', 'tariff_line', 'isscfc_code', 'isscfc_id'), all.x = T)
    
    #' ISSCFC description 
    tradedata_mapped[(toimprove == 1 | hs6_use == 1) & !is.na(isscfc_code), isscfc_descr := isscfc_descr.x]
    tradedata_mapped[unmapped == 0 & toimprove == 0, isscfc_descr := isscfc_descr.y, ]
    tradedata_mapped[, `:=`(isscfc_descr.x = NULL, isscfc_descr.y = NULL)]
    
    #' Appending codes to be mapped 
    to_map_improve <- copy(tradedata_mapped)#[is.na(isscfc_code) | toimprove == 1]  
    trademap_full[, `:=`(startyear = startyear_orig, endyear = endyear_orig, startyear_orig = NULL, endyear_orig = NULL)]
    
    trade_map_complete <- rbindlist(list(trademap_full, to_map_improve), fill = TRUE)
    trade_map_complete[, `:=`(unmapped = ifelse(is.na(unmapped), 0, unmapped), 
                              toimprove = ifelse(is.na(toimprove), 0, toimprove))]
    
    trade_map_complete <- trade_map_complete[, 
                       list(country = unique(country[!is.na(country)]), 
                            tariff_line_descr = last(tariff_line_descr),
                            isscfc_descr = last(isscfc_descr), 
                            remark = last(remark), 
                            startyear = min(startyear), 
                            endyear = max(endyear),
                            unmapped = last(unmapped), 
                            toimprove = last(toimprove),
                            hs6_use = max(hs6_use)), 
                       by = c("reporter", "flow", "tariff_line", 'isscfc_code', 'isscfc_id', 'scheda')]
    
    ##-- Setting columns names and order
    setnames(trade_map_complete, old = c("reporter"), new = c("rep"))
    columns_order <- names(ReadDatatable(table = fishtrade_trade_map, limit = 1))
    
    setcolorder(trade_map_complete, c('rep', 'country', 'tariff_line', 'flow', 'isscfc_code', 'isscfc_id', 
                                      'tariff_line_descr', 'isscfc_descr', 'startyear', 'endyear', 'remark', 
                                      'scheda', 'hs6_use', 'unmapped', 'toimprove'))
    
    to_map_dt <- ReadDatatable(table = fishtrade_table_to_map, where = paste0('rep = ', .reporter))
    trade_map_complete <- rbindlist(list(to_map_dt, trade_map_complete), fill = TRUE)
    trade_map_complete <- unique(trade_map_complete)
    
    ##-- Include ISSCFC description
    
    ##-- Impute missing star/end year
    trade_map_complete[is.na(startyear), startyear := .year]
    trade_map_complete[is.na(endyear), endyear := .year]
    
    # country name
    trade_map_complete[, country := ifelse(is.na(country), trade_map_complete[,  as.character(na.exclude(unique(country)))], country)]
    
    #' if the user run the mapping for a new year, then the end year will be updated with the latest year.
    trade_map_complete[, duplicated := duplicated(.SD) | duplicated(.SD, fromLast = TRUE), .SDcols = c("rep", "tariff_line", "flow", "isscfc_code")]
    trade_map_complete[duplicated == TRUE, `:=`(startyear = min(startyear), endyear = max(endyear)), by = c("rep", "tariff_line", "flow", "isscfc_code")]
    trade_map_complete[, duplicated := duplicated(.SD), .SDcols = c("rep", "tariff_line", "flow", "isscfc_code")]
    trade_map_complete <- trade_map_complete[duplicated == FALSE][, duplicated := NULL]
    
    #'*FIXE ME!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
    #' if the mapping still missing and the user run the mapping for a new year, then the end year will be updated with the latest year.
    trade_map_complete[, duplicated := duplicated(.SD) | duplicated(.SD, fromLast = TRUE), .SDcols = c("rep", "tariff_line", "flow", "startyear", "endyear")]
    trade_map_complete[, n_na := rowSums(is.na(.SD))]
    trade_map_complete[, filter := n_na == min(n_na), by = c("rep", "tariff_line", "flow", "startyear", "endyear")]
    trade_map_complete <- trade_map_complete[filter == TRUE,][, `:=`(n_na = NULL, filter = NULL, duplicated = NULL)]
    
    ##-- Write table on SWS ----
    writeDT(dt_name = fishtrade_table_to_map, data = trade_map_complete, sql_filter = paste0('rep = ', .reporter), insert = FALSE)
    
    n_missing <- trade_map_complete[, sum(unmapped)]
    
    if(n_missing == 0) {
      out <- list(n_missing = 0, 
                  n_improve = trade_map_complete[, sum(toimprove)],
                  status = "nothing_to_map")
      
    } else {
      
      out <- list(n_missing = n_missing, 
                  n_improve = trade_map_complete[, sum(toimprove)],
                  status = "to_map")
      
    }
    
    
    
    return(out)
    
  }

  out <- list(n_missing = 0, n_improve = 0, status = "nothing_to_map")
  
  return(out)
  
}


