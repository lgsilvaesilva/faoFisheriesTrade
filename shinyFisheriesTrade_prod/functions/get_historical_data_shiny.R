get_historical_data <- function(.year, .reporter, .reactive_values, .source) {
  
  .reactive_values
  years_od    <- .year
  reporter_od <- .reporter
  
  sql_filter <- sprintf("rep = '%s' OR rep = '%s'AND year = '%s'", 
                        reporter_od,  
                        stringr::str_pad(reporter_od, 3, "left", "0"),
                        years_od)
  
  if(.source == "corrected") {
    
    data_to_detect <- ReadDatatable("fishtrade_outlier_validated", where = sql_filter)
    data_to_detect <- data_to_detect[, list(year, rep, prt, flow, 
                                            comm = tariff_line, value = value_correction, 
                                            weight = weight_correction,
                                            imputation_method)]  
    
  } else {
    
    data_to_detect <- ReadDatatable("fishtrade_data_imputed_validated", where = sql_filter)  
    
  }
  
  sql_filter <- sprintf("rep = '%s' OR rep = '%s'", reporter_od, stringr::str_pad(reporter_od, 3, "left", 0))
  rep_trade <- ReadDatatable("fishtrade_trade_map", where = sql_filter)
  rep_trade[, rep := as.numeric(rep)]
  
  ##** Mapping data to ISSCFC ----
  data_to_detect <- map_trade(.data = data_to_detect, .map = rep_trade)
  setnames(data_to_detect, c("rep"), c("reporter"))
  data_to_detect[, reporter := stringr::str_pad(reporter, 3, "left", "0")]
  
  ##** Remove ISSCFC discarded ----
  data_to_detect <- data_to_detect[isscfc_code != -1, ]
  
  ##-- Get Historical Data ----
  sql_rep_hist <- sprintf("rep = '%s' AND year <> '%s' AND value_usd IS NOT NULL", reporter_od, years_od)
  rep_hist <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist)
  setnames(rep_hist, c("rep", "remarks", "isscfc"), c("reporter", "remark", "isscfc_code"))
  setkey(rep_hist,  reporter, flow, year)
  rep_hist[, value := NULL]
  
  rep_hist <- rep_hist[, list( year, reporter, flow, scheda, 
                               isscfc_code, 
                               tariff_line, tariff_line_descr,
                               weight = qty, value = value_usd)]
  
  # sql_comm <- sprintf("(%s)", toString(sprintf("'%s'", data_to_detect[, unique(isscfc_code)])))
  # sql_rep_hist_comm <- paste(sql_rep_hist, "AND isscfc IN", sql_comm)
  # rep_hist2 <- ReadDatatable("fishtrade_data_legacy", where = sql_rep_hist_comm)  
  
  .reactive_values$data_partner <- data_to_detect
  
  data_to_detect <- data_to_detect[, list(year, reporter, flow, prt, scheda, 
                                          isscfc_code,
                                          tariff_line = comm, tariff_line_descr,
                                          weight, value, 
                                          imputation_method)]
  data_full <- rbindlist(l = list(rep_hist, data_to_detect), fill = TRUE, idcol = "case")
  data_full[, 
            `:=`(value = as.numeric(value),
                 weight = as.numeric(weight),
                 year = as.numeric(year))]
  
  .reactive_values$data_full <- data_full 
  
}