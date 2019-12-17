##-- Outliers detected ----
##** Correction proposed at the level that the outlier was detected
# data2sws[, rep := stringr::str_pad(rep, 3, "left", 0)]
# .box_coef <- 1.5

fix_outlier <- function(.data_full, .out, .box_coef = 1.5, .n_prt = 5){
  
  ##-- Correction at partner level ----
  data_corrected <- copy(.data_full)
  data_corrected[, id := 1:.N]
  data_corrected[, uv := value / weight]
  data_corrected[, corrected := FALSE]
  data_corrected[, c("weight_box_fix", "value_box_fix",
                     "weight_ts_fix", "value_ts_fix",
                     "level_agg", "criteria") := list(NA_real_, NA_real_,NA_real_, NA_real_, NA_character_, NA_character_)]
  
  data_corrected[,level_agg := factor(level_agg, levels = c("tariff_line", "isscfc_code", paste0("hs", c(6, 4, 2)), "prt","fao_group", "flow"), ordered = T)]
  
  .out[level_aggregation == "isscfc", level_aggregation := "isscfc_code"]
  .out[, level_aggregation := factor(level_aggregation, levels = c("tariff_line", "isscfc_code", paste0("hs", c(6, 4, 2)), "prt","fao_group", "flow"), ordered = T)]
  
  for(i in 1:nrow(.out)) {
    out_df <- .out[i, ]
    
    ##-- Filter Partners to be corrected ----
    # filter the partners which comprised the aggregated records
    filter_level <- sprintf("%s == '%s'", out_df$level_aggregation, out_df$code_aggregation)
    prt_level <- data_corrected[year == out_df$year & reporter == out_df$rep & flow == out_df$flow & eval(parse(text = filter_level))]
    
    # count the number of partners; if there are more than 5 partner, then we try to detect partners outliers and fix only these partners.
    apply_prt_box <- prt_level[, .N > .n_prt]
    
    if(apply_prt_box == TRUE) {
      
      correction_prt <- od_box_plot(.var = prt_level[, get(out_df[, criteria])], .box_coef = .box_coef)
      index <- correction_prt$index <- !is.na(correction_prt$replacements)

      # test: this correction is more specific than the previous correction
      index_fix <- prt_level[, index == TRUE & (is.na(level_agg) | out_df[, level_aggregation] < level_agg)]
      replacement <- correction_prt$replacements[index_fix]
      
      if(all(index_fix == FALSE)) next
      
      if(out_df[, criteria] == "uv") {
        
        prt_level[index_fix, `:=`(weight_box_fix = value/replacement,
                                  level_agg  = out_df$level_aggregation,
                                  criteria   = out_df$criteria)]
        
      }
      
      if(out_df[, criteria] == "weight") {
        
        prt_level[index_fix,  `:=`(weight_box_fix = replacement * weight/sum(weight),
                                   level_agg  = out_df$level_aggregation,
                                   criteria   = out_df$criteria)]
        
      }
      
      if(out_df[, criteria] == "value") {
       
        prt_level[index_fix, `:=`(value_box_fix = replacement * value/sum(value),
                                  level_agg  = out_df$level_aggregation,
                                  criteria   = out_df$criteria)]
        
      }
      
      data_corrected <- rbindlist(list(data_corrected[!(id %in% prt_level$id)], prt_level), fill = T)
      
    } else {
      
      index_fix <- prt_level[, (is.na(level_agg) | out_df[, level_aggregation] < level_agg)]
      if(all(index_fix == FALSE)) next
      
      
      if(out_df[, criteria] == "uv") {
        
        prt_level[, `:=`(weight_box_fix = value/out_df[, out_box_correction], 
                         weight_ts_fix = value/out_df[, out_ts_correction],
                         level_agg  = out_df$level_aggregation,
                         criteria   = out_df$criteria)]
        
      }
      
      if(out_df[, criteria] == "weight") {
        
        prt_level[, `:=`(weight_box_fix = out_df[, out_box_correction] * weight/sum(weight), 
                         weight_ts_fix = out_df[, out_ts_correction] * weight/sum(weight),
                         level_agg  = out_df$level_aggregation,
                         criteria   = out_df$criteria)]
        
      }
      
      if(out_df[, criteria] == "value") {
        
        prt_level[, `:=`(value_box_fix = out_df[, out_box_correction] * value/sum(value), 
                         value_ts_fix = out_df[, out_ts_correction] * value/sum(value),
                         level_agg  = out_df$level_aggregation,
                         criteria   = out_df$criteria)]
        
      }
      
      data_corrected <- rbindlist(list(data_corrected[!(id %in% prt_level$id)], prt_level), fill = T)
      
    }  
  }
  
  return(data_corrected)
  
}












