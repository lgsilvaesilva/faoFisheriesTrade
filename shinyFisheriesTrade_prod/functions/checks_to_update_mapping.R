checks_to_update_mapping <- function(.data, .no_empty, .duplicates_key) {
  df <- copy(.data)
  
  var_missing <- df[, lapply(.SD, function(x) sum(is.na(x) | gsub("\\s+", "", x) == "")), .SDcols = .no_empty]
  row_duplicates_df <- df[, .N, by = .duplicates_key]
  row_duplicates <- row_duplicates_df[, which(N > 1)]
  
  out <- list(var_missing = var_missing[, var_missing != 0, with = F],
              row_duplicates = row_duplicates,
              n_duplicates = length(row_duplicates))
  
  out$status <- nrow(out$var_missing) == 0 & out$n_duplicates == 0
    
  return(out)
  
}
