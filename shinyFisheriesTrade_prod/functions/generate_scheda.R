generate_scheda <- function(.data) {
  standard_fao_group <- c(1:9, LETTERS[1])

  .data_full <- copy(.data)
  .data <- .data_full[is.na(scheda), ]
  .reporter  <- unique(.data$reporter)
  .flow      <- unique(.data$flow)
  .fao_group <- .data[!is.na(fao_group), unique(fao_group)]
    
  if(!all(.fao_group %in% standard_fao_group)) {
    return(-1)
  }
  
  sql_filter <- sprintf("rep IN (%s) AND flow IN (%s) AND fao_group IN (%s)",
                        toString(sprintf("'%s'", .reporter)), 
                        toString(sprintf("'%s'", .flow)), 
                        toString(sprintf("'%s'", .fao_group)))
  
  fishtrade_max_scheda <- ReadDatatable('fishtrade_max_scheda', where = sql_filter)
  setnames(fishtrade_max_scheda, 'rep', 'reporter')
  
  # if there is no max scheda, then add a new from 1
  if(nrow(fishtrade_max_scheda) == 0) {
    
    fishtrade_max_scheda <- data.table(rep = .reporter, flow = .flow, fao_group = .fao_group, max_scheda = 1)
    fishtrade_max_scheda$scheda <- paste0(fishtrade_max_scheda$fao_group, ".", fishtrade_max_scheda$max_scheda)
    
    writeDT(dt_name = 'fishtrade_max_scheda', data = fishtrade_max_scheda, sql_filter = sql_filter)
    
    return(fishtrade_max_scheda)
    
  } else {
    data_maxscheda <- merge(.data_full, fishtrade_max_scheda, by = c('reporter', 'flow', 'fao_group'), all.x = TRUE)
    data_maxscheda[is.na(max_scheda), max_scheda := "0"]
    data_maxscheda[is.na(scheda), new_scheda := max(as.numeric(max_scheda)) + 1:.N, by =  c('reporter','flow', 'fao_group')]
    data_maxscheda[!is.na(new_scheda) & is.na(scheda), 
                   scheda := paste0(fao_group, ".", stringr::str_pad(new_scheda, 4, "left", 0)), 
                   by =  c('reporter','flow', 'fao_group')]
  
    fishtrade_max_scheda <- data_maxscheda[!is.na(new_scheda), list(max_scheda = max(new_scheda)), by =  c('reporter','flow', 'fao_group')]
    data_maxscheda[, `:=`(max_scheda = NULL, new_scheda = NULL)]
    setnames(fishtrade_max_scheda, 'reporter', 'rep')
    
    writeDT(dt_name = 'fishtrade_max_scheda', data = fishtrade_max_scheda, sql_filter = sql_filter)
    return(data_maxscheda)
    
  }
  
  
}