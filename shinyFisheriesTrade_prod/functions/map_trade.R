map_trade <- function(.data, .map) {
  
  .data <- copy(.data)
  .data <- .data[, `:=`(startyear = as.numeric(year), endyear = as.numeric(year))]
  .map <- copy(.map)
  
  setnames(.map, 'tariff_line','comm')
  
  # data_class <- sapply(.data[, list(rep, comm, flow, startyear, endyear)], typeof)
  # convert_class <- sprintf("`:=`(%s)", toString(paste0(names(data_class), " = ", "as.", data_class, "(", names(data_class), ")")))
  
  data_class <- sapply(.data[, list(rep, comm, flow, startyear, endyear)], typeof)
  convert_class <- sprintf("`:=`(%s)", toString(paste0(names(data_class), " = ", "as.", "character", "(", names(data_class), ")")))
  
  .map[, eval(parse(text = convert_class))]
  .data[, eval(parse(text = convert_class))]
  
  .map[, `:=`(startyear = as.numeric(startyear), endyear = as.numeric(endyear))]
  .data[, `:=`(startyear = as.numeric(startyear), endyear = as.numeric(endyear))]
  
  setkey(.map, rep, comm, flow, startyear, endyear)
  setkey(.data, rep, comm, flow, startyear, endyear)
  
  data_mapped <- foverlaps(.data, .map)
  
  data_mapped[, `:=`(i.startyear = NULL,
                     i.endyear = NULL,
                     startyear = NULL,
                     endyear = NULL,
                     hs6_use = NULL#,
                     # isscfc_descr = NULL
  )]
  
  return(data_mapped)
  
}