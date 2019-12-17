# source('~/projects/Fisheries_proj/shinyFisheriesTrade/functions/removeInvalidReporters.R', echo=TRUE)

process_tldata <- function(.data, .source = 'unsd') {
  
  data <- copy(.data)
  
  if(nrow(data) == 0) return(data)
  
  # unsdpartnersblocks ----
  unsdpartnersblocks <- faosws::ReadDatatable("unsdpartnersblocks")
  unsdpartnersblocks <- unsdpartnersblocks[unsdpb_rtcode %in% c(251, 381, 579, 581, 757, 842),
                                           list(
                                             wholepartner = as.numeric(unsdpb_rtcode),
                                             partner      = as.numeric(unsdpb_formula)
                                           )]
  
  # AdapTradeNames ----
  setnames(
    data, 
    old = c("tyear", "rep", "prt", "flow", "comm", "hsrep", "tvalue", "weight", "qty", "qunit"),
    new = c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit")
  )
  
  # removeNonNumeric ----
  data <- data[grepl("^[0-9]+$", hs), ]
  
  # Remove EU (97) ----
  data <- data[reporter != "97", ]
  
  # adaptTradeDataTypes ----
  data[, `:=`(year = as.integer(year), 
              reporter = as.integer(reporter),
              partner = as.integer(partner), 
              flow = as.integer(flow))
       ]
  
  # Fisheries: re-imports in imports and re-exports separately ----
  data[, flow := ifelse(flow == 4, 1, flow)]
  # data[, flow := ifelse(flow == 3, 2, flow)] # from 1_tradeDocumentation
  
  # Keep only HS with more than 6 digits ----
  data <- data[nchar(hs) >= 6, ]
  
  # M49 to FAO area list ----
  
  ## 1. TL M49 codes (which are different from official M49) are
  ## converted in country codes using a specific conversion
  ## table (`unsdpartnersblocks`) provided by Team ENV.
  data <- unsdpartnersblocks[data, on = "partner"]
  data <- data[, partner := ifelse(is.na(wholepartner), partner, wholepartner)][, wholepartner := NULL]
  
  # fcl_m49_mapping <- ReadDatatable('fcl_m49_mapping', columns = c('fao_code', 'm49_code'))
  # fcl_m49_mapping <- fcl_m49_mapping[!(is.na(fao_code) | is.na(m49_code)), ]
  # 
  # data <- merge(data, fcl_m49_mapping, by.x = 'reporter', by.y = 'm49_code', all.x = TRUE)
  # data[, reporter := fao_code][, fao_code := NULL]
  # 
  # data <- merge(data, fcl_m49_mapping, by.x = 'partner', by.y = 'm49_code', all.x = TRUE)
  # data[, partner := fao_code][, fao_code := NULL]
  
  data[, `:=`(reporter = as.character(faoswsTrade::convertComtradeM49ToFAO(reporter)),
              partner  = as.character(faoswsTrade::convertComtradeM49ToFAO(partner)))]
  
  # Remove invalid (inexistent) reporters ----
  
  data <- removeInvalidReporters(tradedata = data, year = unique(data$year))
  
  # FS to M49 Codes ----
  data[, `:=`(reporter = as.integer(faoswsUtil::fs2m49(as.character(reporter))),
              partner  = as.integer(faoswsUtil::fs2m49(as.character(partner))))]
  
  # FAO code 252 was converted to NA (and few NAs also existed)
  data[is.na(partner), partner := 896L]
  src_dt <- data.table(flow = c(1, 2, 3), src = c(.source[1], .source[2], .source[2]))
  data <- merge(data, src_dt, by = "flow", all.x = TRUE)
  
  # Set HS codes to 12 digits ----
  data[, `:=`(hslength = nchar(hs), 
              hs = stringr::str_pad(hs, 12, "right", "0"))]

  # Reorder
  data <- data[, c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit", "hslength", "src"), with = F]
  # setcolorder(data, c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit", "hslength", "src"))
  
  # XXX remove 5 transactions (in the whole dataset) that have "S3" as HS version: ----
  # > data[, .N, hsrep]
  #    hsrep       N
  # 1:    H0   17631
  # 2:    H1  460363
  # 3:    H2 1174853
  # 4:    H3 2160322
  # 5:    S3       5
  # 6:    H4 4418000
  if(any(.source == "unsd")) {
    data[is.na(hsrep), hsrep := ""]
    data <- data[hsrep != "S3", ]  
  }
  
  
  # Global assignment
  return(data)
  
}
