# source('~/projects/Fisheries_proj/shinyFisheriesTrade/functions/removeInvalidReporters.R', echo=TRUE)

process_esdata <- function(.data) {
  
  data <- copy(.data)
  
  if(nrow(data) == 0) return(data)
  
  EURconversionUSD <- ReadDatatable('eur_conversion_usd')
  stopifnot(nrow(EURconversionUSD) > 0)
  
  # period is like "200052" = year 2000, week 52
  data[, period := stringr::str_sub(period, 1, 4)]
  
  # AdapTradeNames
  setnames(
    data,
    c("declarant", "partner", "product_nc", "flow", "stat_regime",
      "period", "value_1k_euro", "qty_ton", "sup_quantity"),
    c("reporter", "partner", "hs", "flow", "stat_regime",
      "year", "value", "weight", "qty")
  )
  
  # Some data has no value, no weight and 0 qty: remove
  data <- data[!(is.na(value) & is.na(weight) & dplyr::near(qty, 0))]
  
  if (all(unique(data$year) %in% EURconversionUSD$eusd_year) == FALSE) {
    stop("Exchange rate not available for all years")
  }
  
  ## Only regime 4 is relevant for Eurostat data.
  # Remove totals, 1010 = "European Union", 1011 = "Extra-European Union", see
  # http://ec.europa.eu/eurostat/documents/3859598/5889816/KS-BM-05-002-EN.PDF
  data <- data[ stat_regime == "4" & !(reporter == "EU" | partner %in% c("1010", "1011"))]
  data[, stat_regime := NULL]
  
  # removeNonNumeric
  data <- data[grepl("^[0-9]+$", hs), ]
  
  # adaptTradeDataTypes
  data[, `:=`(year = as.integer(year), 
              reporter = as.integer(reporter),
              partner = as.integer(partner), 
              flow = as.integer(flow))]
  
  # Keep only HS with more than 6 digits
  data <- data[nchar(hs) >= 6]
  
  # Convert ES geonomenclature country/area codes to FAO codes, then to M49.
  data[, `:=`(reporter = faoswsTrade::convertGeonom2FAO(reporter),
              partner  = faoswsTrade::convertGeonom2FAO(partner))]
  
  # XXX https://github.com/SWS-Methodology/faoswsTrade/issues/147
  # Will be eventually "Other nei"
  data[is.na(partner), partner := 252]
  
  data[, `:=`(reporter = as.integer(faoswsUtil::fs2m49(as.character(reporter))),
              partner  = as.integer(faoswsUtil::fs2m49(as.character(partner))))]
  
  data[is.na(partner), partner := 896L] # "Other, nei"
  
  # set qty is the same as weight, and qunit accordingly. hsrep is not available
  data[, `:=`(qty = weight, qunit = "8", hsrep = NA_character_, src = "eurostat")]
  
  # Set HS codes to 12 digits
  data[, `:=`(hslength = nchar(hs), hs = stringr::str_pad(hs, 12, "right", "0"))]
  
  # Convert value to USD and weight to KG to make it compatible with tldata
  EURconversionUSD <- EURconversionUSD[, list(year = as.integer(eusd_year), 
                                              eur_usd = as.numeric(eusd_exchangerate))]
  
  data <- EURconversionUSD[data, on = "year"]
  
  # original `value` is in k EUR, `eur_usd` is quantity of USD per EUR
  data[, `:=`(value = value * eur_usd * 1000, weight = weight * 1000)]
  data[, eur_usd := NULL ]
  
  # Reorder
  setcolorder(data, c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit", "hslength", "src"))
  
  # Global assignment
}
