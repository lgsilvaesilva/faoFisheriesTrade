#' @title Get trade fisheries data from SWS
#' 
#' @description This function will populate the calling environtment 
#' with raw data from Eurostat or UNSD. In the latter case, European 
#' countries will not be removed, this is something left to further 
#' computations.
#' 
#' @param .reporter TODO
#' @param .local TODO
#' 
#' @import dplyr
#' 
#' @export

get_working_data <- function(.reporter = NA, .local = NA, .year = 'all') {

  hs_filter <- ReadDatatable('fishtrade_param_hs_filter')
  stopifnot(nrow(hs_filter) > 0)
  hs_filter <- paste(hs_filter$hs_start, collapse = "|")

  hs_exclude <- ReadDatatable('fishtrade_param_hs_exclude')
  stopifnot(nrow(hs_exclude) > 0)
  hs_exclude <- paste(hs_exclude$hs_start, collapse = "|")

  if(.local == TRUE) {
    
    countries_eu <- readRDS(file = '~/projects/Fisheries_proj/fisheriesTrade/data/countries_eu.rds')
    
  } else {
    
    countries_eu <- ReadDatatable('country_european_union')
    setnames(countries_eu, "rep", "reporter")
    
  }
  
  if(.reporter %in% countries_eu$reporter | .reporter == "all") {
    
    data_source <- TRUE
    
  } else {
    
    data_source <- FALSE
    
  }
  
  is_country_eu <- .reporter %in% countries_eu$reporter
  
  .reporter <- tolower(.reporter)

  if (data_source | .reporter == 'all') {

    EURconversionUSD <- ReadDatatable('eur_conversion_usd')
    stopifnot(nrow(EURconversionUSD) > 0)

    # XXX
    esdata <- download_sws_trade_data(.reporter = ifelse(.reporter == "all", "eurostat", .reporter), .year = "all", .local = .local)
    #esdata <- readRDS("f:/esdata_dt.rds")[, chapter := NULL]

    # period is like "200052" = year 2000, week 52
    esdata[, period := stringr::str_sub(period, 1, 4)]

    # AdapTradeNames
    setnames(
      esdata,
      c("declarant", "partner", "product_nc", "flow", "stat_regime",
        "period", "value_1k_euro", "qty_ton", "sup_quantity"),
      c("reporter", "partner", "hs", "flow", "stat_regime",
        "year", "value", "weight", "qty")
    )

    # Some data has no value, no weight and 0 qty: remove
    esdata <- esdata[!(is.na(value) & is.na(weight) & dplyr::near(qty, 0))]

    if (all(unique(esdata$year) %in% EURconversionUSD$eusd_year) == FALSE) {
      stop("Exchange rate not available for all years")
    }

    ## Only regime 4 is relevant for Eurostat data.
    # Remove totals, 1010 = "European Union", 1011 = "Extra-European Union", see
    # http://ec.europa.eu/eurostat/documents/3859598/5889816/KS-BM-05-002-EN.PDF
    esdata <-
      esdata[
        stat_regime == "4" & !(reporter == "EU" | partner %in% c("1010", "1011"))
      ][, stat_regime := NULL]

    # removeNonNumeric
    esdata <- esdata[grepl("^[0-9]+$", hs)]

    # Fisheries filter
    #
    # 030760 = "snails, other than sea snails"
    # 160558 = "snails, other than sea snails prepared or preserved"
    esdata <-
      esdata[
        stringr::str_detect(hs, paste0("^(", hs_filter, ")"))
      ][!grepl(paste0("^(", hs_exclude, ")"), hs)]

    # adaptTradeDataTypes
    esdata[, `:=`(year = as.integer(year), reporter = as.integer(reporter),
                  partner = as.integer(partner), flow = as.integer(flow))]

    # Keep only HS with more than 6 digits
    esdata <- esdata[nchar(hs) >= 6]

    # Convert ES geonomenclature country/area codes to FAO codes, then to M49.
    esdata[, `:=`(reporter = faoswsTrade::convertGeonom2FAO(reporter),
                  partner  = faoswsTrade::convertGeonom2FAO(partner))]

    # XXX https://github.com/SWS-Methodology/faoswsTrade/issues/147
    # Will be eventually "Other nei"
    esdata[is.na(partner), partner := 252]


    esdata[, `:=`(reporter = as.integer(faoswsUtil::fs2m49(as.character(reporter))),
                  partner  = as.integer(faoswsUtil::fs2m49(as.character(partner))))]

    esdata[is.na(partner), partner := 896L] # "Other, nei"

    # set qty is the same as weight, and qunit accordingly. hsrep is not available
    esdata[, `:=`(qty = weight, qunit = "8", hsrep = NA_character_, src = "eurostat")]

    # Set HS codes to 12 digits
    esdata[, `:=`(hslength = nchar(hs), hs = stringr::str_pad(hs, 12, "right", "0"))]

    # Convert value to USD and weight to KG to make it compatible with tldata
    esdata <-
      EURconversionUSD[,
        list(year = as.integer(eusd_year),
             eur_usd = as.numeric(eusd_exchangerate))
      ][
        esdata, on = "year"
      ][,
        # original `value` is in k EUR, `eur_usd` is quantity of USD per EUR
        `:=`(value = value * eur_usd * 1000, weight = weight * 1000)
      ][,
        eur_usd := NULL
      ]

    # Reorder
    setcolorder(esdata, c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit", "hslength", "src"))

    # Global assignment
    esdata <<- esdata
  }


  if (!data_source | .reporter == 'all') {

    if (.local == TRUE) {
      
      unsdpartnersblocks <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/data_description/unsdpartnersblocks.rds")
      
    } else {
      
      unsdpartnersblocks <- faosws::ReadDatatable("unsdpartnersblocks")
      
    }

    stopifnot(nrow(unsdpartnersblocks) > 0)

    unsdpartnersblocks <-
      unsdpartnersblocks[,
        list(
          wholepartner = as.numeric(unsdpb_rtcode),
          partner      = as.numeric(unsdpb_formula)
        )
      ][wholepartner %in% c(251, 381, 579, 581, 757, 842)]


    # XXX
    tldata <- download_sws_trade_data(.reporter = ifelse(.reporter == "all", "unsd", .reporter), 
                                      .year = .year, 
                                      .local = .local)

    # AdapTradeNames
    setnames(
      tldata,
      c("tyear", "rep", "prt", "flow", "comm", "hsrep",
      "tvalue", "weight", "qty", "qunit"),
      c("year", "reporter", "partner", "flow", "hs", "hsrep",
      "value", "weight", "qty", "qunit")
    )

    # removeNonNumeric
    tldata <- tldata[grepl("^[0-9]+$", hs)]

    # Fisheries filter ----
    #
    # 030760 = "snails, other than sea snails"
    # 160558 = "snails, other than sea snails prepared or preserved"
    tldata <-
      tldata[
        stringr::str_detect(hs, paste0("^(", hs_filter, ")[0-9]*"))
      ][!grepl(paste0("^(", hs_exclude, ")"), hs)]

    # Remove EU (97) ----
    tldata <- tldata[reporter != "97"]

    # adaptTradeDataTypes ----
    tldata[, `:=`(year = as.integer(year), reporter = as.integer(reporter),
                  partner = as.integer(partner), flow = as.integer(flow))]

    # Fisheries: re-imports in imports and re-exports separately ----
    tldata[, flow := ifelse(flow == 4, 1, flow)]

    # Keep only HS with more than 6 digits ----
    tldata <- tldata[nchar(hs) >= 6]


    # M49 to FAO area list ----

    ## 1. TL M49 codes (which are different from official M49) are
    ## converted in country codes using a specific conversion
    ## table (`unsdpartnersblocks`) provided by Team ENV.
    tldata <-
      unsdpartnersblocks[tldata, on = "partner"][,
        partner := ifelse(is.na(wholepartner), partner, wholepartner)
      ][, wholepartner := NULL]

    tldata[, `:=`(reporter = as.character(faoswsTrade::convertComtradeM49ToFAO(reporter)),
                  partner  = as.character(faoswsTrade::convertComtradeM49ToFAO(partner)))]

    # Remove invalid (inexistent) reporters ----
    tldata <-
      plyr::ddply(
        tldata,
        .variables = "year",
        .fun = function(x) removeInvalidReporters(x, unique(x$year))
      )

    setDT(tldata)

    # FS to M49 Codes ----
    tldata[, `:=`(reporter = as.integer(faoswsUtil::fs2m49(as.character(reporter))),
                  partner  = as.integer(faoswsUtil::fs2m49(as.character(partner))))]

    # FAO code 252 was converted to NA (and few NAs also existed)
    tldata[is.na(partner), partner := 896L]

    tldata[, src := "unsd"]

    # Set HS codes to 12 digits ----
    tldata[, `:=`(hslength = nchar(hs), hs = stringr::str_pad(hs, 12, "right", "0"))]

    # Reorder
    tldata <- tldata[, c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit", "hslength", "src"), with = F]
    # setcolorder(tldata, c("year", "reporter", "partner", "flow", "hs", "hsrep", "value", "weight", "qty", "qunit", "hslength", "src"))

    # XXX remove 5 transactions (in the whole dataset) that have "S3" as HS version: ----
    # > tldata[, .N, hsrep]
    #    hsrep       N
    # 1:    H0   17631
    # 2:    H1  460363
    # 3:    H2 1174853
    # 4:    H3 2160322
    # 5:    S3       5
    # 6:    H4 4418000
    tldata <- tldata[hsrep != "S3"]

    # Global assignment
    tldata <<- tldata
  }

}

