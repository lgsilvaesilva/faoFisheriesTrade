
#' @title Pulling fisheries trade data 
#'
#' @description This function aiming to pull trade data from SWS. It allows the user to combine different sources
#' for each flow.
#' 
#' @param .reporter M49 country code.
#' @param .year year with four digits (e.g 2010)
#' @param .source "eurostat", "unsd", "tdm"
#' @param .flow 1, 2, "all"
#'
#' @return
#' @export
#'
#' @importFrom get_tldata, get_esdata, chapters_filter
get_tradedata <- function(.reporter, .year, .source, .flow) {
  
  stopifnot(length(.source) == length(.flow))
  
  .source <- tolower(.source)
  
  out <- list()
  
  ##++ Getting data from EUROSTAT ----
  if (any(.source %in% "eurostat")) {
    flow_eu <- .flow[.source == "eurostat"]
    flow_eu <- ifelse(length(flow_eu) == 2 & all(.source == "eurostat"), "all", flow_eu)
    
    esdata <- get_esdata(.reporter = .reporter, .years = .year, .flow = flow_eu)
    
    if(!is.null(esdata)) {
      
      out$tradedata$esdata <- process_esdata(.data = esdata)
      
    } else {
      
      out$tradedata$esdata <- NULL
      
    }
    
    
  }
  
  ##-- Getting data from UNSD ----
  if (any(.source %in% "unsd")) {
    flow_unsd <- .flow[.source == "unsd"]
    flow_unsd <- ifelse(length(flow_unsd) == 2 & all(.source == "unsd"), "all", flow_unsd)
    
    unsdpartnersblocks <- faosws::ReadDatatable("unsdpartnersblocks")
    .reporter_m49 <- unsdpartnersblocks[unsdpb_formula == .reporter & unsdpb_endvalidyear >= .year, unique(unsdpb_rtcode)]
    
    if(length(.reporter_m49) == 0) .reporter_m49 <- .reporter
    
    if(.reporter_m49 == 97) {
      
      out$tradedata <- rbindlist(out$tradedata)
      
      out$call <- list(reporter = .reporter, year = .year, source = .source, flow = .flow)
      
      return(out)
    }
    
    tldata <- get_tldata(.reporter = .reporter_m49, .years = .year, .flow = flow_unsd)
    
    if(!is.null(tldata) & nrow(tldata) != 0) {
      
      out$tradedata$tldata <- process_tldata(.data = tldata)
      
      # china
      out$tradedata$tldata[, reporter := ifelse(reporter == 1248, 156, reporter)]
      out$tradedata$tldata[, partner := ifelse(partner == 1248, 156, partner)]
      
    } else {
      
      out$tradedata$tldata <- NULL
      
    }
    
    
  }
  
  ##-- Getting data from TDM ----
  if (any(.source %in% "tdm")) {
    flow_tdm <- .flow[.source == "tdm"]
    flow_tdm <- ifelse(length(flow_tdm) == 2 & all(.source == "tdm"), "all", flow_tdm)
    
    unsdpartnersblocks <- faosws::ReadDatatable("unsdpartnersblocks")
    .reporter_m49 <- unsdpartnersblocks[unsdpb_formula == .reporter & unsdpb_endvalidyear >= .year, unsdpb_rtcode]
    
    if(length(.reporter_m49) == 0) .reporter_m49 <- .reporter
    
    tdm_data <- get_tdm(.reporter = .reporter_m49, .years = .year, .flow = flow_tdm)
    
    if(!is.null(tdm_data) & nrow(tdm_data) != 0) {
      
      out$tradedata$tdm_data <- process_tldata(.data = tdm_data, .source = .source)
      # china
      out$tradedata$tdm_data[, reporter := ifelse(reporter == 1248, 156, reporter)]
      out$tradedata$tdm_data[, partner := ifelse(partner == 1248, 156, partner)]
      
    } else {
      
      out$tradedata$tdm_data <- NULL
      
    }
    
    
  }
  
  out$tradedata <- rbindlist(out$tradedata)
  
  out$call <- list(reporter = .reporter, year = .year, source = .source, flow = .flow)
  
  return(out)
  
}