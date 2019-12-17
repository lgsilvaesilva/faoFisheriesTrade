
# source('~/projects/Fisheries_proj/sources-fromChristian/fisheriesTrade/R/chapters_filter.R', echo=TRUE)

get_esdata <- function(.reporter, .years, .flow) {
  
  chapters_condition <- paste0("chapter IN (", chapters_filter(), ")")
  hs_filter  <- ReadDatatable('fishtrade_param_hs_filter')
  hs_exclude <- ReadDatatable('fishtrade_param_hs_exclude')
  
  if(all(nchar(.years) != 4)) {
    stop("The parameter year must have 4 digits.")
  }
  
  geonomcode <- m49_to_geonom(.reporter)
  
  ##-- Filter only that commodities specied by the FIAS team ----
  hs_filter_sql <- paste(hs_filter$hs_start, "%|", sep = "", collapse = "")
  hs_filter_sql <- paste0("product_nc SIMILAR TO '", substr(hs_filter_sql, 1, nchar(hs_filter_sql) - 1), "'")
  
  ##-- Trade table name
  trade_table_name <- "ce_combinednomenclature_unlogged_"
  trade_table <- paste0(trade_table_name, .years)
  
  ##-- Test if there is data available
  trade_available_tables <- grep(trade_table_name, faosws::FetchDatatableNames(), value = TRUE)
  if(!all(trade_table %in% trade_available_tables)) {
    
    warning("Please check these table(s): ", trade_table[!trade_table %in% trade_available_tables])
    
    return(NULL)
    
  }
  
  ##-- Concatening all conditions: chapters, commodities, and reporters
  sql_query <- paste(chapters_condition,
                     hs_filter_sql,
                     sep = " AND ")  
  
  ##-- Filter reporter ----
  if(.reporter != "all") {
    rep_sql <- sprintf("declarant IN (%s)", toString(sprintf("'%s'", geonomcode)))
    sql_query <- paste(sql_query, rep_sql, sep = " AND ")
  }
  
  ##-- Flow filter ----
  if(.flow != "all") {
    if(.flow == 2) {
      
      flow_sql <- toString(sprintf("'%s'", c(.flow, 3)))
      flow_sql <- sprintf("flow IN (%s)", flow_sql)
      
    } else {
      
      flow_sql <- sprintf("flow = '%s'", .flow)
      
    }
    sql_query <- paste(sql_query, flow_sql, sep = " AND ")
  } 
  
  
  
  ##-- Columns to be retrieved
  sql_columns <- c("declarant", "partner", 
                   "product_nc", "flow", 
                   "stat_regime", "period", 
                   "value_1k_euro", 
                   "qty_ton", 
                   "sup_quantity")
  
  ##-- The trade data are stored in tables divided by year, so it is necessary to loop all of them.
  # tradedata <- lapply(trade_table, ReadDatatable, where = sql_query, columns = sql_columns)
  
  tradedata <- list()
  for(tables in trade_table) {
    
    tradedata[[as.character(tables)]] <- ReadDatatable(table = tables, 
                                                       where = sql_query, 
                                                       columns = sql_columns)
    print(tables)
    
  }
  
  # ##-- Combine all data-year
  tradedata <- data.table::rbindlist(tradedata)
  
  # ##-- Exclude ornamental fishes ----
  hs_exclude_regex <- sprintf("^(%s)", paste0(hs_exclude$hs_start, collapse = "|"))
  tradedata <- tradedata[!grepl(hs_exclude_regex, product_nc), ]
  
  return(tradedata)
  
}


