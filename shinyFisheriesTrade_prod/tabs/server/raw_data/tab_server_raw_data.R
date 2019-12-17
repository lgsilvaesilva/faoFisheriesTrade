
##-- Reactive Values ----
rd_values <- reactiveValues(trade_raw_data = data.table())

##-- Get years ----
#' Get trade years according to the data source
#' Tariff Line or Eurostat
get_years <- reactive({
  
  dts <- FetchDatatableNames()
  
  trade_years <-
    dts[grepl(input$rd_source, dts)] %>%
    stringr::str_sub(start = -4) %>%
    unique() %>%
    sort() %>%
    grep(pattern = "^[1-2][0-9]{3}", value = T) %>%
    as.numeric()
  
  return(trade_years)
})

##-- Button: Years ----
output$btn_trade_year <- renderUI({
  trade_years <- get_years()
  
  pickerInput(inputId = "rd_years", 
              multiple = TRUE,
              label = "Year", 
              choices = trade_years,
              options = list(
                `live-search` = TRUE,
                `actions-box` = TRUE)
  )
  
})

##-- Button: Country ----
output$btn_trade_country <- renderUI({
  
  country_choices <- m49_codes$reporter
  names(country_choices) <- as.character(m49_codes$country)
  country_choices <- sort(country_choices)
  
  pickerInput(inputId = "rd_country", 
              multiple = TRUE,
              label = "Country", 
              choices = country_choices,
              options = list(
                `live-search` = TRUE,
                `actions-box` = TRUE)
  )
  
})

##-- Confirmation dialog box ----
observeEvent(input$btn_load_data, {
  
  confirmSweetAlert(
    session = session,
    inputId = "run_load_data",
    type = "warning",
    text = tags$span(tags$h3("Do you want to confirm?")),
    title = "Loading data could take a while.",
    danger_mode = TRUE, 
    html = T
  )
  
})

##-- Get Raw data ----
get_raw_tradedata <- eventReactive(input$run_load_data, {
  
  validate(
    need(input$rd_years, 'Please choose at least one year'),
    need(input$rd_country, 'Please choose at least one country.')
  )
  
  if(input$run_load_data == FALSE) return(NULL)
  
  year_selected     <- input$rd_years
  source_selected   <- input$rd_source
  reporter_selected <- input$rd_country
  # tradedata <- list()
  
  ##-- Tariff Line Data ----
  if(source_selected == "ct_tariffline_unlogged_") {
    
    tradedata <- get_tldata(.reporter = reporter_selected, .years = year_selected)
    
  }
  
  ##-- EuroStat Data ----
  if(source_selected == "ce_combinednomenclature_unlogged_") {
    
    rep_sql <- sprintf("declarant = '%s'", m49_to_geonom(reporter_selected))
    hs_filter_sql <- paste(hs_filter$hs_start, "%|", sep = "", collapse = "")
    hs_filter_sql <- paste0("product_nc SIMILAR TO '", substr(hs_filter_sql, 1, nchar(hs_filter_sql) - 1), "'")
    
    hs_exclude_sql <- paste(hs_exclude$hs_start, "%|", sep = "", collapse = "")
    hs_exclude_sql <- paste0("product_nc NOT SIMILAR TO '", substr(hs_exclude_sql, 1, nchar(hs_exclude_sql) - 1), "'")
    
    for (year in year_selected) {
      tradedata[[as.character(year)]] <- ReadDatatable(paste0(source_selected, year), 
                                                       where =  paste(chapters_condition, rep_sql, hs_filter_sql, sep = " AND "), 
                                                       columns = c("declarant", "partner", "product_nc", "flow", "stat_regime",
                                                                   "period", "value_1k_euro", "qty_ton", "sup_quantity")
      )
    }  
    
    tradedata <- data.table::rbindlist(tradedata)
    
    setnames(
      tradedata,
      c("declarant", "partner", "product_nc", "flow", "stat_regime", "period", "value_1k_euro", "qty_ton", "sup_quantity"),
      c("reporter", "partner", "hs", "flow", "stat_regime", "year", "value", "weight", "qty")
    )
    
  }
  
  ##-- Exclude ornamental fishes ----
  tradedata <- tradedata[!grepl(paste0("^(", paste0(hs_exclude$hs_start, collapse = "|"), ")"), comm), ]
  
  if(nrow(tradedata) == 0) return(NULL)
  
  return(tradedata)
  
})

##-- Table: raw data ----
output$tbl_raw_data <- DT::renderDataTable(server = TRUE, {
  
  rd_values$trade_raw_data <- get_raw_tradedata()
  
  if(is.null(rd_values$trade_raw_data)) {
    
    sendSweetAlert(
      session = session,
      title = "No data",
      text = "There is no data to be shown.",
      type = "info"
    )
    
    return(NULL)
    
  }
  
  
  datatable(rd_values$trade_raw_data, 
            rownames = FALSE, 
            filter = 'top'
  ) 
  
})

##-- Download raw data ----

output$download_raw_data <- downloadHandler(
  
  filename = function() {
    
    paste(Sys.Date(), "_trade_raw_data", ".csv", sep = "")
    
  },
  
  content = function(file) {
    
    write.csv(rd_values$trade_raw_data, file, row.names = FALSE)
    
  }
)








