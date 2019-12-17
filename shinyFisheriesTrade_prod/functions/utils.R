unit_conversion <- function(data) {
  
  
  if(!any(class(data) %in% "data.table")) {
    
    data <- copy(setDT(data))
    
  } else {
    data <- copy(data)
  }
  
  
  test_col_names <- all(c('weight', 'qty', 'qunit') %in% names(data))
  
  if(!test_col_names) stop("The data should have the columns: weight, qty, and qunit")
  
  ##-- Mathematical conversion of units and “imputation” ----
  data[qunit == "8"  & is.na(weight) & !is.na(qty), weight := qty]
  data[qunit == "6"  & !is.na(qty), `:=`(qty = 2 * qty,    qunit = "5")]
  data[qunit == "9"  & !is.na(qty), `:=`(qty = 1000 * qty, qunit = "5")]
  data[qunit == "11" & !is.na(qty), `:=`(qty = 12 * qty,   qunit = "5")]
  data[qunit == "12" & !is.na(qty), `:=`(qty = 1000 * qty, qunit = "7")]
  
  return(data)
  
}

##-- Imputation methods ----
##++ Partially Aggregation ----
partially_aggregation <- function(data, threshold = 0.5) {
  
  
  if(!any(class(data) %in% "data.table")) {
    
    data <- copy(setDT(data))
    
  } else {
    data <- copy(data)
  }
  
  
  test_col_names <- all(c('weight', 'qty', 'qunit', 'value') %in% names(data))
  
  if(!test_col_names) stop("The data should have the columns: weight, qty, value, and qunit")
  
  data[, `:=`(na_value = is.na(value), 
              na_weight = is.na(weight), 
              na_qty = is.na(qty))]
  
  data <- data[, list(value = sum(value), 
                      weight = sum(weight), 
                      qty = sum(qty)), 
               by = c("year", "rep", "prt", "flow", "comm", "hsrep", 
                      "hslength", "qunit", "na_value", "na_weight", "na_qty", "imputation_method")]
  
  
  ##-- Imputation by partially reported weight ----
  data[,`:=`(n = .N,
             percvanna = sum(value[!is.na(weight)]) / sum(value),
             uv = sum(value[!is.na(weight)]) / sum(weight, na.rm = TRUE)), 
       by = c("year", "rep", "prt", "flow", "comm", "hsrep", "hslength")]
  
  ## Compute the % of valid value is greater than 50%
  data[, cond := (n > 1 & percvanna >= threshold & is.na(weight)), ]
  
  # imputation_control_list <- data_log(p_list = imputation_control_list, 
  #                                     p_step = "partially_aggregation", 
  #                                     p_data = data)
  
  ## Do the imputation if the condition is TRUE
  data[, `:=`(
    weight = ifelse(cond, value / uv, weight),
    qty    = ifelse(cond, value / uv, qty),
    qunit  = ifelse(cond, "8", qunit),
    imputation_method  = ifelse(cond, "partially_reported_weight", imputation_method))]
  
  data[,`:=`(na_value  = is.na(value),
             na_weight = is.na(weight),
             na_qty    = is.na(qty))]
  
  data <- data[, list(value = sum(value), 
                      weight = sum(weight), 
                      qty = sum(qty),
                      percvanna = unique(percvanna)),
               by = c("year", "rep", "prt", "flow", "comm", "hsrep", "hslength", 
                      "qunit", "na_value", "na_weight", "na_qty", "imputation_method")]
  
  return(data)
  
}

##-- UV interpolate ----

uv_hs_interpolate <- function(data, nonna_threshold = 1) {
  
  if(!any(class(data) %in% "data.table")) {
    
    data <- copy(setDT(data))
    
  } else {
    
    data <- copy(data)
    
  }
  
  data[, nonna := sum(!is.na(uv)), list(rep, prt, flow, comm)]
  data[, uv_hs_int := ifelse(nonna > nonna_threshold, zoo::na.fill(uv, "extend"), na.omit(unique(uv))[1]), 
       list(rep, prt, flow, comm)]
  
  data[is.na(uv) & !is.na(uv_hs_int), 
       `:=`(uv = uv_hs_int, 
            weight = value / uv_hs_int,
            qty = value / uv_hs_int, #LG
            qunit = "8",               #LG    
            imputation_method = "uv_interpolate")]
  
  data[, uv_hs_int := NULL]  
  
  return(data)
}

##++ Weight per unit and unit value ----
wq_uv_imputation <- function(data) {

  if(!any(class(data) %in% "data.table")) {
    
    data <- copy(setDT(data))
    
  } else {
    data <- copy(data)
  }
  
  test_col_names <- all(c('weight', 'qty', 'qunit') %in% names(data))
  
  if(!test_col_names) stop("The data should have the columns: weight, qty, and qunit")
  
  data[, `:=`(hs6 = substr(comm, 1, 6))]
  setnames(data, 'rep', 'reporter')
  # XXX: this tables should have unique rows!
  wq_table_reporter_year <- readRDS("data/helper_tables/wq_table_reporter_year.rds")
  wq_table_reporter      <- readRDS("data/helper_tables/wq_table_reporter.rds")
  wq_table_region_year   <- readRDS("data/helper_tables/wq_table_region_year.rds")
  wq_table_region        <- readRDS("data/helper_tables/wq_table_region.rds")
  wq_table_global_year   <- readRDS("data/helper_tables/wq_table_global_year.rds")
  wq_table_global        <- readRDS("data/helper_tables/wq_table_global.rds")
  uv_table_reporter_year <- readRDS("data/helper_tables/uv_table_reporter_year.rds")
  uv_table_region_year   <- readRDS("data/helper_tables/uv_table_region_year.rds")
  uv_table_global_year   <- readRDS("data/helper_tables/uv_table_global_year.rds")
  
  
  ########################################################################
  m49_codes <- generate_regions()
  m49_codes[, reporter := as.character(reporter)]
  
  data <- m49_codes[data, on = "reporter"]
  ########################################################################
  
  
  # Will be used to check that the dimension stays the same after merging
  nrow_data_before <- nrow(data)
  
  data[, flow := as.character(flow)] # In the tables the field flow is as character
  data[, reporter := as.numeric(reporter)] # In the tables the field flow is as character
  data[, year := as.numeric(year)] # In the tables the field year is as character
  
  setnames(data, 'comm', 'hs')
  
  data <- wq_table_reporter_year[data, on = c("year", "reporter", "flow", "hs", "qunit")]
  data <- wq_table_reporter[data, on = c("reporter", "flow", "hs", "qunit")]
  data <- wq_table_region_year[data, on = c("year", "region", "flow", "hs6", "qunit")]
  data <- wq_table_region[data, on = c("region", "flow", "hs6", "qunit")]
  data <- wq_table_global_year[data, on = c("year", "flow", "hs6", "qunit")]
  data <- wq_table_global[data, on = c("flow", "hs6", "qunit")]
  
  data <- uv_table_reporter_year[data, on = c("year", "reporter", "flow", "hs")]
  data <- uv_table_region_year[data, on = c("year", "region", "flow", "hs6")]
  data <- uv_table_global_year[data, on = c("year", "flow", "hs6")]
  
  nrow_data_after <- nrow(data)
  
  if (nrow_data_after != nrow_data_before) {
    stop("Merging helper tables created duplications.")
  }
  
  ##-- Estimate weight per unit ----
  #' wq tables weight imputation:
  #' If weight is NA AND 
  #'    qty is not NA AND 
  #'    wqs_XXX is not NA
  #' then weight is filled by the qty * XXX
  #' 
  ##-- Imputation made on the Tariff Line Level
  ##++ Reporter Year ----
  data[is.na(weight) & !is.na(qty) & !is.na(wqs_reporter_year), 
       `:=`(weight = qty * wqs_reporter_year,
            imputation_method = "wq_reporter_year")]
  
  ##-- Estimate the Unit Value ----
  ##++ Reporter-Year ----
  data[is.na(weight) & !is.na(value) & !is.na(uvs_reporter_year), 
       `:=`(weight = value / uvs_reporter_year,
            imputation_method = "uv_reporter_year")]
  
  ##-- Estimate weight per unit ----
  ##++ Reporter ----  
  data[is.na(weight) & !is.na(qty) & !is.na(wqs_reporter), 
       `:=`(weight = qty * wqs_reporter,
            imputation_method = "wq_reporter")]
  
  ##-- Unit Value Total ----
  uv_tot <- data[!is.na(value) & !is.na(weight), 
                   list(uvt = sum(value) / sum(weight)), 
                   by = c("year", "reporter", "flow", "hs")]
  
  data <- uv_tot[data, on = c("year", "reporter", "flow", "hs")]
  data[, shipments := .N, list(year, reporter, prt, flow, hs)]
  data[is.na(weight) & shipments > 1 & !is.na(value) & !is.na(uvt), 
         `:=`(weight = value / uvt,
              imputation_method = "uv_total")]
  
  ##++ Region-Year ----
  data[is.na(weight) & !is.na(qty) & !is.na(wqs_region_year), 
       `:=`(weight = qty * wqs_region_year,
            imputation_method = "wq_region_year")]
  
  ##-- Estimate the Unit Value ----
  ##++ Region-Year ----
  data[is.na(weight) & !is.na(value) & !is.na(uvs_region_year),
       `:=`(weight = value / uvs_region_year,
            imputation_method = "uv_region_year")]
  
  ##++ Global-Year ----
  data[is.na(weight) & !is.na(value) & !is.na(uvs_global_year), 
       `:=`(weight = value / uvs_global_year,
            imputation_method = "uv_global_year")]
  
  ##++ Region ----
  data[is.na(weight) & !is.na(qty) & !is.na(wqs_region), 
       `:=`(weight = qty * wqs_region,
            imputation_method = "wq_region")]
  
  ##++ Global-Year ----
  data[is.na(weight) & !is.na(qty) & !is.na(wqs_global_year), 
       `:=`(weight = qty * wqs_global_year,
            imputation_method = "wq_global_year")]
  
  #++ Global ----
  data[is.na(weight) & !is.na(qty) & !is.na(wqs_global), 
       `:=`(weight = qty * wqs_global,
            imputation_method = "wq_global")]
  

  
  # Clean data
  data[,
       `:=`(
         wqs_global        = NULL,
         wqs_global_year   = NULL,
         wqs_region        = NULL,
         wqs_region_year   = NULL,
         wqs_reporter      = NULL,
         wqs_reporter_year = NULL,
         uvs_global_year   = NULL,
         uvs_region_year   = NULL,
         uvs_reporter_year = NULL,
         uvt = NULL
       )
       ]
  
  #'*When we aggregate countries in the TL levels do not make sense,* 
  #'*because those codes could be different means in the TL level.*
  wq_table_hs6 = data[qunit != 8 & !is.na(weight) & !is.na(qty),
                        list(wqm_hs6 = median(weight / qty)),
                        by = list(year, hs6, qunit)]
  
  
  data <- wq_table_hs6[data, on = c("year", "qunit", "hs6")]
  
  # XXX in this case we didn't test how many reporters we have to
  # calculate the median. Should we?
  data[is.na(weight) & !is.na(qty) & !is.na(wqm_hs6), 
         `:=`(weight = qty * wqm_hs6,
              imputation_method = "wqm_hs6_reporter_year")]
  
  data[, wqm_hs6 := NULL]
  setnames(data, 'reporter', 'rep')
  setnames(data, 'hs', 'comm')
  
  return(data)
  
}

fao_confirmSweetAlert <- function (session, inputId, title = NULL, text = NULL, type = NULL, 
          danger_mode = FALSE, btn_labels = c("Cancel", "Confirm"), 
          closeOnClickOutside = FALSE, html = FALSE, p_selector = "#placeholderSweetAlert") 
{
  insertUI(selector = p_selector, where = "afterBegin", ui = useSweetAlert(),
                      immediate = TRUE, session = session)
  # insertUI(selector = "body", where = "afterBegin", ui = use_sweet_alert(), 
  #          immediate = TRUE, session = session)
  if (is.null(type)) 
    type <- jsonlite::toJSON(NULL, auto_unbox = TRUE, null = "null")
  if ("shiny.tag" %in% class(text)) 
    html <- TRUE
  if (!html) {
    text <- jsonlite::toJSON(text, auto_unbox = TRUE, null = "null")
    session$sendCustomMessage(type = "sweetalert-sw-confirm", 
                              message = list(id = inputId, title = title, text = text, 
                                             icon = type, buttons = btn_labels, dangerMode = danger_mode, 
                                             closeOnClickOutside = closeOnClickOutside))
  }
  else {
    id <- paste0("placeholder-", sample.int(1e+06, 1))
    session$sendCustomMessage(type = "sweetalert-sw-confirm", 
                              message = list(id = inputId, title = title, icon = type, 
                                             sw_id = id, text = as.character(tags$div(id = id)), 
                                             buttons = btn_labels, as_html = html, dangerMode = danger_mode, 
                                             closeOnClickOutside = closeOnClickOutside))
    insertUI(session = session, selector = paste0("#", id), 
             ui = text, immediate = TRUE)
  }
}

# fao_confirmSweetAlert <-function (session, inputId, title = NULL, text = NULL, type = "question", 
#                                   btn_labels = c("Cancel", "Confirm"), btn_colors = NULL, closeOnClickOutside = FALSE, 
#                                   showCloseButton = FALSE, html = FALSE, p_selector = "#placeholderSweetAlert",...) 
# {
#   
#   insertUI(selector = p_selector, where = "afterBegin", ui = useSweetAlert(),
#            immediate = TRUE, session = session)
#   if (is.null(type)) 
#     type <- jsonlite::toJSON(NULL, auto_unbox = TRUE, null = "null")
#   if ("shiny.tag" %in% class(text)) 
#     html <- TRUE
#   if (inherits(session, "session_proxy")) {
#     if (!starts_with(inputId, session$ns(""))) 
#       inputId <- session$ns(inputId)
#   }
#   if (!isTRUE(html)) {
#     session$sendCustomMessage(type = "sweetalert-sw-confirm", 
#                               message = list(id = inputId, as_html = html, swal = shinyWidgets:::dropNullsOrNA(list(title = title, 
#                                                                                                                     text = text, type = type, confirmButtonText = btn_labels[2], 
#                                                                                                                     cancelButtonText = btn_labels[1], showConfirmButton = !is.na(btn_labels[2]), 
#                                                                                                                     showCancelButton = !is.na(btn_labels[1]), confirmButtonColor = btn_colors[2], 
#                                                                                                                     cancelButtonColor = btn_colors[1], showCloseButton = showCloseButton, 
#                                                                                                                     allowOutsideClick = closeOnClickOutside))))
#   }
#   else {
#     id <- paste0("placeholder-", sample.int(1e+06, 1))
#     session$sendCustomMessage(type = "sweetalert-sw-confirm", 
#                               message = list(id = inputId, as_html = html, sw_id = id, 
#                                              swal = shinyWidgets:::dropNullsOrNA(list(title = title, type = type, 
#                                                                                       html = as.character(tags$div(id = id)), confirmButtonText = btn_labels[2], 
#                                                                                       cancelButtonText = btn_labels[1], showConfirmButton = !is.na(btn_labels[2]), 
#                                                                                       showCancelButton = !is.na(btn_labels[1]), confirmButtonColor = btn_colors[2], 
#                                                                                       cancelButtonColor = btn_colors[1], showCloseButton = showCloseButton, 
#                                                                                       allowOutsideClick = closeOnClickOutside))))
#     insertUI(session = session, 
#              selector = paste0("#", id),
#              ui = text, immediate = TRUE)
#   }
# }