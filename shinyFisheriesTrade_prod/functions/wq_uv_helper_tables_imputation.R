wq_imputation <- function(data, 
                          helper_table = "", 
                          method = 'wq',
                          no_imputation, 
                          local = TRUE) {
  
  if(!any(class(data) %in% "data.table")) {
    
    data <- copy(setDT(data))
    
  } else {
    
    data <- copy(data)
    
  }
  
  if(local == TRUE) {
    # from local machine
    helper_table_dt <- readRDS(helper_table)
    helper_table <- substr(basename(helper_table), 1, regexpr("\\.", basename(helper_table))[1] - 1)
    
  } else {
    # from SWS
    helper_table_dt <- ReadDatatable(helper_table)
    
  }
  
  if(nrow(helper_table_dt) == 0) stop("There is no data in the helper table")
  
  ## Adapt class variable
  helper_table_dt[, flow := as.numeric(flow)] # In the tables the field flow is as character
  
  
  if(method == 'wq') {
    ## Rename
    setnames(helper_table_dt, 
             old = colnames(helper_table_dt)[startsWith(colnames(helper_table_dt), "wq")],
             new = 'wqs')
    
    # Will be used to check that the dimension stays the same after merging
    nrow_data_before <- nrow(data)
    data <- helper_table_dt[data, on = c("year", "reporter", "flow", "hs", "qunit")]
    nrow_data_after <- nrow(data)
    
    if (nrow_data_after != nrow_data_before) stop("Merging helper tables created duplications.")
    
    
    data[  substr(hs, 1, 4) != no_imputation & 
             is.na(weight) & 
             !is.na(qty) & 
             !is.na(wqs), 
           `:=`(weight = qty * wqs, imputation_method = helper_table)]  
  }
  
  if(method == "uv") {
    ## Rename
    setnames(helper_table_dt, 
             old = colnames(helper_table_dt)[startsWith(colnames(helper_table_dt), "uv")],
             new = 'uvs')
    
    # Will be used to check that the dimension stays the same after merging
    nrow_data_before <- nrow(data)
    data <- helper_table_dt[data, on = c("year", "reporter", "flow", "hs", "qunit")]
    nrow_data_after <- nrow(data)
    
    if (nrow_data_after != nrow_data_before) stop("Merging helper tables created duplications.")
    
    
    data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(value) & !is.na(uvs),
         `:=`(weight = value / uvs,
              imputation_method = "uv_region_year")]
  }
  
   
  return(data)
   
}


#' # XXX: this tables should have unique rows!
#' wq_table_reporter_year <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/wq_table_reporter_year.rds")
#' wq_table_reporter      <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/wq_table_reporter.rds")
#' wq_table_region_year   <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/wq_table_region_year.rds")
#' wq_table_region        <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/wq_table_region.rds")
#' wq_table_global_year   <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/wq_table_global_year.rds")
#' wq_table_global        <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/wq_table_global.rds")
#' 
#' uv_table_reporter_year <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/uv_table_reporter_year.rds")
#' uv_table_region_year   <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/uv_table_region_year.rds")
#' uv_table_global_year   <- readRDS("~/projects/Fisheries_proj/fisheriesTrade/data/helper_tables/uv_table_global_year.rds")
#' 
#' 
#' ########################################################################
#' m49_codes <- generate_regions()
#' data <- m49_codes[data, on = "reporter"]
#' ########################################################################
#' 
#' 
#' # Will be used to check that the dimension stays the same after merging
#' nrow_data_before <- nrow(tldata)
#' 
#' data[, flow := as.character(flow)] # In the tables the field flow is as character
#' 
#' data <- wq_table_reporter_year[data, on = c("year", "reporter", "flow", "hs", "qunit")]
#' data <- wq_table_reporter[data, on = c("reporter", "flow", "hs", "qunit")]
#' data <- wq_table_region_year[data, on = c("year", "region", "flow", "hs6", "qunit")]
#' data <- wq_table_region[data, on = c("region", "flow", "hs6", "qunit")]
#' data <- wq_table_global_year[data, on = c("year", "flow", "hs6", "qunit")]
#' data <- wq_table_global[data, on = c("flow", "hs6", "qunit")]
#' 
#' data <- uv_table_reporter_year[data, on = c("year", "reporter", "flow", "hs")]
#' data <- uv_table_region_year[data, on = c("year", "region", "flow", "hs6")]
#' data <- uv_table_global_year[data, on = c("year", "flow", "hs6")]
#' 
#' nrow_data_after <- nrow(data)
#' 
#' if (nrow_data_after != nrow_data_before) {
#'   stop("Merging helper tables created duplications.")
#' }
#' 
#' ##-- Estimate weight per unit ----
#' #' wq tables weight imputation:
#' #' If weight is NA AND 
#' #'    qty is not NA AND 
#' #'    wqs_XXX is not NA
#' #' then weight is filled by the qty * XXX
#' #' 
#' ##-- Imputation made on the Tariff Line Level
#' ##++ Reporter Year ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(qty) & !is.na(wqs_reporter_year), 
#'      `:=`(weight = qty * wqs_reporter_year,
#'           imputation_method = "wq_reporter_year")]
#' 
#' ##-- Estimate the Unit Value ----
#' ##++ Reporter-Year ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(value) & !is.na(uvs_reporter_year), 
#'      `:=`(weight = value / uvs_reporter_year,
#'           imputation_method = "uv_reporter_year")]
#' 
#' ##-- Estimate weight per unit ----
#' ##++ Reporter ----  
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(qty) & !is.na(wqs_reporter), 
#'      `:=`(weight = qty * wqs_reporter,
#'           imputation_method = "wq_reporter")]
#' 
#' ##-- Unit Value Total ----
#' uv_tot <- data[!is.na(value) & !is.na(weight), 
#'                list(uvt = sum(value) / sum(weight)), 
#'                by = c("year", "reporter", "flow", "hs")]
#' 
#' data <- uv_tot[data, on = c("year", "reporter", "flow", "hs")]
#' data[, shipments := .N, list(year, reporter, partner, flow, hs)]
#' data[is.na(weight) & shipments > 1 & !is.na(value) & !is.na(uvt), 
#'      `:=`(weight = value / uvt,
#'           imputation_method = "uv_total")]
#' 
#' ##++ Region-Year ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(qty) & !is.na(wqs_region_year), 
#'      `:=`(weight = qty * wqs_region_year,
#'           imputation_method = "wq_region_year")]
#' 
#' ##-- Estimate the Unit Value ----
#' ##++ Region-Year ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(value) & !is.na(uvs_region_year),
#'      `:=`(weight = value / uvs_region_year,
#'           imputation_method = "uv_region_year")]
#' 
#' ##++ Global-Year ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(value) & !is.na(uvs_global_year), 
#'      `:=`(weight = value / uvs_global_year,
#'           imputation_method = "uv_global_year")]
#' 
#' ##++ Region ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(qty) & !is.na(wqs_region), 
#'      `:=`(weight = qty * wqs_region,
#'           imputation_method = "wq_region")]
#' 
#' ##++ Global-Year ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(qty) & !is.na(wqs_global_year), 
#'      `:=`(weight = qty * wqs_global_year,
#'           imputation_method = "wq_global_year")]
#' 
#' #++ Global ----
#' data[substr(hs, 1, 4) != no_imputation & is.na(weight) & !is.na(qty) & !is.na(wqs_global), 
#'      `:=`(weight = qty * wqs_global,
#'           imputation_method = "wq_global")]
#' 
#' 
#' 
#' # Clean data
#' data[,
#'      `:=`(
#'        wqs_global        = NULL,
#'        wqs_global_year   = NULL,
#'        wqs_region        = NULL,
#'        wqs_region_year   = NULL,
#'        wqs_reporter      = NULL,
#'        wqs_reporter_year = NULL,
#'        uvs_global_year   = NULL,
#'        uvs_region_year   = NULL,
#'        uvs_reporter_year = NULL,
#'        uvt = NULL
#'      )
#'      ]
#' 
#' #'*When we aggregate countries in the HS levels do not make sense, because those codes could be different means in the HS level.*
#' wq_table_hs6 = data[qunit != 8 & !is.na(weight) & !is.na(qty),
#'                     list(wqm_hs6 = median(weight / qty)),
#'                     by = list(year, hs6, qunit)]
#' 
#' 
#' data <- wq_table_hs6[data, on = c("year", "qunit", "hs6")]
#' 
#' # XXX in this case we didn't test how many reporters we have to
#' # calculate the median. Should we?
#' data[is.na(weight) & !is.na(qty) & !is.na(wqm_hs6), 
#'      `:=`(weight = qty * wqm_hs6,
#'           imputation_method = "wqm_hs6_reporter_year")]
#' 
#' data[, wqm_hs6 := NULL]

return(data)