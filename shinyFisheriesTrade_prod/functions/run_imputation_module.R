# Parameter to be used to figure out NA's masked by zero.
# If the monetary value is great than the quantile 'treshold_value' and weight value is ZERO then is assigned NA to weight, otherwise, the weight is kept with the original value. 
# threshold_value <- 0.05
# out_coef <- 5
# outlier_percent <- 5/100

# invisible(sapply(dir('~/projects/Fisheries_proj/fisheriesTrade/R', full.names = TRUE), source))

my_summary <- function(v){
  if(!any(is.na(v))){
    res <- c(summary(v),"NA's"=0)
  } else{
    res <- summary(v)
  }
  return(res)
}

data_log <- function(p_list, p_step, p_data) {

  p_list_object <- deparse(substitute(p_list))

  p_list[[p_step]]$weight_summary <- p_data[, my_summary(weight)]
  p_list[[p_step]]$value_summary  <- p_data[, my_summary(value)]
  p_list[[p_step]]$dim <- dim(p_data)

  assign(p_list_object, p_list, envir = .GlobalEnv)

}

imputation_control_list <- list()
run_imputation <- function(.reporter, 
                           .data2impute, 
                           .zero_masked_threshold = 0.05,
                           .pa_threshold = 0.5,
                           # Ornamental fish needs not to be imputed
                           .no_imputation = NULL) {
  
  ##-- Tests ----
  if(missing(.data2impute))   stop('Please provide the paramenter ".data2impute"')
  if(missing(.reporter)) stop('Please provide the paramenter ".reporter"')
  if(nrow(.data2impute) == 0)  stop('There is no data.')
  ##--
  
  ##-- Pre process data ----
  .data2impute <- copy(.data2impute)
  .data2impute[, weight := as.numeric(weight)]
  .year <- .data2impute[, unique(year)]
  
  ##-- No impute data ----
  
  #'split data in two datasets: 1) it composed by the commodities which should be imputed; 2) it composed by commodities should not be imputed
  .no_imputation          <- sprintf("^(%s)", paste(.no_imputation, collapse = '|'))
  .data2impute_not_impute <- .data2impute[grepl(pattern = .no_imputation, x = comm), ]
  .data2impute            <- .data2impute[!grepl(pattern = .no_imputation, x = comm), ]
  
  imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "initial", p_data = .data2impute)
  
  #'*XXX: for 2017 data there is no missing data. As agreed with Adrienne, when qunit == 0 and weight == 0*
  #'*and HS starts with 0301, then weight will assigned by zero.*
  .data2impute[weight == 0 & qunit == 1 & grepl('^0301([0]|[2-9])*', comm), weight := NA_real_]
  
  progress_save_imputation$inc(.45, detail = " Mathematical conversion of units.")

  ##-- Mathematical conversion of units and “imputation” ----
  .data2impute <- unit_conversion(.data2impute)
  
  ##-- NA Masked by ZERO ----
  #'*XXX*: In this part of script we try to figure out NA's masked by ZERO value
  # For instance,  if the zero comes with a monetary value of less than 1.000 dollars, so leave it as an actual zero.
  # Rule: if the weight is zero and the monetory value is great than the quantile 'threshold_value', then we assign NA.
  
  .data2impute[, zero_masked := weight == 0 & value > quantile(value, .zero_masked_threshold, na.rm = T), by = list(rep, flow, comm)]
  imputation_control_list$weight_masked$freq <- .data2impute[, sum(zero_masked, na.rm = T)]
  .data2impute[zero_masked == 1, weight := NA_real_][, zero_masked := NULL]

  ## The field imputation_method stores the method used to impute the data, when it is possible.
  .data2impute[, imputation_method := ifelse(!is.na(weight), "not_missing", "not_imputed")]
  
  ##-- Partially aggregation ----
  
  progress_save_imputation$inc(.65, detail = "  Imputation by partially reported weight.")
  .data2impute <- partially_aggregation(data = .data2impute, threshold = .pa_threshold)
  imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "partially_reported_weight", p_data = .data2impute)
  
  .data2impute[, `:=`(na_value = is.na(value), na_weight = is.na(weight))]
  .data2impute <-
    .data2impute[,
           list(
             value    = sum(value),
             weight   = sum(weight),
             qty      = sum(qty),
             qunit    = unique(qunit),
             hslength = unique(hslength),
             percvanna = unique(percvanna),
             imputation_method = toString(unique(imputation_method))
           ),
           list(year, rep, prt, flow, comm, na_value, na_weight)]
  
  .data2impute[grepl("partially_reported_weight", imputation_method), imputation_method := "partially_reported_weight"]
  
  ##-- Compute UV ----
  #' *XXX*: unit value is computed using weight or quantity?
  .data2impute[, uv := value / weight]

  
  ##-- Interpolate UV HS ----
  .data2impute <- uv_hs_interpolate(data = .data2impute)
  
  ##-- Imputation by Estimate weight per unit and univ value ----
  progress_save_imputation$inc(.75, detail = " Imputation by Estimate weight per unit.")
  .data2impute <- wq_uv_imputation(data = .data2impute)
  
  .data2impute[, `:=`(na_value = is.na(value), na_weight = is.na(weight))]
  
  .data2impute <-
    .data2impute[,
           list(
             value    = sum(value),
             weight   = sum(weight),
             qty      = sum(qty),
             qunit    = unique(qunit),
             hslength = unique(hslength),
             percvanna = unique(percvanna),
             imputation_method = toString(unique(imputation_method))
           ),
           list(year, rep, prt, flow, comm, na_value, na_weight)]
  
  .data2impute[grepl("uv_interpolate", imputation_method), imputation_method := "uv_interpolate"]
  
  
  # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  # XXX note that HS length now is set to the maximum of hslenghts XXX
  # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  ##-- Hierarchical Median Unit Value ----
  .data2impute[, `:=`(uv = value / weight, hs6 = substr(comm, 1, 6))]
  
  progress_save_imputation$inc(.85, detail = " Hierarchical Median Unit Value.")
  .data2impute[, `:=`(n_rep_hs  = .N, uvm_rep_hs  = median(uv, na.rm = TRUE)), by = list(year, rep, flow, comm)]
  # .data2impute[, `:=`(n_rep_hs8 = .N, uvm_rep_hs8 = median(uv, na.rm = TRUE)), by = list(year, .reporter, flow, hs8)]
  .data2impute[, `:=`(n_rep_hs6 = .N, uvm_rep_hs6 = median(uv, na.rm = TRUE)), by = list(year, rep, flow, hs6)]
  # .data2impute[, `:=`(n_rep_cfc = .N, uvm_rep_cfc = median(uv, na.rm = TRUE)), by = list(year, .reporter, flow, faocode)]
  
  .data2impute[,
         uvm := NA_real_
         ][,
           uvm :=
             case_when(
               n_rep_hs  >= 10 & is.na(uv) ~ uvm_rep_hs,
               # n_rep_hs8 >= 10 & is.na(uv) ~ uvm_rep_hs8,
               n_rep_hs6 >= 10 & is.na(uv) ~ uvm_rep_hs6,
               # n_rep_cfc >= 10 & is.na(uv) ~ uvm_rep_cfc,
               TRUE                        ~ uv
             )
           ]
  
  
  .data2impute[is.na(weight) & !is.na(value) & !is.na(uvm), 
         `:=`(weight = value / uvm,
              imputation_method = "hierarchical_reporter_year")]
  .data2impute[, uv := value / weight]
  
  # imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "hierarchical_reporter_year", p_data = .data2impute)
  
  # Clean
  .data2impute[,
         `:=`(
           uvm_rep_hs  = NULL,
           # uvm_rep_hs8 = NULL,
           uvm_rep_hs6 = NULL,
           # uvm_rep_cfc = NULL,
           n_rep_hs    = NULL,
           # n_rep_hs8   = NULL,
           n_rep_hs6   = NULL,
           # n_rep_cfc   = NULL,
           uvm = NULL
         )
         ]
  ##-- Imputation by interpolation Unit Value Total ----
  progress_save_imputation$inc(.90, detail = " Imputation by interpolation Unit Value Total.")
  uv_tot <- .data2impute[!is.na(value) & !is.na(weight), 
                 list(uvt = sum(value) / sum(weight)), 
                 by = c("year", "rep", "flow", "comm")]
  uv_tot <-
    # Only for those that have at least one missing weight
    uv_tot[comm %in% unique(.data2impute[is.na(weight)]$comm)] %>%
    dplyr::group_by(rep, flow, comm) %>%
    tidyr::complete(year = min(.data2impute$year):max(.data2impute$year)) %>%
    setDT()
  
  uv_tot[, nonna := sum(!is.na(uvt)), list(rep, flow, comm)]
  uv_tot[, uv_int := ifelse(nonna > 1, zoo::na.fill(uvt, "extend"), na.omit(unique(uvt))[1]), list(rep, flow, comm)]
  uv_tot[is.na(uvt) & !is.na(uv_int), uvt := uv_int][, `:=`(uv_int = NULL, nonna = NULL)]
  
  .data2impute <- uv_tot[.data2impute, on = c("year", "rep", "flow", "comm")]
  
  .data2impute[is.na(weight) & !is.na(uvt) & !is.na(value), 
         `:=`(weight = value / uvt,
              imputation_method  = "uv_total_interpolate")]
  
  .data2impute[, uvt := NULL]
  
  .data2impute[, uv := value / weight]
  
  # imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "uv_total_interpolate", p_data = .data2impute)
  
  ##-- Hierarchical: Global Year Unit Value ----
  progress_save_imputation$inc(.98, detail = " Hierarchical: Global Year Unit Value.")
  
  # At this point the remaining NAs are infrequent combinations
  # of .reporter/flow/HS, so we use just the median for the specific
  # year, without any "robustness" check.
  
  if (nrow(.data2impute[is.na(weight)]) > 0) {
    .data2impute[, uv_hs6 := median(uv, na.rm = TRUE), list(year, flow, hs6)]
    .data2impute[is.na(weight) & !is.na(uv_hs6) & !is.na(value), 
           `:=`(weight = value / uv_hs6,
                imputation_method = "hierarchical_global_year")]
    # imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "hierarchical_global_year", p_data = .data2impute)
    .data2impute[, uv_hs6 := NULL]
  }
  
  
  .data2impute[, uv := value / weight]
  
  if (nrow(.data2impute[is.na(weight)]) > 0) {
    # XXX This are "difficult" cases, as, e.g., when a country
    # trades a commodity for just one year and the weight is missing: how to
    # impute the missing weight? For example .reporter = 566 hs6 = 030346
    # (import only in 2008).
    #
    # Possible ideas:
    #
    # 1) Maybe, we could use comtrade's "standard unit values":
    # https://unstats.un.org/unsd/tradekb/Knowledgebase/ImportExport-Unit-Value-Indices?Keywords=Standard+Unit+Values
    # or calculate them ourselves (as the comtrade ones could be mising
    # in most recent years).
    #
    # 2) have a look if you can work with the faocodes somehow, e.g.,
    # if "034.2.5.6.35" and "034.2.5.6.30" can be considered "similar"
    # products, you could take the median unit value from "034.2.5.6.30"
    # to impute "034.2.5.6.35" (or, in general, those starting with
    # "034.2.5.6.3" (notice the missing trailing number))
    #
    # 3) use the closest unit value that can be extracted from the uv_table_*
    # for instance, in uv_table_global_year[substr(hs, 1, 6) == '030346'] there
    # is no UV for 2008, but you can take the one available in 2010 and use it
    #
    # 4) similar to 3), but you explicitely extrapolate the unit values for
    # the missing years, like using zoo::na.fill(uv_global_year, "extend")
    # on the expanded `uv_table_global_year`:
    
    ##-- Global year Expanded - HS6 ----
    uv_table_global_year <- readRDS("data/helper_tables/uv_table_global_year.rds")
    data_cj <- CJ(year = uv_table_global_year$year, flow = uv_table_global_year$flow, hs6 = uv_table_global_year$hs6, unique = TRUE)
    uv_table_global_year_expanded <- uv_table_global_year[data_cj, on = c("year", "flow", "hs6")]
    uv_table_global_year_expanded[, avail := sum(!is.na(uvs_global_year)), list(flow, hs6)][, n := .N, avail]
    uv_table_global_year_expanded <- uv_table_global_year_expanded[avail > 2]
    uv_table_global_year_expanded[, n := NULL][, avail := NULL]
    uv_table_global_year_expanded[, uvs := zoo::na.fill(uvs_global_year, "extend")][, uvs_global_year := NULL]
    .data2impute <- uv_table_global_year_expanded[.data2impute, on = c('year', 'flow', 'hs6')]
    
    .data2impute[is.na(weight) & !is.na(uvs), 
           `:=`(weight = value / uvs,
                imputation_method = "global_year_expanded")]
    # imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "global_year_expanded", p_data = .data2impute)
    
    .data2impute[, uvs := NULL]
    
    ##-- Global year Expanded - HS6 ----
    # Last resort:
    # uv_table_global_year_expanded[, hs6 := substr(hs, 1, 6)][, hs := NULL]
    # uv_table_global_year_expanded[, uvs := median(uvs), by = list(hs6)]
    # .data2impute[, hs6 := substr(hs, 1, 6)]
    # .data2impute <- uv_table_global_year_expanded[.data2impute, on = c('year', 'flow', 'hs6')]
    # .data2impute[is.na(weight) & !is.na(uvs), weight := value / uvs]
    # 
    # control_list$weight_na_uvs_global_median <- sum(is.na(.data2impute$weight))
    # control_list$tldata_weight_na_uvs_global_median$dim <- dim(.data2impute)
    # control_list$tldata_weight_na_uvs_global_median$weight_summary <- .data2impute[, summary(weight)]
    # control_list$tldata_weight_na_uvs_global_median$value_summary <- .data2impute[, summary(value)]
    # 
    # control_list$weight_not_imputed <- nrow(.data2impute[is.na(weight)])
    
    
    
    # if (nrow(.data2impute[is.na(weight)]) > 0) {
    #   # We did our best, but there may be some other unmapped codes
    #   warning("Some missing weights in UNSD data could not be imputed.")
    #   # Here we can send an email to the user asking what to do with
    #   # these (few) codes ad, MAYBE, create n SWS datatable with the
    #   # decision taken.
    #   # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    #   # XXX for now, let's remove the remaining transactionsflows with
    #   # missing weight that we could not assign:
    #   imputation_control_list <- data_log(p_list = imputation_control_list, p_step = "not_imputed", p_data = .data2impute)
    #   .data2impute <- .data2impute[!is.na(weight)]
    #   # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    # }
    
  }
  
  .data2impute <-
    .data2impute[,
                 list(
                   value    = sum(value),
                   weight   = sum(weight),
                   qty      = sum(qty),
                   qunit    = unique(qunit),
                   hslength = unique(hslength),
                   percvanna = unique(percvanna),
                   imputation_method = toString(unique(imputation_method))
                 ),
                 list(year, rep, prt, flow, comm, na_value, na_weight)]
  
  .data2impute[, uv := value / weight]
  
  .data2impute[, `:=`(hs6 = NULL, 
                      na_value = NULL, na_weight = NULL)]
  
  .data2impute <- .data2impute[, list(year, rep, prt, flow, comm, 
                                      value, weight, uv, qty, qunit, hslength, percvanna, imputation_method)]
  
  ##-- No imputation ----
  .data2impute_not_impute[, `:=`(na_value = is.na(value), na_weight = is.na(weight))]
  
  .data2impute_not_impute <-
    .data2impute_not_impute[,
                 list(
                   value    = sum(value),
                   weight   = sum(weight),
                   qty      = sum(qty),
                   qunit    = unique(qunit),
                   hslength = unique(hslength)
                 ),
                 list(year, rep, prt, flow, comm, na_value, na_weight, qunit, hslength)]
  
  .data2impute_not_impute[, imputation_method := 'blocked_to_impute']
  .data2impute_not_impute[, uv := value / weight]
  
  .data2impute <- rbindlist(list(.data2impute_not_impute, .data2impute), fill = TRUE)
  # .data2impute[imputation_method != 'partially_reported_weight', percvanna := NA_real_]
  
  .data2impute <- .data2impute[, list(year, rep, prt, flow, comm, 
                                      value, weight, uv, qty, qunit, hslength, percvanna, imputation_method)]
  
  progress_save_imputation$inc(1, detail = " Finished.")
  
  # out <- list(.data2impute = .data2impute, imputation_control_list = imputation_control_list)
  out <- list(.data2impute = .data2impute)
  return(out)
  
}