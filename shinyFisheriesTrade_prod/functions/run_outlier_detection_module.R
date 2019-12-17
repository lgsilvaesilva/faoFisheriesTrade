##-- Functions ----
##** Box Plot function ----
od_box_plot <- function(.var, .box_coef = 1.5, iterate = 2) {
  
  limits <- quantile(.var, c(.25, .75), type = 1) + .box_coef * IQR(.var, type = 1) * c(-1, 1)
  med <- median(.var, na.rm = T)
  
  if (diff(limits) > 1e-14) {
    
    outliers <- which((.var < limits[1]) | (.var > limits[2]))
    
  } else {
    
    outliers <- numeric(0)
    
  }
  set.seed(12345)
  .var[outliers] <- ifelse(.var[outliers] < limits[1], 
                           runif(n = sum(.var[outliers] < limits[1]), limits[1], med), 
                           runif(n = sum(.var[outliers] >= limits[1]), med, limits[2]))
  
  if (iterate > 1) {
    
    tmp <- od_box_plot(.var = .var, .box_coef = .box_coef, iterate = 1)
    
    if (length(tmp$index) > 0) {
      outliers <- sort(c(outliers, tmp$index))
      .var[outliers] <- ifelse(.var[outliers] < limits[1], 
                               runif(n = sum(.var[outliers] < limits[1]), limits[1], med), 
                               runif(n = sum(.var[outliers] >= limits[1]), med, limits[2]))   
    }
    
  }
  
  isOUT <- 1:length(.var) %in% outliers
  replacements <- rep(NA, length(.var))
  replacements[outliers] <- .var[outliers]
  
  return(list(index = outliers, replacements = replacements, isOUT = isOUT))
  
}

##** TS outlier ----
od_ts_toolbox <- function(.yvar, .xvar, .level = 99) {
  
  # ## Average method
  # mean_forecast <- forecast::meanf(.yvar, h = 1, level = .level)
  # mean_forecast <- data.frame(method = mean_forecast$method,
  #                             forecast = mean_forecast$mean,
  #                             lower = mean_forecast$lower[1],
  #                             upper = mean_forecast$upper[1])
  # 
  # ## Naive
  # naive_forecast <- forecast::naive(.yvar, h = 1, level = .level)
  # naive_forecast <- data.frame(method = naive_forecast$method, 
  #                              forecast = naive_forecast$mean,
  #                              lower = naive_forecast$lower[1],
  #                              upper = naive_forecast$upper[1])
  # 
  # ## Drift method
  # # This is equivalent to drawing a line between the first and last observations, and extrapolating it into the future.
  # drift_forecast <- forecast::rwf(.yvar, h = 1, drift = TRUE, level = .level)
  # drift_forecast <- data.frame(method = drift_forecast$method, 
  #                              forecast = drift_forecast$mean,
  #                              lower = drift_forecast$lower[1],
  #                              upper = drift_forecast$upper[1])
  # 
  # methods_df <- rbind(mean_forecast, naive_forecast, drift_forecast)
  
  ## Outlier Detection
  outliers <- tsoutliers(x = .yvar, iterate = 5)
  index <- 1:length(.yvar) %in% outliers$index
  replacements <- rep(NA, length(.yvar))
  replacements[outliers$index] <- outliers$replacements
  
  out <- list(index = index, replacements = replacements)
  
  return(out)
  
}
##** Tidy outlier ----
tidy_outlier <- function(.data, .variable) {
  df <- copy(.data)
  
  df[, out_ts_correction  := out_ts_correction * ifelse(out_ts == F, NA, out_ts)]
  df[, out_box_correction := out_box_correction * ifelse(out_box == F, NA, out_box)]
  
  xx <- melt(df[, list(year, reporter, flow, level_aggregation, value = get(.variable), out_ts_correction, out_box_correction, 
                       fao_group, isscfc_code, hs6, hs4, hs2, tariff_line)], 1:7, 
             value.name = "code_aggregation")
  xx <- xx[!is.na(code_aggregation), ][, criteria := .variable]
  xx[, variable := NULL]
  setcolorder(xx, c("year", "reporter", "flow", "level_aggregation", "code_aggregation", "criteria",  "value", "out_ts_correction", "out_box_correction"))
  
  return(xx)
  
}
##** Detecting by group ----
detect_group <- function(.data, .by = c("reporter", "flow", "isscfc_code", "year"), .variable, .logarithm = F, .seq_len, .n_years, .box_coef) {
  
  data2detect <- copy(.data)
  
  data_agg <- data2detect[, list(value = sum(value, na.rm = T), weight = sum(weight, na.rm = T)), by = .by]
  data_agg <- data_agg[eval(parse(text = sprintf("order(%s)", toString(.by)))), ]
  
  data_agg[, `:=`(uv = value / weight,
                  n_years = uniqueN(year),
                  sequential = last(rle(diff(year))$lengths) + 1) , 
           by = eval(.by[-length(.by)])]
  
  data_agg[is.na(sequential), sequential := 1]
  
  ##*** Detection ----
  data_agg[, apply_ts := sequential >= .seq_len]
  data_agg[, apply_box := n_years >= .n_years]
  
  data_agg[, c("out_ts_correction", "out_box_correction") := NA_real_]
  data_agg[, c("out_ts", "out_box") := FALSE]
  
  ##*** Logarithm ----
  if(.logarithm == TRUE) {
    log_var <- sprintf("%s := log(%s)", .variable, .variable)
    
    data_agg[, eval(parse(text = log_var))]
    
  }
  
  data_agg[apply_ts == TRUE, 
           c("out_ts", "out_ts_correction") := od_ts_toolbox(.yvar = get(.variable), .xvar = year), 
           by = eval(.by[-length(.by)])]
  
  data_agg[apply_box == TRUE, 
           c("out_box", "out_box_correction") := od_box_plot(.var = get(.variable), .box_coef = .box_coef)[c(3, 2)], 
           by = eval(.by[-length(.by)])]
  
  
  if(.logarithm == TRUE) {
    exp_var <- sprintf("`:=`(%s = exp(%s), out_ts_correction = exp(out_ts_correction), out_box_correction = exp(out_box_correction))", .variable, .variable)
    
    data_agg[, eval(parse(text = exp_var))]
    
  }
  
  return(data_agg)
  
}
##-- Outlier Detection function ----
run_outlier_detection <- function(.data, 
                                  .variable,        # variable for applying the methods (UV, value, and weight)
                                  .levels = "all",  # levels for applying the methods
                                  .logarithm = FALSE,
                                  # .year = .data[, max(year)],
                                  # .reporter = .data[, unique(reporter)],
                                  .seq_len = 10,    # apply the TS methods only for those groups with a sequential TS greater than .seq_len
                                  .n_years = 5,     # apply the Box plot methods only for those groups with more than 5 available years
                                  .box_coef = 3) {  # this determines how far the plot ‘whiskers’ extend out from the box.
  
  data2detect <- copy(.data)
  
  ##-- Flow ----
  flow_od <- detect_group(.data = data2detect, 
                          .by = c("reporter", "flow", "year"), 
                          .variable = .variable, 
                          .logarithm = .logarithm,
                          .seq_len = .seq_len, 
                          .n_years = .n_years, 
                          .box_coef = .box_coef)
  
  ##-- ISSCFC ----
  isscfc_od <- detect_group(.data = data2detect, 
                            .by = c("reporter", "flow", "isscfc_code", "year"), 
                            .variable = .variable, 
                            .logarithm = .logarithm,
                            .seq_len = .seq_len, 
                            .n_years = .n_years, 
                            .box_coef = .box_coef)
  
  ##-- Partner ----
  prt_od <- detect_group(.data = data2detect, 
                         .by = c("reporter", "flow", "prt", "year"), 
                         .variable = .variable, 
                         .logarithm = .logarithm,
                         .seq_len = .seq_len, 
                         .n_years = .n_years, 
                         .box_coef = .box_coef)
  
  prt_od <- prt_od[!is.na(prt)]
  
  ##-- Tariff Line ----
  tl_od <- detect_group(.data = data2detect, 
                        .by = c("reporter", "flow", "tariff_line", "year"), 
                        .variable = .variable,
                        .logarithm = .logarithm,
                        .seq_len = .seq_len, 
                        .n_years = .n_years, 
                        .box_coef = .box_coef)
  
  ##-- HS6  ----
  hs6_od <- detect_group(.data = data2detect, 
                         .by = c("reporter", "flow", "hs6", "year"), 
                         .variable = .variable, 
                         .logarithm = .logarithm,
                         .seq_len = .seq_len, 
                         .n_years = .n_years, 
                         .box_coef = .box_coef)
  
  ##-- HS4  ----
  hs4_od <- detect_group(.data = data2detect, 
                         .by = c("reporter", "flow", "hs4", "year"), 
                         .variable = .variable,
                         .logarithm = .logarithm,
                         .seq_len = .seq_len, 
                         .n_years = .n_years, 
                         .box_coef = .box_coef)
  
  ##-- HS2  ----
  hs2_od <- detect_group(.data = data2detect, 
                         .by = c("reporter", "flow", "hs2", "year"), 
                         .variable = .variable, 
                         .logarithm = .logarithm,
                         .seq_len = .seq_len, 
                         .n_years = .n_years, 
                         .box_coef = .box_coef)
  
  ##-- FAO major groups  ----
  fao_od <- detect_group(.data = data2detect, 
                         .by = c("reporter", "flow", "fao_group", "year"), 
                         .variable = .variable, 
                         .logarithm = .logarithm,
                         .seq_len = .seq_len, 
                         .n_years = .n_years, 
                         .box_coef = .box_coef)
  ## Output ----
  out <- list(flow = flow_od, 
              fao_group = fao_od, 
              isscfc = isscfc_od, 
              tariff_line = tl_od, 
              hs6 = hs6_od, 
              hs4 = hs4_od, 
              hs2 = hs2_od)
  
  return(out)
  
}











































