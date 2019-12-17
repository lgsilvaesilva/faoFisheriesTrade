shinyServer(function(input, output, session) {
  ##-- Build data ----
    ##++ Data ----
    source("tabs/server/build_data/tab_server_build_data.R", local = TRUE)
  
  ##-- Mapping ----
    ##++ Mapping ----
    source("tabs/server/mapping/tab_server_mapping.R", local = TRUE)
    ##++ Run Mapping ----
    source("tabs/server/mapping/tab_server_run_mapping.R", local = TRUE)
  
  ##-- Raw data ----
    #++ Data ----
    source("tabs/server/raw_data/tab_server_raw_data.R", local = TRUE)
    source("tabs/server/raw_data/tab_server_data_viz_table.R", local = TRUE)
  
  ##-- Imputation ----
    ##++ Run imputation ----
    source("tabs/server/imputation/tab_server_run_imputation.R", local = TRUE)
    ##++ Imputation ----
    source("tabs/server/imputation/tab_server_imputation.R", local = TRUE)
    ##++ Imputation ----
    source("tabs/server/imputation/tab_server_visual_reporting.R", local = TRUE)
  
  ##-- Outlier Detection ----
    ##++ Run Detection ----
    source("tabs/server/outlier_detection/tab_server_run_outlier_detection.R", local = TRUE)
    ##++ Outlier Detection ----
    source("tabs/server/outlier_detection/tab_server_outlier_detection.R", local = TRUE)
  
  ##-- Mirroring ----
    ##++ Run Mirroring ----
    source("tabs/server/mirroring/tab_server_run_mirroring.R", local = TRUE)
    source("tabs/server/mirroring/tab_server_mirroring_visualization.R", local = TRUE)
  
})