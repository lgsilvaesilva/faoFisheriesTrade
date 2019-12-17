tab_files <- list.files(path = "tabs/ui/outlier_detection", full.names = T)
suppressMessages(lapply(tab_files, source))

outlier_detection <- tabPanel(title = "Outliers", 
                       value = "outlier_detection",
                       hr(),
                       tabsetPanel(id = "tab_outlier",
                         run_outlier_detection_ui,
                         outlier_detection_ui
                       )
)