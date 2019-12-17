tab_files <- list.files(path = "tabs/ui/imputation", full.names = T)
suppressMessages(lapply(tab_files, source))

imputation <- tabPanel(title = "Imputation", 
                       value = "imputation",
                       hr(),
                       tabsetPanel(id = "tab_imputation",
                         run_imputation_ui,
                         imputation_ui,
                         imputation_reporting_ui
                       )
)