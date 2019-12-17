tab_files <- list.files(path = "tabs/ui/mapping", full.names = T)
suppressMessages(lapply(tab_files, source))

mapping <- tabPanel(title = "Mapping", 
                    value = "mapping",
                    hr(),
                    tabsetPanel(id = "tab_mapping",
                      run_mapping_ui,
                      mapping_ui
                    )
)