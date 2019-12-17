tab_files <- list.files(path = "tabs/ui/raw_data", full.names = T)
suppressMessages(lapply(tab_files, source))

raw_data <- tabPanel(title = "Data", 
                    value = "data_viz",
                    hr(),
                    tabsetPanel(id = "tab_data_viz",
                      data_viz_table_ui
                      # raw_data_ui
                    )
)