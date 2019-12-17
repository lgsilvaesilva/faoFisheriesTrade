tab_files <- list.files(path = "tabs/ui/mirroring", full.names = T)
suppressMessages(lapply(tab_files, source))

mirroring <- tabPanel(title = "Mirroring", 
                    value = "tab_mirroring",
                    hr(),
                    tabsetPanel(id = "mirroring",
                                run_mirroring_ui,
                                mirror_visualization_ui
                    )
)