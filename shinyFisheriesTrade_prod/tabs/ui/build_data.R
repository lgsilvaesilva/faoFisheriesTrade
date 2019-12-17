tab_files <- list.files(path = "tabs/ui/build_data", full.names = T)
suppressMessages(lapply(tab_files, source))

build_data <- tabPanel(title = "Getting Data", 
                     value = "build_data",
                     hr(),
                     tabsetPanel(
                       build_data_ui
                     )
)