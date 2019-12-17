build_data_ui <- tabPanel(title = "Data builder", 
                          value = "build_data_table",
                          column(width = 12, 
                                 ##-- + Countries ----
                                 column(width = 2,
                                        pickerInput(inputId = "bd_reporter", 
                                                    multiple = FALSE,
                                                    label = "Country", 
                                                    choices = country_full_list,
                                                    options = list(
                                                      `live-search` = TRUE,
                                                      `actions-box` = TRUE)
                                        )
                                 ),
                                 ##-- + Years ----
                                 column(width = 2,
                                        
                                        airYearpickerInput(inputId = "bd_year", 
                                                           label = "Year", 
                                                           placeholder = "Select a year", 
                                                           multiple = FALSE
                                        )
                                        
                                        # pickerInput(inputId = "bd_year", 
                                        #             multiple = FALSE,
                                        #             label = "Year", 
                                        #             choices = year_full_list,
                                        #             options = list(
                                        #               `live-search` = TRUE,
                                        #               `actions-box` = TRUE)
                                        # )
                                 ),
                                 ##-- + Flow: imports ----
                                 column(width = 2,
                                        pickerInput(inputId = "bd_import_source", 
                                                    multiple = FALSE,
                                                    label = "Import", 
                                                    choices = data_source_list,
                                                    options = list(
                                                      `live-search` = TRUE,
                                                      `actions-box` = TRUE)
                                        )
                                 ),
                                 ##-- + Flow: exports ----
                                 column(width = 2,
                                        pickerInput(inputId = "bd_export_source", 
                                                    multiple = FALSE,
                                                    label = "Export", 
                                                    choices = data_source_list,
                                                    options = list(
                                                      `live-search` = TRUE,
                                                      `actions-box` = TRUE)
                                        )
                                 ),
                                 br(),
                                 column(width = 4, style = 'padding-top: 35px;',
                                        ##-- + Run building data ----
                                        actionButton(
                                          inputId = "btn_build_data",
                                          label = "Build data",
                                          style = "bordered",
                                          color = "#27ae60",
                                          icon = icon("cubes")
                                        ),
                                        
                                        HTML("&nbsp"),
                                        
                                        ##-- + Load built data ----
                                        actionButton(
                                          inputId = "btn_load_built_data",
                                          label = "Load data",
                                          style = "bordered",
                                          color = "primary",
                                          icon = icon("database")
                                        ),
                                        HTML("&nbsp"),
                                        
                                        ##-- + Download built data ----
                                        downloadButton(outputId = "download_built_data",
                                                       # inputId = "btn_download_raw_data",
                                                       label = "Download"#,
                                                       # style = "bordered",
                                                       # color = "primary",
                                                       # icon = icon("download")
                                        ),
                                        
                                        HTML("&nbsp"),
                                        
                                        ##-- + Save built data ----
                                        actionButton(
                                          inputId = "btn_save_built_data",
                                          label = "Save",
                                          style = "bordered",
                                          color = "primary",
                                          icon = icon("save")
                                        )
                                        
                                 )
                          ),
                          column(width = 12,
                                 withSpinner(DT::dataTableOutput("tbl_built_data"), type = 6)
                          )
)