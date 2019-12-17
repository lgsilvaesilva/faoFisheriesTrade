run_mapping_ui <- tabPanel(title = "Run mapping", 
                           value = "run_mapping",
                           column(width = 12,
                                  ##-- + Year to be mapped ----
                                  column(width = 2,
                                         pickerInput(inputId = "mapping_years", 
                                                     multiple = FALSE,
                                                     label = "Year", 
                                                     choices = NULL, 
                                                     options = list(
                                                       `actions-box` = TRUE)
                                         )
                                         # uiOutput("btn_mapping_year")
                                         
                                  ),
                                  ##-- + Reporter to be mapped ----
                                  column(width = 2,
                                           pickerInput(inputId = "reporter_to_map",
                                                       label = "Reporter:",
                                                       choices = NULL,
                                                       options = list(`live-search` = TRUE))
                                         # uiOutput("btn_reporter_to_map")
                                         
                                  ),
                                  ##-- + Is an European country? ----
                                  column(width = 2, style = 'padding-top: 60px;',
                                         
                                         materialSwitch(
                                           inputId = "is_european",
                                           label = strong("European?"),
                                           value = FALSE, 
                                           status = "danger"
                                         )
                                         
                                  ),
                                  column(width = 2, style = 'padding-top: 55px;',
                                    ##-- + Run mapping ----
                                    actionButton(inputId = "run_map",
                                                 label = "Run mapping",
                                                 style = "color: white; background-color: #BF3945;",
                                                 icon("bolt"), 
                                                 width = "auto")
                                  )
                           )
)
