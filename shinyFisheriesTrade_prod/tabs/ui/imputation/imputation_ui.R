imputation_ui <- tabPanel(title = "Imputation", 
                          icon = icon("table"), 
                          value = "imputation_tab",
                          ##-- Button ----
                          column(width = 12,
                                 ##-- + Year ----
                                 column(width = 2,
                                        pickerInput(inputId = "imputed_years", 
                                                    multiple = FALSE,
                                                    label = "Year", 
                                                    choices = NULL,
                                                    options = list(`actions-box` = TRUE)
                                        )                                        
                                 ),
                                 ##-- + Reporter ----
                                 column(width = 2,
                                        pickerInput(inputId = "reporter_imputed", 
                                                    label = "Reporter:", 
                                                    choices = NULL,
                                                    options = list(`live-search` = TRUE))
                                        
                                 ),
                                 ##-- + Save data ----
                                 column(width = 4, style = 'padding-top: 55px;',
                                        ##-- + Save imputation ----
                                        actionButton(inputId = "save_data_imputed",
                                                     label = "Save imputation",
                                                     style = "color: white; background-color: #035AA6;",
                                                     icon("save"), 
                                                     width = "auto"),
                                        
                                        HTML("&nbsp"),
                                        ##-- + Export imputation ----
                                        actionButton(inputId = "export_data_imputation",
                                                     label = "Update data",
                                                     style = "color: white; background-color: #035AA6;",
                                                     icon("file-export"),
                                                     width = "auto"),
                                        
                                        HTML("&nbsp"),
                                        ##-- + Codes ----
                                        dropdown(
                                          
                                          ##-- Filter imputed values
                                          pickerInput(inputId = "filter_imputed", 
                                                      multiple = TRUE,
                                                      label = "Methods:",
                                                      choices = NULL,
                                                      options = list(
                                                        `actions-box` = TRUE)),
                                          
                                          circle = FALSE, status = "danger",
                                          
                                          icon = icon("filter"), width = "300px",
                                          
                                          tooltip = tooltipOptions(title = "Click to see filters !")
                                        )
                                 )
                          ),
                          
                          column(width = 12,
                                 useShinyalert(),
                                 br(), br(),
                                 ##-- Trademap table ----
                                 column(12,
                                        wellPanel(withSpinner(rHandsontableOutput("imputed_data", height = '500px', width = '100%'), type = 6)),
                                        br(),
                                        wellPanel(plotlyOutput("donut_imports_general_tl")),
                                        br(),
                                column(6,
                                       wellPanel(
                                         tags$div(
                                           tags$span(style="color:gray", tags$h3("Imports"))
                                         ),
                                         plotlyOutput("bar_chart_methods_imports")
                                         )
                                       ),
                                column(6,
                                       wellPanel(
                                         tags$div(
                                           tags$span(style="color:gray", tags$h3("Exports"))
                                         ),
                                         plotlyOutput("bar_chart_methods_exports")
                                         )
                                )
                                        
                                 )
                                 
                          )
)

