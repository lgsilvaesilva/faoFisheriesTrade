run_outlier_detection_ui <- tabPanel(title = "Run outlier detection", 
                              icon = icon("play"), 
                              value = "run_outlier_detection",
                              column(width = 12,
                                     ##-- + Years ----
                                     column(width = 2,
                                            pickerInput(inputId = "od_years", 
                                                        multiple = FALSE,
                                                        label = "Year", 
                                                        choices = NULL,
                                                        options = list(
                                                          `actions-box` = TRUE)
                                            )
                                     ),
                                     ##-- + Reporter to be imputed ----
                                     column(width = 2,
                                            pickerInput(inputId = "reporter_to_detect", 
                                                        label = "Reporter:", 
                                                        choices = NULL,
                                                        options = list(`live-search` = TRUE))                                            
                                     ),
                                     ##-- + Reporter to be imputed ----
                                     column(width = 2,
                                            pickerInput(inputId = "source_to_detect", 
                                                        label = "Source", 
                                                        choices = c("Imputation" = "imputation", "Corrected" = "corrected"), 
                                                        selected = "imputation"
                                            )
                                            
                                     ),
                                     
                                     column(width = 4, style = 'padding-top: 55px;',
                                            ##-- + Run imputation ----
                                            actionButton(inputId = "run_detection",
                                                         label = "Run detection",
                                                         style = "color: white; background-color: #BF3945;",
                                                         icon("play"), 
                                                         width = "auto"),
                                            
                                            HTML("&nbsp"),
                                            
                                            dropdown(
                                              tags$div(title = "Parameter used to detect outliers.",
                                                       materialSwitch(
                                                         inputId = "par_logarithm",
                                                         label = tags$label("Logarithm"), 
                                                         value = FALSE,
                                                         status = "warning"
                                                       ),
                                                       sliderInput(inputId = "par_coef_box",
                                                                   label = "Boxplot coeficient",
                                                                   min = 1,
                                                                   max = 15,
                                                                   value = 5,
                                                                   step = 1),
                                                       sliderInput(inputId = "par_n_sequential",
                                                                   label = "Sequential length",
                                                                   min = 5,
                                                                   max = 15,
                                                                   value = 10,
                                                                   step = 1),
                                                       sliderInput(inputId = "par_n_values",
                                                                   label = "Minimum sample",
                                                                   min = 3,
                                                                   max = 15,
                                                                   value = 5,
                                                                   step = 1)
                                              ),

                                              circle = FALSE,
                                              status = "info",
                                              size = "default",

                                              icon = icon("sliders-h"),
                                              width = "300px",
                                              tooltip = tooltipOptions(title = "Click to see parameters.")
                                            )
                                            
                                     )
                              )
)
