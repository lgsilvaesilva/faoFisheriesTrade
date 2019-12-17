run_imputation_ui <- tabPanel(title = "Run imputation", 
                              icon = icon("play"), 
                           value = "tab_run_imputation",
                           column(width = 12,
                                  ##-- + Years ----
                                  column(width = 2,
                                         pickerInput(inputId = "impute_years", 
                                                     multiple = FALSE,
                                                     label = "Year", 
                                                     choices = NULL,
                                                     options = list(
                                                       `actions-box` = TRUE)
                                         )
                                         # uiOutput("btn_impute_year")
                                  ),
                                  ##-- + Reporter to be imputed ----
                                  column(width = 2,
                                         pickerInput(inputId = "reporter_to_impute", 
                                                     label = "Reporter:", 
                                                     choices = NULL,
                                                     options = list(`live-search` = TRUE))
                                         # uiOutput("btn_reporter_to_impute")
                                         
                                  ),
                                  column(width = 4, style = 'padding-top: 55px;',
                                    ##-- + Run imputation ----
                                    actionButton(inputId = "run_imputation",
                                                 label = "Run imputation",
                                                 style = "color: white; background-color: #BF3945;",
                                                 icon("play"), 
                                                 width = "auto"),
                                    
                                    HTML("&nbsp"),
                                    
                                    dropdown(
                                      tags$div(title = "Parameter used to figure out NA's masked by zero",
                                      sliderInput(inputId = "threshold_masked_zero", 
                                                  label = "NA's masked by zero:", 
                                                  min = 1, 
                                                  max = 10, 
                                                  value = 5, 
                                                  step = 1, 
                                                  post = "%")
                                      ),
                                      
                                      circle = FALSE, 
                                      status = "success", 
                                      size = "default",
                                      
                                      icon = icon("sliders-h"), 
                                      width = "300px", 
                                      tooltip = tooltipOptions(title = "Click to see parameters.")
                                    )
                                    
                                  )
                                  # ,
                                  # column(width = 2, style = 'padding-top: 55px;',
                                  #        ##-- + Run imputation ----
                                  #        dropdown(
                                  #          sliderInput(inputId = "threshold_masked_zero", 
                                  #                      label = "Choose a value:", 
                                  #                      min = 1, max = 10, value = 5, step = 1, post = "%"),
                                  #          
                                  #          circle = FALSE, status = "info",
                                  #          
                                  #          icon = icon("sliders-h"), width = "300px",
                                  #          
                                  #          tooltip = tooltipOptions(title = "Click to see filters !")
                                  #        )
                                  # )
                           )#,
                           # column(width = 12,
                           #        
                           #        # withSpinner(plotOutput("gg_imputation_methods"), type = 6)#,
                           #        # withSpinner(DT::dataTableOutput("tbl_imputed_data"), type = 6)
                           #        
                           # )
)
