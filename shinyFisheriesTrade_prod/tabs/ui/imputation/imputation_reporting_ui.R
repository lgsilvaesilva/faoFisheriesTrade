imputation_reporting_ui <- tabPanel(title = "DataViz", 
                                    icon = icon("chart-line"), 
                                    value = "imputation_reporting",
                                    ##-- Button ----
                                    column(width = 12,
                                           ##-- + Year ----
                                           column(width = 2,
                                                  pickerInput(inputId = "imputed_years_viz", 
                                                              multiple = FALSE,
                                                              label = "Year", 
                                                              selected = 2013,
                                                              choices = NULL, 
                                                              options = list(`actions-box` = TRUE)
                                                  )                                                  
                                           ),
                                           ##-- + Reporter ----
                                           column(width = 2,
                                                  pickerInput(inputId = "reporter_imputed_viz", 
                                                              label = "Reporter:", 
                                                              choices = NULL,
                                                              options = list(`live-search` = TRUE))                                                  
                                           ),
                                           ##-- + Flow ----
                                           column(width = 2,
                                                  pickerInput(inputId = "flow_imputed_viz", 
                                                              label = "Flow:", 
                                                              choices = NULL,
                                                              options = list(`live-search` = TRUE))                                                  
                                           ),
                                           column(12,
                                                  wellPanel(DT::dataTableOutput("tblCommodityImputed")),
                                                  br(),
                                                  column(6, 
                                                         conditionalPanel("(typeof input.tblCommodityImputed_cells_selected !== 'undefined' && input.tblCommodityImputed_cells_selected.length > 0)", 
                                                                          radioGroupButtons(
                                                                            inputId = "impute_target_variable",
                                                                            label = "Choose a variable:", 
                                                                            choices = c(`<i class='fa fa-weight-hanging'></i> Weight`  = "weight", 
                                                                                        `<i class='fa fa-dollar'></i> Value` = "value", 
                                                                                        `<i class='fa fa-barcode'></i> Unit Value`  = "unit_value"),
                                                                            justified = T
                                                                          )
                                                                          ,
                                                                          wellPanel(withSpinner(plotlyOutput("gg_ts_commodity"), type = 6))
                                                         )
                                                  ),
                                                  column(6, style = 'padding-top: 77px;',
                                                         conditionalPanel("(typeof input.tblCommodityImputed_cells_selected !== 'undefined' && input.tblCommodityImputed_cells_selected.length > 0)", 
                                                                          wellPanel(withSpinner(plotlyOutput("gg_bc_perc_nna_comm"), type = 6))
                                                         )
                                                  ),
                                                  br(),
                                                  column(4, 
                                                         conditionalPanel("(typeof input.tblCommodityImputed_cells_selected !== 'undefined' && input.tblCommodityImputed_cells_selected.length > 0)", 
                                                                          wellPanel(withSpinner(plotlyOutput("gg_histogram_parterns"), type = 6))
                                                         )
                                                  ),
                                                  column(8,
                                                         conditionalPanel("(typeof input.tblCommodityImputed_cells_selected !== 'undefined' && input.tblCommodityImputed_cells_selected.length > 0)", 
                                                                          wellPanel(
                                                                            ##-- + Save imputation ----
                                                                            actionButton(inputId = "save_data_imputed_prt",
                                                                                         label = "Save imputation",
                                                                                         style = "color: white; background-color: #09C184;border-color: transparent;",
                                                                                         icon("save"), 
                                                                                         width = "auto"),
                                                                            
                                                                            HTML("&nbsp"),
                                                                            ##-- + Export imputation ----
                                                                            actionButton(inputId = "export_data_imputation_prt",
                                                                                         label = "Update data",
                                                                                         style = "color: white; background-color: #09C184;border-color: transparent;",
                                                                                         icon("file-export"),
                                                                                         width = "auto"),
                                                                            br(), br(),
                                                                            withSpinner(rHandsontableOutput("tblPartners", height = '450px', width = '100%'), type = 6)
                                                                          )
                                                         )
                                                  )
                                           )       
                                    )
)


