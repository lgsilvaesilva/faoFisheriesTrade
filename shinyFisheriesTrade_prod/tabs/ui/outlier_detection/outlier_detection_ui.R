outlier_detection_ui <- tabPanel(title = "Outlier detection", 
                                 icon = icon("search"), 
                                 value = "outlier_detection_tab",
                                 column(width = 12,
                                        
                                        ##-- Year ----
                                        column(width = 2,
                                               div(id = 'placeholderSweetAlert'),
                                               pickerInput(inputId = "detected_years",
                                                           multiple = FALSE,
                                                           label = "Year",
                                                           selected = NULL,
                                                           choices = NULL,
                                                           options = list(`actions-box` = TRUE)
                                               )
                                               # uiOutput("btn_year_detected")
                                               
                                        ),
                                        ##-- Reporter ----
                                        column(width = 2,
                                               pickerInput(inputId = "reporter_detected",
                                                           label = "Reporter:",
                                                           selected = NULL,
                                                           choices = NULL,
                                                           options = list(`live-search` = TRUE))
                                               # uiOutput("btn_reporter_detected")
                                               
                                        ),
                                        ##-- Outlier checked ----
                                        column(width = 3, style = 'padding-top: 55px;',
                                               ##-- Outlier validated ----
                                               actionButton(inputId = "validated_outlier",
                                                            label = "Checked",
                                                            style = "color: #777777; background-color: #C9E879;",
                                                            icon("check-double"),
                                                            width = "auto"),
                                               HTML("&nbsp"),
                                               ##-- Update correction ----
                                               actionButton(inputId = "export_outlier_correction",
                                                            label = "Update data",
                                                            # style = "color: white; background-color: #035AA6; float: right;",
                                                            style = "color: white; background-color: #BF3945;",                                                     icon("file-export"),
                                                            width = "auto")
                                        )
                                 ),
                                 column(width = 12,
                                        withSpinner(DT::dataTableOutput("tbl_outliers_detected"), type = 6)
                                 ),
                                 conditionalPanel("(typeof input.tbl_outliers_detected_cells_selected !== 'undefined' && input.tbl_outliers_detected_cells_selected.length > 0)",
                                                  ##** Aggregation Level ----
                                                  column(width = 12,
                                                         column(width = 2,
                                                                pickerInput(inputId = "aggregation_level_viz", 
                                                                            label = "Aggregation Level", 
                                                                            choices = c("ISSCFC" = "isscfc_code",
                                                                                        "Tariff Line" = "tariff_line",
                                                                                        "Partner" = "Partner",
                                                                                        "HS6" = "hs6",
                                                                                        "HS4" = "hs4",
                                                                                        "HS2" = "hs2",
                                                                                        "FAO Group" = "fao_group"))
                                                         ),
                                                         ##** Criteria ----
                                                         column(width = 4, style = 'padding-top: 27px;',
                                                                radioGroupButtons(
                                                                  inputId = "criteria_viz",
                                                                  label = "Choose a criteria:", 
                                                                  choices = c(`<i class='fa fa-weight-hanging'></i> Weight`  = "weight", 
                                                                              `<i class='fa fa-dollar'></i> Value` = "value", 
                                                                              `<i class='fa fa-barcode'></i> Unit Value`  = "uv"),
                                                                  justified = T)
                                                         )
                                                  ),
                                                  column(width = 12,
                                                         ##** Chart: time series ----
                                                         column(width = 6,
                                                                wellPanel(
                                                                  tags$h4(HTML('<a data-toggle="collapse" href="#TSOutlierExpand"
                                                                               aria-expanded="false" aria-controls="TSOutlierExpand">
                                                                               <i class="fa fa-window-maximize" style="font-size: x-large; color: #BF3945;"></i>
                                                                               <i class="fa fa-window-minimize" style="font-size: x-large; color: #BF3945;"></i>
                                                                               </a>'), "Time Series"),
                                                                  tags$div(id = 'TSOutlierExpand',  class = "collapse",
                                                                           br(),
                                                                           withSpinner(plotlyOutput("gg_ts_outlier"), type = 6)
                                                                  )
                                                                )
                                                         ),
                                                         ##** Chart: partner correction ----
                                                         column(width = 6,
                                                                wellPanel(
                                                                  tags$h4(HTML('<a data-toggle="collapse" href="#prtCorrectionExpand"
                                                                               aria-expanded="false" aria-controls="prtCorrectionExpand">
                                                                               <i class="fa fa-window-maximize" style="font-size: x-large; color: #BF3945;"></i>
                                                                               <i class="fa fa-window-minimize" style="font-size: x-large; color: #BF3945;"></i>
                                                                               </a>'), "Partner Correction"),
                                                                  tags$div(id = 'prtCorrectionExpand',  class = "collapse",
                                                                           br(),
                                                                           withSpinner(plotlyOutput("gg_prt_correction"), type = 6)
                                                                  )
                                                                )
                                                         )
                                                  ),
                                                  column(width = 12,
                                                         ##** Table: at partner level ----
                                                         wellPanel(
                                                           tags$h4(HTML('<a data-toggle="collapse" href="#tblPartnerLevelExpand"
                                                                        aria-expanded="true" aria-controls="tblPartnerLevelExpand">
                                                                        <i class="fa fa-window-maximize" style="font-size: x-large; color: #BF3945;"></i>
                                                                        <i class="fa fa-window-minimize" style="font-size: x-large; color: #BF3945;"></i>
                                                                        </a>'), "Data at the partner level"),
                                                           tags$div(id = 'tblPartnerLevelExpand',  class = "collapse in",
                                                                    
                                                                    ##-- Save correction ----
                                                                    actionBttn(inputId = "save_outlier_correction",
                                                                               label = "Save",
                                                                               style = "bordered",
                                                                               # style = "color: white; background-color: #035AA6;",
                                                                               icon("save"), 
                                                                               color = "success",
                                                                               # width = "auto"
                                                                    ),
                                                                    br(),
                                                                    br(),
                                                                    withSpinner(rHandsontableOutput("tblPartnerOutlier", height = '450px', width = '100%'), type = 6),
                                                                    tags$style(type="text/css", "div#tblPartnerOutlier th {font-weight:bold;}")
                                                           )
                                                         )
                                                         
                                                  ),
                                                  ##-- Aggregation Table ----
                                                  column(width = 12,
                                                         column(6, 
                                                                ##** Group By ----
                                                                column(width = 3,
                                                                       pickerInput(inputId = "group_agg", 
                                                                                   label = "Group by", 
                                                                                   choices = c("Year"= "year", "Partner" = "Partner", "HS6" = "hs6", 
                                                                                               "HS4" = "hs4",  "HS2" = "hs2", "Partner Code" = "partner", 
                                                                                               "FAO Group" = "fao_group",
                                                                                               "ISSCFC" = "isscfc_code", "Reporter" = "reporter", 
                                                                                               "Flow" = "flow", "Scheda" = "scheda", "Tariff Line" = "tariff_line"),
                                                                                   multiple = TRUE)
                                                                ),
                                                                ##** Value to be aggregated ----
                                                                column(width = 3,
                                                                       pickerInput(inputId = "value_agg", 
                                                                                   label = "Value", 
                                                                                   choices = c("Weight" = "weight", 
                                                                                               "Value" = "value", 
                                                                                               "Value Proposal" = "value_correction", 
                                                                                               "Weight Proposal" = "weight_correction"),
                                                                                   multiple = TRUE)
                                                                ),
                                                                ##** Function to be applied ----
                                                                column(width = 3,
                                                                       pickerInput(inputId = "function_agg", 
                                                                                   label = "Function", 
                                                                                   choices =  c("Sum" = "sum", 
                                                                                                "Average" = "mean", 
                                                                                                "Median" = "median", 
                                                                                                "Count" = ".N",
                                                                                                "Stand. Deviation" = "sd", 
                                                                                                "Variance" = "var", 
                                                                                                "Unit Value" = "uv",
                                                                                                "Minima" = "min", 
                                                                                                "Maxima" = "max"),
                                                                                   multiple = TRUE)
                                                                       
                                                                ),
                                                                ##** Run Aggregation ----
                                                                column(width = 3, style = 'padding-top: 25px;',
                                                                       actionButton(inputId = "run_outlier_aggregation",
                                                                                    label = "Summarize",
                                                                                    style = "color: white; background-color: #BF3945;",
                                                                                    icon("play"), 
                                                                                    width = "auto")
                                                                ),
                                                                ##** Table: aggregation table ----
                                                                column(width = 12,
                                                                       wellPanel(
                                                                         tags$h4(HTML('<a data-toggle="collapse" href="#tblAggregationExpand"
                                                                        aria-expanded="false" aria-controls="tblAggregationExpand">
                                                                        <i class="fa fa-window-maximize" style="font-size: x-large; color: #BF3945;"></i>
                                                                        <i class="fa fa-window-minimize" style="font-size: x-large; color: #BF3945;"></i>
                                                                        </a>'), "Aggregation Table"),
                                                                         tags$div(id = 'tblAggregationExpand',  class = "collapse",
                                                                                  br(),
                                                                                  withSpinner(rHandsontableOutput("tblAggregation", height = '300px', width = '100%'), type = 6)
                                                                                  
                                                                         ))
                                                                )
                                                         )
                                                         
                                                  )
                                                  
                                 )
                                 
)































