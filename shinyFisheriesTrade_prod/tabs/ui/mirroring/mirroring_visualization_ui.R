mirror_visualization_ui <- tabPanel(title = "Mirroring visualization", 
                                    icon = icon("chart-line"), 
                                    value = "mirrorin_visualization",
                                    column(width = 12, 
                                           div(id = "placeholder_mirroring"),
                                           ##-- Year ----
                                           column(width = 2,
                                                  pickerInput(inputId = "year_mirrored", 
                                                              multiple = FALSE,
                                                              label = "Year", 
                                                              selected = NULL, 
                                                              choices = NULL, 
                                                              options = list(`actions-box` = TRUE))
                                           ),
                                           
                                           ##-- Mirrored ----
                                           column(width = 2,
                                                  pickerInput(inputId = "is_mirrored", 
                                                              multiple = F,
                                                              label = "Mirrored", 
                                                              selected = c("All" = "all"), 
                                                              choices = c("All" = "all", "Yes" = TRUE, "No" = FALSE), 
                                                              options = list(`actions-box` = TRUE))
                                           ),
                                           
                                           ##-- Reporter ----
                                           column(width = 2,
                                                  pickerInput(inputId = "reporter_mirrored", 
                                                              multiple = FALSE,
                                                              label = "Reporter", 
                                                              selected = NULL, 
                                                              choices = NULL, 
                                                              options = list(`actions-box` = TRUE, `live-search` = TRUE))
                                           ),
                                           
                                           ##-- Show ----
                                           column(width = 2, style = 'padding-top: 55px;',
                                                  actionButton(inputId = "show_data",
                                                               label = "Get Data",
                                                               style = "color: white; background-color: #BF3945;",
                                                               icon("bolt"), 
                                                               width = "auto")
                                           )
                                    ),
                                    ##-- Commodity source ----
                                    column(width = 12,
                                           br(), 
                                           br(), 
                                           br(), 
                                           column(width = 2,
                                                  conditionalPanel("input.show_data != 0",
                                                                   wellPanel(
                                                                     pickerInput(inputId = "commodity_source", 
                                                                                 multiple = FALSE,
                                                                                 label = "Commodity type", 
                                                                                 selected = "ISSCFC", 
                                                                                 choices = c("ISSCFC" = "isscfc_code", "Tariff Line" = "tariff_line", "Scheda" = "scheda"), 
                                                                                 options = list(`actions-box` = TRUE)),
                                                                     
                                                                     
                                                                     ##-- Flow ----
                                                                     uiOutput("flow_ui"),
                                                                     
                                                                     ##-- ISSCFC ----
                                                                     uiOutput("commodity_ui")
                                                                     
                                                                   )  
                                                  )
                                                  
                                           ),
                                           column(width = 10,
                                                  conditionalPanel("input.show_data != 0",
                                                                   radioGroupButtons(
                                                                     inputId = "criteria_viz_mirror", 
                                                                     selected = c(`<i class='fa fa-barcode'></i> Unit Value`  = "uv"),
                                                                     # label = "Choose a criteria:", 
                                                                     choices = c(`<i class='fa fa-weight-hanging'></i> Weight`  = "weight", 
                                                                                 `<i class='fa fa-dollar'></i> Value` = "value", 
                                                                                 `<i class='fa fa-barcode'></i> Unit Value`  = "uv"),
                                                                     justified = T),
                                                                   plotlyOutput("gg_ts_mirrored")
                                                  )      
                                           )
                                    ),
                                    
                                    column(width = 12,
                                           wellPanel(
                                             tags$h4(HTML('<a data-toggle="collapse" href="#dataMirroredExpand"
                                                                               aria-expanded="false" aria-controls="dataMirroredExpand">
                                                                               <i class="fa fa-window-maximize" style="font-size: x-large; color: #BF3945;"></i>
                                                                               <i class="fa fa-window-minimize" style="font-size: x-large; color: #BF3945;"></i>
                                                                               </a>'), "Data"),
                                             tags$div(id = 'dataMirroredExpand',  class = "collapse",
                                                      actionButton(inputId = "save_mirroring_correction",
                                                                   label = "Save",
                                                                   # style = "color: white; background-color: #035AA6; float: right;",
                                                                   style = "color: white; background-color: #035AA6;",
                                                                   icon("save"), 
                                                                   width = "auto"),
                                                      br(),
                                                      br(),
                                                      wellPanel(rHandsontableOutput("tblMirror", height = '300px', width = '100%'))
                                             )
                                           )
                                    )
                                    
                                    
)
