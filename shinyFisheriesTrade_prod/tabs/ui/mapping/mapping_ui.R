mapping_ui <- tabPanel(title = "Mapping", 
                       value = "mapping",
                       ##-- Button ----
                       column(width = 12,
                              ##-- + Reporter ----
                              column(width = 2,
                                     pickerInput(inputId = "reporter_map", 
                                                 label = "Reporter:", 
                                                 choices = NULL,
                                                 options = list(`live-search` = TRUE))
                                     # uiOutput("btn_reporter_map")
                              ),
                              ##-- + Codes ----
                              column(width = 2,
                                     pickerInput(inputId = "codes_map", 
                                                 label = "Filters:", 
                                                 choices = c("All" = "all", 
                                                             "Unmapped" = "unmapped", 
                                                             # "Mapped" = "mapped", 
                                                             "To improve" = "to_improve",
                                                             "HS6 mapping" = "hs6_use"), 
                                                 selected = "unmapped")
                              ),
                              ##-- + Use ISSCFC ----
                              column(width = 6, style = 'padding-top: 55px;',
                                     actionButton(inputId = "discard_code",
                                                  label = "Discard",
                                                  style = "color: white; background-color: #035AA6;",
                                                  icon("trash"),
                                                  width = "auto"),
                                     # HTML("&nbsp"),
                                     # ##-- + Generate Scheda ----
                                     # actionButton(inputId = "gen_scheda",
                                     #              label = "Gen. scheda",
                                     #              style = "color: white; background-color: #035AA6;",
                                     #              icon("random"),
                                     #              width = "auto"),
                                     
                                     HTML("&nbsp"),
                                     ##-- + Add mapping ----
                                     actionButton(inputId = "add_mapping",
                                                  label = "Add mapping",
                                                  style = "color: white; background-color: #035AA6;",
                                                  icon("plus"),
                                                  width = "auto"),
                                     
                                     HTML("&nbsp"),
                                     ##-- + Discard ----
                                     actionButton(inputId = "use_isscfc",
                                                  label = "Use ISSCFC",
                                                  style = "color: white; background-color: #035AA6;",
                                                  icon("hand-pointer"),
                                                  width = "auto"),
                                     
                                     HTML("&nbsp"),
                                     ##-- + Save map ----
                                     actionButton(inputId = "save_map",
                                                  label = "Save map",
                                                  style = "color: white; background-color: #035AA6;",
                                                  icon("save"), 
                                                  width = "auto"),
                                     
                                     HTML("&nbsp"),
                                     ##-- + Export map ----
                                     actionButton(inputId = "export_map",
                                                  label = "Update map",
                                                  style = "color: white; background-color: #035AA6;",
                                                  icon("file-export"),
                                                  width = "auto"),
                                     
                                     HTML("&nbsp")
                                     
                              )
                              
                              
                       ),
                       
                       column(width = 12,
                              useShinyalert(),
                              br(), br(),
                              ##-- Trademap table ----
                              column(12, 
                                     wellPanel(rHandsontableOutput("trademap", height = '400px', width = '100%')),
                                     br()
                              ),
                              
                              column(width = 3, 
                                     plotOutput("gg_bar_missing_rows", height = "200px")
                                     ),
                              column(width = 3, 
                                     plotOutput("gg_bar_missing_commotidy", height = "200px")
                              ),
                              column(width = 3, 
                                     plotOutput("gg_bar_missing_rows_perc", height = "200px")
                              ),
                              column(width = 3, 
                                     plotOutput("gg_bar_missing_commodity_perc", height = "200px")
                              )
                              
                       )
)

