run_mirroring_ui <- tabPanel(title = "Run mirroring", 
                             value = "run_mirroring",
                             column(width = 12,
                                    ##-- + Year to be mirrored ----
                                    column(width = 2,
                                           div(id = "placeholder_run_mirror"),
                                           pickerInput(inputId = "mirroring_years", 
                                                       multiple = FALSE,
                                                       label = "Year", 
                                                       choices = NULL, 
                                                       options = list(
                                                         `actions-box` = TRUE))
                                    ),
                                    ##-- + Check partners
                                    column(width = 2, style = 'padding-top: 55px;',
                                           ##-- + Run mapping ----
                                           actionButton(inputId = "check_partners",
                                                        label = "Check Non-reporter",
                                                        style = "color: white; background-color: #BF3945;",
                                                        icon("check"), 
                                                        width = "auto")
                                    ),
                                    ##-- + Reporter to be mirrored ----
                                    # column(width = 2,
                                    #        pickerInput(inputId = "non_reporter",
                                    #                    label = "Non-reporter:",
                                    #                    choices = NULL,
                                    #                    # multiple = TRUE,
                                    #                    options = list(`live-search` = TRUE))
                                    # ),
                                    # ##-- + Reporter to be mirrored ----
                                    # column(width = 2,
                                    #        pickerInput(inputId = "flow_to_mirror",
                                    #                    label = "Flow:",
                                    #                    choices = NULL,
                                    #                    options = list(`live-search` = TRUE))
                                    # ),
                                    conditionalPanel("input.check_partners != 0",
                                      ##-- + Reporter to be mirrored ----
                                      column(width = 2,
                                             pickerInput(inputId = "donors_not_mirror",
                                                         label = "Remove Donors:",
                                                         choices = NULL,
                                                         selected = NULL,
                                                         multiple = TRUE,
                                                         options = list(`live-search` = TRUE))
                                      ),
                                      ##-- + Run Mirroring
                                      column(width = 2, style = 'padding-top: 55px;',
                                             ##-- + Run mapping ----
                                             actionButton(inputId = "run_mirror",
                                                          label = "Run mirroring",
                                                          style = "color: white; background-color: #BF3945;",
                                                          icon("exchange-alt"), 
                                                          width = "auto")
                                      )  
                                    )
                                    
                             ),
                             column(width = 12,
                                    DT::dataTableOutput("tblMirrored")
                                    )
)
