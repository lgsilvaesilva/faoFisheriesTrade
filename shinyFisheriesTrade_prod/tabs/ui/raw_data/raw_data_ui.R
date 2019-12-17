raw_data_ui <- tabPanel(title = "Data", 
                        value = "raw_data_table",
                        column(width = 12, 
                               ##-- + Source ----
                               column(width = 2,
                                      prettyRadioButtons(
                                        inputId = "rd_source",
                                        label = "Source:", 
                                        choices = c("Tariff Line" = "ct_tariffline_unlogged_", 
                                                    "Eurostat" = "ce_combinednomenclature_unlogged_"),
                                        inline = TRUE, 
                                        status = "primary",
                                        fill = TRUE)
                               ),
                               ##-- + Years ----
                               column(width = 2,
                                      uiOutput("btn_trade_year")
                               ),
                               ##-- + Countries ----
                               column(width = 2,
                                      uiOutput("btn_trade_country")
                               ),
                               br(),
                               column(width = 3, style = 'padding-top: 35px;',
                                      ##-- + Run show data ----
                                      actionButton(
                                        inputId = "btn_load_data",
                                        label = "Show data",
                                        style = "bordered",
                                        color = "primary",
                                        icon = icon("searchengin")
                                      ),
                                      
                                      HTML("&nbsp"),
                                      
                                      ##-- + Download raw data ----
                                      downloadButton(outputId = "download_raw_data",
                                        # inputId = "btn_download_raw_data",
                                        label = "Download"#,
                                        # style = "bordered",
                                        # color = "primary",
                                        # icon = icon("download")
                                      )
                                      
                               )
                        ),
                        column(width = 12,
                               withSpinner(DT::dataTableOutput("tbl_raw_data"), type = 6)
                               )
)