data_viz_table_ui <- tabPanel(title = "Data Aggregation", 
                        value = "data_viz_table",
                        column(width = 12, 
                               ##-- + Years ----
                               column(width = 2,
                                      # uiOutput("btn_year_table")
                                      pickerInput(inputId = "btn_year_viz", 
                                                  label = "Year", 
                                                  choices = NULL)
                               ),
                               ##-- + Mirrored ----
                               column(width = 2,
                                      pickerInput(inputId = "is_mirrored_data_viz", 
                                                  multiple = F,
                                                  label = "Mirrored", 
                                                  selected = c("All" = "all"), 
                                                  choices = c("All" = "all", "Yes" = TRUE, "No" = FALSE), 
                                                  options = list(`actions-box` = TRUE))
                               ),
                               
                               ##-- + Countries ----
                               column(width = 2,
                                      # uiOutput("btn_country_table")
                                      pickerInput(inputId = "btn_reporter_viz", 
                                                  label = "Reporter", 
                                                  choices = NULL, 
                                                  options = list(`live-search` = TRUE))
                               ),
                               ##-- + Group ----
                               column(width = 2,
                                      pickerInput(inputId = "group_agg_data_viz", 
                                                  label = "Group by", 
                                                  choices = c("Reporter" = "Reporter", 
                                                              "Reporter Code" = "reporter", 
                                                              "Partner" = "Partner", 
                                                              "Partner Code" = "partner", 
                                                              "Flow" = "flow", 
                                                              "HS6" = "hs6", "HS4" = "hs4", "HS2" = "hs2", 
                                                              "FAO Group" = "fao_group",
                                                              "ISSCFC" = "isscfc_code", 
                                                              "Scheda" = "scheda", 
                                                              "Tariff Line" = "tariff_line"),
                                                  selected = "fao_group",
                                                  multiple = FALSE) 
                                      ),
                               br(),
                               column(width = 3, style = 'padding-top: 35px;',
                                      ##-- + Run show data ----
                                      actionButton(
                                        inputId = "run_summary_data_table",
                                        label = "Summary",
                                        # style = "bordered",
                                        style = "color: white; background-color: #BF3945;",
                                        icon = icon("cogs")
                                      )
                               )
                        ),
                        column(width = 12,
                               br(),
                               tags$h4(tags$b("Status:"), textOutput("textReporterStatus", inline = TRUE)),
                               br(),
                               wellPanel(
                                 tags$h4(HTML('<a data-toggle="collapse" href="#tblAggregationViz"
                                                                        aria-expanded="false" aria-controls="tblAggregationViz">
                                                                        <i class="fa fa-window-maximize" style="font-size: x-large; color: #BF3945;"></i>
                                                                        <i class="fa fa-window-minimize" style="font-size: x-large; color: #BF3945;"></i>
                                                                        </a>'), "Aggregation Table"),
                                 tags$div(id = 'tblAggregationViz',  class = "collapse",
                                          br(),
                                          withSpinner(DT::dataTableOutput("tblYearBook"), type = 6)
                                          
                                 ))
                               
                        )
)