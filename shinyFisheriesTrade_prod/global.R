rm(list = ls())

##-- Packages ----
.libPaths( c(#"/srv/shiny-server/shinyFisheriesTrade_prod/lib",
             "/home/shiny/R/x86_64-pc-linux-gnu-library/3.2",
             "/usr/local/lib64/R-3.1.2/library",
             "/srv/shiny-server/FisheriesTrade/lib",
             "/work/SWS_R_Share/shiny/Rlib/3.1",
             .libPaths()))

# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

is_run_local <- F

suppressMessages({

  ##++ Data manipulation ----
  if(is_run_local) library(shiny, lib = 'lib') else library(shiny, lib.loc = "/srv/shiny-server/shinyFisheriesTrade_prod/lib")
  library(dplyr)
  library(data.table)
  
  ##++ Visualization ----
  if(is_run_local)   library(rhandsontable) else library(rhandsontable, lib.loc = "/srv/shiny-server/shinyFisheriesTrade/lib")

  library(shinyWidgets)
  library(shinyalert)
  library(ggplot2)
  library(DT)
  library(shinycssloaders)
  library(plotly)
  ##++ FAO packages ----
  # library(faoebx5)
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(faoswsTrade)
  ##++ Modeling ----
  library(forecast)
  ##--
})
xx <- sessionInfo()
message("shiny: ", xx$otherPkgs[['shiny']]$Version)
message("shinyWidgets: ", xx$otherPkgs[['shinyWidgets']]$Version)
message("rhandsontable: ", xx$otherPkgs[['rhandsontable']]$Version)

##-- Utils functions ----
invisible(sapply(list.files('functions/', full.names = T), source))
##--

##-- SWS Connection ----
if(is_run_local) {
  
  if(CheckDebug()){
    library(faoswsModules)
    SETTINGS <- ReadSettings('sws/sws.yml')
    Sys.getenv('R_SWS_SHARE_PATH', SETTINGS[["share"]])
    # R_SWS_SHARE_PATH = "Z:"
    SetClientFiles(SETTINGS[["certdir"]])
    GetTestEnvironment(baseUrl = SETTINGS[['server']],
                       token = SETTINGS[['token']])
  }
  
} else {
  
  if(CheckDebug()){
    library(faoswsModules)
    R_SWS_SHARE_PATH = "Z:"
    SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
    GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                       token = 'bf8860bd-88c1-4c0e-818a-26ac587c8d5c')
  }
  
}
##--

.local <- TRUE

##-- Datatables (SWS) ----
# it stores the data built through "Getting data" tab which will be the input data for next steps
fishtrade_built_data <- "fishtrade_built_data"

#'it stores the mappping from Tariff Line code to ISSCFC code
fishtrade_trade_map <- "fishtrade_trade_map"

#' it stores the hs code for non-fish
fishtrade_param_hs_non_fish <- "fishtrade_param_hs_non_fish"

#' commodities to be mapped
fishtrade_table_to_map <- "fishtrade_table_to_map"

#' it stores which stage of the country-year
fishtrade_reporter_workflow <- "fishtrade_reporter_workflow"

#' it stores the data imputed
fishtrade_data_imputed <- 'fishtrade_data_imputed'

#'
fishtrade_data_imputed_validated <- "fishtrade_data_imputed_validated"

fao_group_dt <- get_mapping_isscfc_fao_group(.local = .local)

##-- Load trade data mapped ----
get_tradedata_mapped <- function() {
  tradedata_mapped <- ReadDatatable(table = fishtrade_table_to_map)
  setnames(tradedata_mapped, "rep", "reporter")
  #' setting variable class
  tradedata_mapped[, `:=`(reporter = as.integer(reporter),
                          flow = as.integer(flow),
                          startyear = as.integer(startyear),
                          endyear = as.integer(endyear),
                          isscfc_code = as.character(ifelse(is.na(isscfc_code), "",
                                                            isscfc_code)),
                          id = 1:.N)]
  return(tradedata_mapped)
}

tradedata_mapped <- get_tradedata_mapped()

##-- Countries name and M49 codes ----
m49_codes <- GetCodeList(domain = "Fisheries", dataset = "commodities_total", dimension = "geographicAreaM49_fi")
m49_codes <- m49_codes[type == 'country', list(code, description)]
setnames(m49_codes, c("description", "code"), c("country", "reporter"))
m49_codes[, reporter := as.integer(reporter)]
m49_codes[, country := iconv(country, from = "UTF-8")]

##-- Labeling countries ----
# tradedata_mapped_descr <- left_join(tradedata_mapped, m49_codes, by = 'reporter')
# tradedata_mapped_descr <- setDT(tradedata_mapped_descr, key = c('reporter', 'tariff_line'))

##-- Reporters list ----
#' it is used as the list of countries with missing codes ----
reporters_df <- unique(tradedata_mapped[, list(reporter, country)])
reporters_list <- reporters_df$reporter
names(reporters_list) <- reporters_df$country

## Add the option to select all available countries.
if (length(reporters_list) > 1) {
  reporters_list <- c("all" = "all", reporters_list)
}

##-- Internal Code ----
hs_masterfile_full <- get_hs_masterfile()
inter_code_map <- hs_masterfile_full[ , .(internal_code = Member,
                                          hsrep,
                                          isscfc = ISSCFC,
                                          isscfc_label = NameEn)]

inter_code_map[, internal_code := stringr::str_pad(string = internal_code,
                                                   width = 4,
                                                   side = "left",
                                                   pad = 0)]

inter_code_map[, `:=`(isscfc = as.character(isscfc), isscfc_label = as.character(isscfc_label))]
inter_code_map <- inter_code_map[,
                                 list(isscfc_code = isscfc[max(hsrep) == hsrep],
                                      isscfc_descr = isscfc_label[max(hsrep) == hsrep]),
                                 by = list(isscfc_id = internal_code)]
setnames(inter_code_map, c("isscfc_code", "isscfc_descr"), c("isscfc_input", "isscfc_descr_input"))


##-- Raw data ----
chapters_condition <- paste0("chapter IN (", chapters_filter(), ")")
hs_filter  <- ReadDatatable('fishtrade_param_hs_filter')
hs_exclude <- ReadDatatable('fishtrade_param_hs_exclude')

##-- List of reporters imputed ----
reporter_imputed_list <- c()

##-- Years which there is at least a country with imputation done
year_reporter <- unique(ReadDatatable(table = "fishtrade_data_imputed", columns = c("year", "rep")))

##-- Data source list ----
data_source_list <- c("UNSD", "EUROSTAT", "TDM")

##-- Country list ----
country_full_list <- m49_codes$reporter
names(country_full_list) <- as.character(m49_codes$country)
country_full_list <- sort(country_full_list)

##-- Year list ----
year_full_list <- 2000:year(Sys.Date())

##-- Flow labels ----
flow_labels <- c('1' = 'Import', '2' = 'Export', '3' = 'Re-export')
flow_labels_code <- c('Import' = 1, 'Export' = 2, 'Re-export' = 3)

##-- Calling header shiny ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]
suppressMessages(lapply(tab_files, source))


##-- Load historical data ----
## Remover quando colocar no SWS
# data_hist <- readRDS('~/projects/Fisheries_proj/Trade2000_NationalDetail/Trade2000_NationalDetail.rds')
# data_hist[, flow := factor(x = flow, levels = c('I', 'E', 'R'), labels = as.character(1:3))]
# data_hist[, flow := as.numeric(as.character(flow))]
# ##-- Mathematical conversions ----
# ##** quantity
# data_hist[, `:=`(qty = ifelse(qunit == "MT", qty * 1000, qty),
#                  qunit = ifelse(qunit == "MT", "KG", qunit))]
#
# data_hist[, `:=`(qty = ifelse(qunit == "LB", qty * 0.453592, qty),
#                  qunit = ifelse(qunit == "LB", "KG", qunit))]
#
#
# data_hist[, `:=`(qty = ifelse(qunit == "QL", qty * 100, qty),
#                  qunit = ifelse(qunit == "QL", "KG", qunit))]
#
# ##** value usd
# data_hist[, value_usd := 1000 * value_usd]
#
# setnames(data_hist, c("remarks", "isscfc"), c("remark", "isscfc_code"))
# setkey(data_hist,  reporter, flow, year)

##-- Highlight columns and rows in the handsontable ----
render_highlight_text <- "
function (instance, td, row, col, prop, value, cellProperties) {
Handsontable.renderers.TextRenderer.apply(this, arguments);
if (instance.params) {

hcols1 = instance.params.col_highlight;
hcols1 = hcols1 instanceof Array ? hcols1 : [hcols1];
hrows1 = instance.params.row_highlight;
hrows1 = hrows1 instanceof Array ? hrows1 : [hrows1];

if (hcols1.includes(col)) td.style.background = 'red';
if (hrows1.includes(row)) td.style.background = '#FFDF00';

for (var i = 0; i < hrows1.length; i++) {
if (hrows1[i] == row && hcols1[i] == col) {
td.style.background = '#FFDF00';
}
}
}
return td;}"

render_highlight_bool <- "
function (instance, td, row, col, prop, value, cellProperties) {
Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
if (instance.params) {

hcols1 = instance.params.col_highlight;
hcols1 = hcols1 instanceof Array ? hcols1 : [hcols1];
hrows1 = instance.params.row_highlight;
hrows1 = hrows1 instanceof Array ? hrows1 : [hrows1];

if (hcols1.includes(col)) td.style.background = 'red';
if (hrows1.includes(row)) td.style.background = '#FFDF00';

for (var i = 0; i < hrows1.length; i++) {
if (hrows1[i] == row && hcols1[i] == col) {
td.style.background = '#FFDF00';
}
}
}
return td;}"

