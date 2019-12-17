isEuropean <- function(.reporter, .year) {
  
  countries_eu <- ReadDatatable('country_european_union') ## table with the European countries until 2019
  geonomcode <- m49_to_geonom(.reporter)
  
  reporter_dt <- data.table(rep = .reporter, geonomcode,year = .year)
  reporter_dt <- countries_eu[reporter_dt, on = 'rep']
  reporter_dt[, isEuropean := year >= startyear & year <= endyear]
  reporter_dt[, isEuropean := ifelse(is.na(isEuropean), FALSE, isEuropean)]
  
  return(reporter_dt)
  
}