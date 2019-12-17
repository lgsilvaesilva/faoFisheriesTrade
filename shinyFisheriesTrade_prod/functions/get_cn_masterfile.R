# library(faoebx5)
# library(data.table)

get_cn_masterfile <- function(local = T){
  
  if (local == T) {
    
    cn_masterfile <- readRDS("data/eucn_masterfile_latest.rds")
    
    return(cn_masterfile)
    
  }
  
  SetEBXCredentials(username = "Fishery-SOAP", password = 'W8EqZpDM4YBMKsrq')
  
  ##-- ISSCFC table ----
  isscfc <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_ISSCFC")
  isscfc <- isscfc[, list(Identifier, ISSCFC = Code, NameEn)]
  
  ##-- CN 2017 ----
  gr_cn17 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_CN82017_ISSCFC")
  cn17    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_CN8_2017")
  map17   <- merge(gr_cn17[, 1:2, with = F], cn17, by.x = "Group", by.y = "Identifier")
  map17   <- map17[, list(Group, Member, CN_Code = Code)]
  map17   <- merge(map17, isscfc, by.x = "Member", by.y = "Identifier")
  map17[, hsrep := 2017]
  
  ##-- CN 2015 ----
  gr_cn15 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_CN82015_ISSCFC")
  cn15    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_CN8_2015")
  map15   <- merge(gr_cn15[, 1:2, with = F], cn15, by.x = "Group", by.y = "Identifier")
  map15   <- map15[, list(Group, Member, CN_Code = Code)]
  map15   <- merge(map15, isscfc, by.x = "Member", by.y = "Identifier")
  map15[, hsrep := 2015]
  
  ##-- CN 2014 ----
  gr_cn14 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_CN82014_ISSCFC")
  cn14    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_CN8_2014")
  map14   <- merge(gr_cn14[, 1:2, with = F], cn14, by.x = "Group", by.y = "Identifier")
  map14   <- map14[, list(Group, Member, CN_Code = Code)]
  map14   <- merge(map14, isscfc, by.x = "Member", by.y = "Identifier")
  map14[, hsrep := 2014]
  
  ##-- CN 2013 ----
  gr_cn13 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_CN82013_ISSCFC")
  cn13    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_CN8_2013")
  map13   <- merge(gr_cn13[, 1:2, with = F], cn13, by.x = "Group", by.y = "Identifier")
  map13   <- map13[, list(Group, Member, CN_Code = Code)]
  map13   <- merge(map13, isscfc, by.x = "Member", by.y = "Identifier")
  map13[, hsrep := 2013]
  
  ##-- CN 2012 ----
  gr_cn12 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_CN82012_ISSCFC")
  cn12    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_CN8_2012")
  map12   <- merge(gr_cn12[, 1:2, with = F], cn12, by.x = "Group", by.y = "Identifier")
  map12   <- map12[, list(Group, Member, CN_Code = Code)]
  map12   <- merge(map12, isscfc, by.x = "Member", by.y = "Identifier")
  map12[, hsrep := 2012]
  
  ##-- HS Master file ----
  cn_masterfile <- rbindlist(list(map12, map13, map14, map15, map17), use.names = T)
  
  rm(list = c('map12', 'map13', 'map14', 'map15', 'map17', 'isscfc'))
  rm(list = c('cn12', 'cn13', 'cn14', 'cn15', 'cn17'))
  rm(list = c('gr_cn12', 'gr_cn13', 'gr_cn14', 'gr_cn15', 'gr_cn17'))
  
  cn_masterfile[, hs := stringr::str_pad(gsub("\\.", "", CN_Code), width = 12, side = "right", pad = "0")]
  cn_rep <- paste0("H", 0:5)
  names(cn_rep) <- as.character(c(2012, 2013, 2014, 2015, 2017))
  cn_masterfile[, hsrep := cn_rep[as.character(hsrep)]]
  
  # hs_masterfile_full <- copy(hs_masterfile)
  # hs_masterfile <- hs_masterfile[, list(hs, hsrep, faocode_hs6 =  ISSCFC)]
  
  return(cn_masterfile)
  
}





