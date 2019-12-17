# # library(faoebx5)
# # library(data.table)
# 
# get_hs_masterfile <- function(local = T){
#   
#   if (local == T) {
#     
#     hs_masterfile <- readRDS("data/hs_masterfile.rds")
#     
#     return(hs_masterfile)
#     
#   }
#   
#   SetEBXCredentials(username = "Fishery-SOAP", password = 'W8EqZpDM4YBMKsrq')
#   
#   ##-- ISSCFC table ----
#   isscfc <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_ISSCFC")
#   isscfc <- isscfc[, list(Identifier, ISSCFC = Code, NameEn)]
#   
#   ##-- HS 2017 ----
#   gr_hs17 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_HS17L3_ISSCFC")
#   gr_hs17 <- gr_hs17[ISSCFCmapping == 'true', ]
#   hs17    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_HS2017_L3")
#   map17   <- merge(gr_hs17[, 1:2, with = F], hs17, by.x = "Group", by.y = "Identifier")
#   map17   <- map17[, list(Group, Member, HS_Code = Code)]
#   map17   <- merge(map17, isscfc, by.x = "Member", by.y = "Identifier")
#   map17[, hsrep := 2017]
#   
#   ##-- HS 2012 ----
#   gr_hs12 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_HS12L3_ISSCFC")
#   hs12    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_HS2012_L3")
#   map12   <- merge(gr_hs12[, 1:2, with = F], hs12, by.x = "Group", by.y = "Identifier")
#   map12   <- map12[, list(Group, Member, HS_Code = Code)]
#   map12   <- merge(map12, isscfc, by.x = "Member", by.y = "Identifier")
#   map12[, hsrep := 2012]
#   
#   ##-- HS 2007 ----
#   gr_hs07 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_HS07L3_ISSCFC")
#   hs07    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_HS2007_L3")
#   map07   <- merge(gr_hs07[, 1:2, with = F], hs07, by.x = "Group", by.y = "Identifier")
#   map07   <- map07[, list(Group, Member, HS_Code = Code)]
#   map07   <- merge(map07, isscfc, by.x = "Member", by.y = "Identifier")
#   map07[, hsrep := 2007]
#   
#   ##-- HS 2002 ----
#   gr_hs02 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_HS02L3_ISSCFC")
#   hs02    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_HS2002_L3")
#   map02   <- merge(gr_hs02[, 1:2, with = F], hs02, by.x = "Group", by.y = "Identifier")
#   map02   <- map02[, list(Group, Member, HS_Code = Code)]
#   map02   <- merge(map02, isscfc, by.x = "Member", by.y = "Identifier")
#   map02[, hsrep := 2002]
#   
#   ##-- HS 1996 ----
#   gr_hs96 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_HS96L3_ISSCFC")
#   hs96    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_HS1996_L3")
#   map96   <- merge(gr_hs96[, 1:2, with = F], hs96, by.x = "Group", by.y = "Identifier")
#   map96   <- map96[, list(Group, Member, HS_Code = Code)]
#   map96   <- merge(map96, isscfc, by.x = "Member", by.y = "Identifier")
#   map96[, hsrep := 1996]
#   
#   ##-- HS 1992 ----
#   gr_hs92 <- ReadEBXGroup(sdmx_name = "HCL_FI_COMMODITY_HS92L3_ISSCFC")
#   hs92    <- ReadEBXCodeList(sdmx_name = "CL_FI_COMMODITY_HS1992_L3")
#   map92   <- merge(gr_hs92[, 1:2, with = F], hs92, by.x = "Group", by.y = "Identifier")
#   map92   <- map92[, list(Group, Member, HS_Code = Code)]
#   map92   <- merge(map92, isscfc, by.x = "Member", by.y = "Identifier")
#   map92[, hsrep := 1992]
#   
#   ##-- HS Master file ----
#   hs_masterfile <- rbindlist(list(map92, map96, map02, map07, map12, map17), use.names = T)
#   
#   rm(list = c('map92', 'map96', 'map02', 'map07', 'map12', 'map17'))
#   rm(list = c('hs92', 'hs96', 'hs02', 'hs07', 'hs12', 'hs17', 'isscfc'))
#   rm(list = c('gr_hs92', 'gr_hs96', 'gr_hs02', 'gr_hs07', 'gr_hs12', 'gr_hs17'))
#   
#   hs_masterfile[, hs := stringr::str_pad(gsub("\\.", "", HS_Code), width = 12, side = "right", pad = "0")]
#   hs_rep <- paste0("H", 0:5)
#   names(hs_rep) <- as.character(c(1992, 1996, 2002, 2007, 2012, 2017))
#   hs_masterfile[, hsrep := hs_rep[as.character(hsrep)]]
#   
#   # hs_masterfile_full <- copy(hs_masterfile)
#   # hs_masterfile <- hs_masterfile[, list(hs, hsrep, faocode_hs6 =  ISSCFC)]
#   
#   return(hs_masterfile)
#   
# }
# 
# 
# 
# 
# 
