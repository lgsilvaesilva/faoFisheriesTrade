get_mapping_isscfc_fao_group <- function(.local) {
  
  if(.local == TRUE) {
    
    isscfc_mapping_fao_group <- readRDS("data/isscfc_mapping_fao_group.rds")
    
  } else {
    
    #' *XXX*: we must get this table from EBX5, but EBX5 was shutdown.
    isscfc_mapping_fao_group <- readRDS("data/isscfc_mapping_fao_group.rds")
    
  }
  
  return(isscfc_mapping_fao_group)
  
}