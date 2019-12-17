##' @import dplyr

# removeInvalidReporters <- function(tradedata) {
# # removeInvalidReporters <- function(tradedata, year) {  
#   if (missing(tradedata)) stop('"tradedata" should be set.')
#   
#   stopifnot("reporter" %in% colnames(tradedata))
#   
#   # Create a table with valid reporters
#   # valid_reporters <-
#   #   GetCodeList(
#   #     domain    = "faostat_one",
#   #     dataset   = "FS1_SUA_UPD",
#   #     dimension = "geographicAreaFS"
#   #   ) %>%
#   #   dplyr::tbl_df() %>%
#   #   dplyr::filter(type != "group") %>%
#   #   dplyr::mutate(
#   #     startDate = as.numeric(stringr::str_sub(startDate, 1, 4)),
#   #     endDate   = as.numeric(stringr::str_sub(endDate, 1, 4))
#   #   ) %>%
#   #   dplyr::select(-selectionOnly, -type) %>%
#   #   dplyr::filter(startDate <= year, endDate >= year)
#   # 
#   # reporters_to_drop <- setdiff(unique(tradedata$reporter), valid_reporters$code)
#   # 
#   # if (length(reporters_to_drop) > 0) {
#   #   x <- ifelse(length(reporters_to_drop) == 1, "y", "ies")
#   #   warning(paste0("The following countr", x, " did not exist in ", year, ": ",
#   #                 paste(reporters_to_drop, collapse=" ")))
#   # 
#   #   # Keep valid reporters
#   #   tradedata %>%
#   #     dplyr::filter(reporter %in% valid_reporters$code)
#   # } else {
#   #   tradedata
#   # }
#   valid_reporters <-
#     GetCodeList(
#       domain    = "faostat_one",
#       dataset   = "FS1_SUA_UPD",
#       dimension = "geographicAreaFS"
#     )
#   
#   valid_reporters <- valid_reporters[type != "group", 
#                   list(
#                     reporter = code,
#                     description,
#                     startDate = year(startDate),
#                     endDate   = year(endDate))]
#   
#   
#   tradedata <- valid_reporters[tradedata, on = 'reporter']
#   
#   reporters_to_drop <- unique(tradedata[!(startDate <= year & endDate >= year), 
#                                  list(reporter, description, year)])
#   
#   if (length(reporters_to_drop) > 0) {
#     
#     x <- ifelse(length(reporters_to_drop) == 1, "y", "ies")
#     msg <- sprintf("The country %s (%s) did not exist in %s ", 
#                    reporters_to_drop$reporter, 
#                    reporters_to_drop$description,
#                    reporters_to_drop$year)
#     
#     warning(paste(msg, collapse = "\n"))
#     
#     # Keep valid reporters
#     tradedata <- tradedata[(startDate <= year & endDate >= year), ]
#     
#     tradedata[, `:=`(description = NULL, 
#                      startDate = NULL, 
#                      endDate = NULL)]
#       
#   } else {
#     
#     tradedata[, `:=`(description = NULL, 
#                      startDate = NULL, 
#                      endDate = NULL)]
#     
#   }
# }

##' @import dplyr

removeInvalidReporters <- function(tradedata, year) {
  
  if (missing(tradedata)) stop('"tradedata" should be set.')
  
  stopifnot("reporter" %in% colnames(tradedata))
  
  # Create a table with valid reporters
  valid_reporters <-
    GetCodeList(
      domain    = "faostat_one",
      dataset   = "FS1_SUA_UPD",
      dimension = "geographicAreaFS"
    ) %>%
    dplyr::tbl_df() %>%
    dplyr::filter(type != "group") %>%
    dplyr::mutate(
      startDate = as.numeric(stringr::str_sub(startDate, 1, 4)),
      endDate   = as.numeric(stringr::str_sub(endDate, 1, 4))
    ) %>%
    dplyr::select(-selectionOnly, -type) %>%
    dplyr::filter(startDate <= year, endDate >= year)
  
  reporters_to_drop <- setdiff(unique(tradedata$reporter), valid_reporters$code)
  
  if (length(reporters_to_drop) > 0) {
    x <- ifelse(length(reporters_to_drop) == 1, "y", "ies")
    warning(paste0("The following countr", x, " did not exist in ", year, ": ",
                   paste(reporters_to_drop, collapse=" ")))
    
    # Keep valid reporters
    tradedata %>%
      dplyr::filter(reporter %in% valid_reporters$code)
  } else {
    tradedata
  }
}


