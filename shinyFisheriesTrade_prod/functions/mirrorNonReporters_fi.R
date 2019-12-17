mirrorNonReporters_fi <- function(tradedata, mirror, factor = 1.12) {
  
  if (missing(tradedata)) stop("\"tradedata\" is missing.")
  if (missing(mirror))  stop("\"mirror\" is missing.")
  
  tradedata <- copy(tradedata)
  mirror <- copy(mirror)
  
  tradedata[, `:=`(reporterM49 = reporter, partnerM49 = partner)]
  
  tradedatanonrep <- tradedata %>%
    left_join(
      mirror %>%
        dplyr::mutate(flow = recode(flow, '1' = 2, '2' = 1)) %>%
        dplyr::mutate(i = 1),
      by = c("partner" = "area", "flow")
    ) %>%
    filter(i == 1) %>%
    select(-i) %>%
    dplyr::mutate_(
      partner_mirr = ~reporter,
      partner_mirrM49 = ~reporterM49,
      reporter = ~partner,
      reporterM49 = ~partnerM49,
      partner = ~partner_mirr,
      partnerM49 = ~partner_mirrM49,
      flow = ~recode(flow, '2' = 1, '1' = 2),
      ## CIF/FOB correction (fixed at 12%; further analyses needed)
      value = ~ifelse(flow == 1, value*factor, value/factor)
    ) %>%
    select_(~-partner_mirr, ~-partner_mirrM49) %>%
    setDT()
  
  ## Data mirrored 
  data_mirrored <- bind_rows(tradedata, tradedatanonrep)
  mirror[, mirrored := 1L]
  data_mirrored <- merge(data_mirrored, mirror, by.x = c("reporter", "flow"), by.y = c("area", "flow"), all.x = TRUE)
  
  data_mirrored[is.na(mirrored), mirrored := 0L]
  
  setDT(data_mirrored)
  
  return(data_mirrored)
  
}