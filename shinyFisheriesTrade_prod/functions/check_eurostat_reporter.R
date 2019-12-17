# Check whether the M49 code passes as parameter (in a given year)
# is a reporter available in ce_combinednomenclature_unlogged_YEAR
# table: if it is, then it's an Eurostat reporter.

check_eurostat_reporter <- function(m49reporter, year) {

  geonomcode <- m49_to_geonom(m49reporter)

  esdata <- data_frame()

  # If 0, not Eurostat code
  if (length(geonomcode) == 1 & !is.na(geonomcode)) {

    esdata <- ReadDatatable(
      paste0("ce_combinednomenclature_unlogged_", year),
      columns = c(
        "period",
        "declarant"),
      limit = 1,
      where = paste0("declarant IN ('", geonomcode, "')")
    )
  }

  return(ifelse(nrow(esdata) > 0, TRUE, FALSE))
}

