#' @title Write data from R to DT object on SWS
#' 
#' @description This function aims to update/write data from R to data tables objects on SWS. 
#'
#' @param dt_name character. The data table name which will be updated.
#' @param data data.table class. A data.table object in R that will be sent to SWS.
#' @param sql_filter which SQL filter it should be applied on the data table stored on SWS.
#'
#' @return Writing status
#' @export
#' @importFrom faosws ReadDatatable Changeset AddInsertions AddDeletions Finalise
#' @importFrom data.table data.table setcolorder
writeDT <- function(dt_name, data, sql_filter = NULL, insert = FALSE) {
  
  table2save <- ReadDatatable(table = dt_name, where = sql_filter, readOnly = T, limit = 1)
  data2up <- copy(data)
  data2up <- data2up[, colnames(table2save), with = FALSE]
  setcolorder(data2up, neworder = colnames(table2save))
  
  if(nrow(table2save) == 0 | insert == TRUE) {
    # Insert
    changeset <- Changeset(dt_name)
    AddInsertions(changeset, data2up)
    Finalise(changeset)
    
    cat("\nData inserted successfully.\n")
    
    return(c("OK" = 0))
    
  } else {
    table2save <- ReadDatatable(table = dt_name, where = sql_filter, readOnly = FALSE)
    
    # Update
    changeset <- Changeset(dt_name)
    AddDeletions(changeset, table2save)
    AddInsertions(changeset, data2up)
    Finalise(changeset)
    
    cat("\nData updated successfully.\n")
    
    return(c("OK" = 0))
    
  }
  
  
  
}