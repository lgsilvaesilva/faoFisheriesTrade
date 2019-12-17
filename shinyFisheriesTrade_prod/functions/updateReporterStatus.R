#' @title Update the workflow status
#' 
#' @description This function aims to update the workflow status for each country and year. There is a data table called
#' by fishtrade_reporter_workflow that stores which stage of the process the given country and year are.
#'
#' @param year year
#' @param reporter reporter codded in M49 standard
#' @param process which process the function should updated: 'is_built_data', 'is_mapped_data', 'is_imputed_data', 'is_outlier_data', 'is_mirroring_data' .
#'
#' @return Workflow status
#' @export
#' @importFrom faosws ReadDatatable
#' @importFrom data.table data.table
updateReporterStatus <- function(.year, .reporter, .process, .value) {
  
  workflow_label <- names(ReadDatatable(table = "fishtrade_reporter_workflow", limit = 1))[-c(1:2)]
  
  sql_filter <- sprintf("rep = '%s' AND year = '%s'", .reporter,  .year)
  
  fishtrade_reporter_workflow <- "fishtrade_reporter_workflow"
  data2save <- ReadDatatable(table = fishtrade_reporter_workflow, where = sql_filter)  

  if(nrow(data2save) > 0) {

    for(i in seq_along(.process)) {
      data2save[, eval(parse(text = paste0(.process[i], " := ", .value[i])))]
    }
    
  } else {

    zero_list <- as.list(rep(0, ncol(data2save)))
    names(zero_list) <- names(data2save)
    data2save <- as.data.table(zero_list)

    data2save[, rep := .reporter]
    data2save[, year := .year]
    
    for(i in seq_along(.process)) {
      data2save[, eval(parse(text = paste0(.process[i], " := ", .value[i])))]
    }

  }
  
  writeDT(dt_name = fishtrade_reporter_workflow, 
          data = data2save, 
          sql_filter = sql_filter)
  
  
}
