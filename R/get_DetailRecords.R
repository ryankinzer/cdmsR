#' @title get_DetailRecords:
#'
#' @description get detail records for a given activity.
#'
#' @param activity_id ID of specific CDMS activity.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL

get_DetailRecords <- function(activity_id){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/activity/getdatasetactivitydata')

  # query parameters
  queryList <- list(id = activity_id)

  # GET request with query parameters
  req <- httr::GET(req_url,
                          query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query detail records for activity_id = ', activity_id, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df[[3]])

}
