#' @title get_DetailRecords:
#'
#' @description get detail records for a given activity.
#'
#' @param activityID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_DetailRecords <- function(activityID, cdms_host = 'https://npt-cdms.nezperce.org'){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/activity/getdatasetactivitydata')

  # ActivityID
  queryList <- list(id = activityID)

  # GET request with query parameters
  req <- httr::GET(req_url,
                          query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query detail records for activityID = ', activityID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df[[3]])

}
