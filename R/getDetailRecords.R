#' @title getDetailRecords:
#'
#' @description
#'
#' @param activityID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples getDetailRecords(activityID, cdms_host = 'https://cdms.nptfisheries.org')
#'
#' @import httr jsonlite
#' @export
#' @return NULL

getDetailRecords <- function(activityID, cdms_host = 'https://cdms.nptfisheries.org'){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  detail_url <- paste0(cdms_host,'/services/api/v1/activity/getdatasetactivitydata')

  # ActivityID
  queryList <- list(id = activityID)

  # GET request with query parameters
  detail_req <- httr::GET(detail_url,
                          query = queryList)


  httr::stop_for_status(detail_req,
                        task = paste0('query detail records for activityID = ', activityID, ' from CDMS.'))

  # parse the response
  detail_con <- content(detail_req, type = 'text', encoding = "UTF-8")

  detail_df <- fromJSON(detail_con, flatten = TRUE)

  return(detail_df[[3]])

}
