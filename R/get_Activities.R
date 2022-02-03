#' @title get_Activities:
#'
#' @description get all activities in CDMS.
#'
#' @param datasetID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_Activities <- function(datasetID, cdms_host = 'https://npt-cdms.nezperce.org'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/activity/getdatasetactivitiesview')

  # ActivityID
  queryList <- list(id = datasetID)

  #httr::modify_url(req_url, query = queryList)
  # Should be:
  # https://npt-cdms.nezperce.org/services/api/v1/activity/getdatasetactivitiesview?id=4335

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query activity records for datasetID = ', datasetID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  df <- dplyr::select(df, ActivityID = Id, dplyr::everything())

  return(df)

}
