#' @title get_HeaderRecords:
#'
#' @description get header records for a given activity.
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

get_HeaderRecords <- function(datasetID, cdms_host = 'https://npt-cdms.nezperce.org'){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/dataset/getheadersdatafordataset')

  # ActivityID
  queryList <- list(id = datasetID)

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query header records for datasetID = ', datasetID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  df <- dplyr::select(df, HeaderID = Id, ActivityID = ActivityId, dplyr::everything())

  return(df)

}
