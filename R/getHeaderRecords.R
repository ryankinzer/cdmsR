#' @title getHeaderRecords:
#'
#' @description
#'
#' @param datasetID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples getHeaderRecords(datasetID, cdms_host = 'https://cdms.nptfisheries.org')
#'
#' @import httr jsonlite
#' @export
#' @return NULL

getHeaderRecords <- function(datasetID, cdms_host = 'https://cdms.nptfisheries.org'){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  head_url <- paste0(cdms_host,'/services/api/v1/dataset/getheadersdatafordataset')

  # ActivityID
  queryList <- list(id = datasetID)

  # GET request with query parameters
  head_req <- httr::GET(head_url,
                        query = queryList)


  httr::stop_for_status(head_req,
                        task = paste0('query header records for datasetID = ', datasetID, ' from CDMS.'))

  # parse the response
  head_con <- content(head_req, type = 'text', encoding = "UTF-8")
  head_df <- fromJSON(head_con, flatten = TRUE)

  return(head_df)

}
