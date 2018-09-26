#' @title getActivities:
#'
#' @description
#'
#' @param datasetID for CDMS dataset.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples getActivities(datasetID, cdms_host = 'https://cdms.nptfisheries.org')
#'
#' @import httr jsonlite
#' @export
#' @return NULL

getActivities <- function(datasetID, cdms_host = 'https://cdms.nptfisheries.org'){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  acts_url <- paste0(cdms_host,'/services/api/v1/activity/getdatasetactivitiesview')

  # ActivityID
  queryList <- list(id = datasetID)

  # GET request with query parameters
  acts_req <- httr::GET(acts_url,
                        query = queryList)


  httr::stop_for_status(acts_req,
                        task = paste0('query activity records for datasetID = ', datasetID, ' from CDMS.'))

  # parse the response
  acts_con <- content(acts_req, type = 'text', encoding = "UTF-8")
  acts_df <- fromJSON(acts_con, flatten = TRUE)

  return(acts_df)

}
