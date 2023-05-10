#' @title get_DatasetView:
#'
#' @description get all data for a given dataset (the datastore), filtered by project if desired.
#'
#' @param datastoreID for CDMS dataset. see get_Datastores()
#'
#' @param projectID for project Id of interest in CDMS project table. see get_Projects()
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_DatasetView <- function(datastoreID, projectID = NULL,
                           cdms_host = 'https://npt-cdms.nezperce.org'){

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfulldatasetview')

  # ActivityID
  queryList <- list(id = datastoreID,
                    ProjectId = projectID)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for datastoreID = ', datastoreID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
