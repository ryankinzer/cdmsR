#' @title get_ProjectLocations:
#'
#' @description get Location information from CDMS for a specific project..
#'
#' @param ProjectID CDMS Project ID. see cdmsR::get_Projects()
#'
#' @param DatastoreID CDMS Datastore ID. see cdmsR::get_Datastores().  NULL returns all.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
#'
get_ProjectLocations <- function(ProjectID,
                                 cdms_host = 'https://npt-cdms.nezperce.org'){

  if(is.null(ProjectID))stop('ProjectID must be provided.')

  # url
  req_url <- paste0(cdms_host,'/services/api/v1/project/getlocations')

  queryList <- list(ProjectId = ProjectID)

  # GET request with query parameters

  httr::modify_url(req_url, query = queryList)

  req <- httr::GET(req_url,
                   query = queryList)

  httr::stop_for_status(req,
                        task = paste0('query project locations from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  return(df)

}
