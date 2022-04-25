#' @title get_ProjectLocations:
#'
#' @description get Location information from CDMS for a specific project.
#'
#' @param project_id CDMS Project ID. see cdmsR::get_Projects()
#'
#' @param datastore_id CDMS Datastore ID. NULL returns all. see cdmsR::get_Datastores().
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL
#'
#'
get_ProjectLocations <- function(project_id,
                                 datastore_id = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  if(is.null(project_id))stop('project_id must be provided.')

  # url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getprojectlocations')

  queryList <- list(ProjectID = project_id,
                    DatastoreID = datastore_id)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)

  httr::stop_for_status(req,
                        task = paste0('query project locations from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  return(df)

}
