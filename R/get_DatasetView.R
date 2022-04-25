#' @title get_DatasetView:
#'
#' @description get all data for a given datastore, filtered by project if desired..
#'
#' @param datastore_id CDMS Datastore ID. see cdmsR::get_Datastores()
#'
#' @param project_id CDMS Project ID. NULL returns all. see cdmsR::get_Projects()
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL

get_DatasetView <- function(datastore_id, project_id = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfulldatasetview')

  # query parameters
  queryList <- list(id = datastore_id,
                    ProjectId = project_id)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for datastore_id = ', datastore_id, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
