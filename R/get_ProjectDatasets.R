#' @title get_ProjectDatasets:
#'
#' @description get a list of datasets associated with a project.
#'
#' @param project_id CDMS Project ID. see cdmsR::get_Projects()
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_ProjectDatasets <- function(project_id){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/project/getprojectdatasets')

  query_list <- list(id = project_id)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = query_list)

  httr::stop_for_status(req,
                        task = paste0('query project datasets from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  df <- dplyr::select(df, DatastoreId, DatasetId = Id, ProjectId, Name, Description, CreateDateTime)

  return(df)
}
