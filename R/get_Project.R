#' @title get_Project:
#'
#' @description get all information associated with a given project
#'
#' @param project_id CDMS Project ID. see cdmsR::get_Projects()
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_Project <- function(project_id){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/project/getproject')

  # project_id
  queryList <- list(id = project_id)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all project information for project_id = ', project_id, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  df <- return(df)
}
