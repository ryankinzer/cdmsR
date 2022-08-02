#' @title get_Datastores:
#'
#' @description gets all available CDMS datastores.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_Datastores <- function(){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/datastore/getdatastores')

  # GET request with query parameters
  req <- httr::GET(req_url)

  httr::stop_for_status(req,
                        task = paste0('query datastores from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  return(df)
}
