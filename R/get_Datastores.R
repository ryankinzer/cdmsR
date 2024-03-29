#' @title get_Datastores:
#'
#' @description gets all the available datasets from the datastore table.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @export
#'
#' @return NULL
#'
get_Datastores <- function(cdms_host = 'https://npt-cdms.nezperce.org'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

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
