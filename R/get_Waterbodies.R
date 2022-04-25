#' @title get_Waterbodies:
#'
#' @description get all waterbodies listed in waterbodies table.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
#'
get_Waterbodies <- function(){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/list/getwaterbodies')

  # GET request with query parameters
  req <- httr::GET(req_url)

  httr::stop_for_status(req,
                        task = paste0('query waterbodies from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  return(df)

}
