#' @title get_Users:
#'
#' @description gets information for users with CDMS login credentials.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_Users <- function(cdms_host = 'https://npt-cdms.nezperce.org/'
                       ){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/user/getallusers')

  # GET request with query parameters
  req <- httr::GET(req_url)

  httr::stop_for_status(req,
                        task = paste0('query User information from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  df <- df[,-c(1,2,3)]

  return(df)
}

