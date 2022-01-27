#' @title get
#'
#' @description get data from
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

getSGScarcassData <- function(cdms_host = 'https://npt-cdms.nezperce.org'){

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getsgscarcassdata')

  # ActivityID
  queryList <- list(id = NULL)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for SOMETHING from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
