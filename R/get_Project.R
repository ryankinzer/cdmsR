#' @title get_Project:
#'
#' @description get all information associated with a given project
#'
#' @param projectID for CDMS Project.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_Project <- function(projectID, cdms_host = 'https://npt-cdms.nezperce.org'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/project/getproject')

  # ProjectID
  queryList <- list(id = projectID)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for projectID = ', projectID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  df <- return(df)
}
