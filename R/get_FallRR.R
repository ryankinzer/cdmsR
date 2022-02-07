#' @title get_FallRR:
#'
#' @description Retrieve Fall Chinook Run Reconstruction data from CDMS.
#'
#' @param BroodYear four digit year filter (YYYY) on Brood Year. NULL returns all years.
#'
#' @param ReturnYear four digit year filter (YYYY) on Return Year. NULL returns all years.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_FallRR <- function(BroodYear = NULL,
                       ReturnYear = NULL,
                       cdms_host = 'https://npt-cdms.nezperce.org'){

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfcrrdata')

  # ActivityID
  queryList <- list(ReturnYear = ReturnYear,
                    BroodYear = BroodYear)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)

  # req[["url"]]


  httr::stop_for_status(req,
                        task = paste0('query Fall Chinook Run Reconstruction data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
