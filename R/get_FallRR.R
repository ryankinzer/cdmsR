#' @title get_FallRR:
#'
#' @description Retrieve Fall Chinook Run Reconstruction data from CDMS.
#'
#' @param brood_year four digit year filter (YYYY) on Brood Year. NULL returns all years.
#'
#' @param return_year four digit year filter (YYYY) on Return Year. NULL returns all years.
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_FallRR <- function(brood_year = NULL,
                       return_year = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # errors
  if(!is.null(return_year)) {
    if(!grepl('\\d{4}', return_year))stop("return_year must be a 4-digit year (YYYY).")
  }

  if(!is.null(brood_year)) {
    if(!grepl('\\d{4}', brood_year))stop("brood_year must be a 4-digit year (YYYY).")
  }

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfcrrdata')

  # ActivityID
  queryList <- list(ReturnYear = return_year,
                    BroodYear = brood_year)

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
