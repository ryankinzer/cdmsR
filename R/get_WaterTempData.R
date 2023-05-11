#' @title get_WaterTempData:
#'
#' @description Retrieve CDMS water temperature data from CDMS for a calendar year.
#'
#' @param year Desired year of data (YYYY).
#'
#' @param locationID Location ID of the desired location. See get_ProjectLocations()
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_WaterTempData <- function(year,
                              locationID = NULL,
                              cdms_host = 'https://npt-cdms.nezperce.org'){

  # Throw errors
  {if(is.null(year) | !grepl('^\\d{4}$', year))stop("year must be provided as YYYY")}

  # build URL for API
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getwatertempdata')
  queryList <- list(Year = year,
                    LocationId = locationID)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query water temperature records from ', x, '.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
