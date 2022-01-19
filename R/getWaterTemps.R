#' @title getWaterTemps()
#'
#' @description Retrieve CDMS water temperature data between two dates.
#'
#' @param StartDate First date of desired date range.
#'
#' @param End Final Date of desired date range.
#'
#' @param projectID for project of interest in CDMS project table.
#'
#' @param locationID from location table.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

getWaterTemps <- function(start_date, end_date,  locationID = NULL,
                          cdms_host = 'https://npt-cdms.nezperce.org'){

  # Throw errors
  {if(is.null(start_date) | !grepl('^\\d{2}-\\d{2}-\\d{4}$', start_date))stop("start_date must be provided as MM-DD-YYYY")}
  {if(is.null(end_date) | !grepl('^\\d{2}-\\d{2}-\\d{4}$', end_date))stop("end_date must be provided as MM-DD-YYYY")}
  {if(start_date >= end_date)stop('end_date must be after start_date (MM-DD-YYYY)')}

  # TESTING
  # start_date = '07-01-2018'
  # # end_date = '06-30-2019'
  # start_date <- '02-01-2020'
  # end_date <- '10-31-2020'
  # locationID = NULL
  # cdms_host = 'http://localhost:80/'

  # for whatever reason this seems necessary - especially for final filter.
  dates <- c(lubridate::mdy(start_date), lubridate::mdy(end_date))

  # loop through years
  temp_data <- lubridate::year(dates[[1]]):lubridate::year(dates[[2]]) %>%
    map_df(.f = function(.x){
      # build URL for API
      req_url <- paste0(cdms_host,'/services/api/v1/npt/getwatertempdata')
      queryList <- list(Year = .x,
                        LocationId = locationID)

      # httr::modify_url(req_url, query = queryList)

      # GET request with query parameters
      req <- httr::GET(req_url,
                       query = queryList)


      httr::stop_for_status(req,
                            task = paste0('query all water temperature records from ', .x, '.'))

      # parse the response
      req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
      df <- jsonlite::fromJSON(req_con, flatten = TRUE)
    }
    )


  # filter for provided start/end dates
  final_df <- temp_data %>%
    mutate(ReadingDate = lubridate::ymd_hms(ReadingDateTime),
           ReadingDate = lubridate::as_date(ReadingDate)) %>%
    filter(between(ReadingDate, dates[[1]], dates[[2]]))

  return(final_df)

}
