#' @title getWaterTemps()
#'
#' @description Retrieve CDMS water temperature data between two dates.
#'
#' @param date_begin First date of desired date range.
#'
#' @param date_end Final Date of desired date range.
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

getWaterTemps <- function(date_begin, date_end,  locationID = NULL,
                          cdms_host = 'https://npt-cdms.nezperce.org'){

  # Throw errors
  {if(is.null(date_begin) | !grepl('^\\d{4}-\\d{2}-\\d{2}$', date_begin))stop("date_begin must be provided as YYYY-MM-DD")}
  {if(is.null(date_end) | !grepl('^\\d{4}-\\d{2}-\\d{2}$', date_end))stop("date_end must be provided as YYYY-MM-DD")}
  {if(date_begin >= date_end)stop('date_end must be after date_begin (YYYY-MM-DD)')}

  # convert dates
  date_begin <- lubridate::ymd(date_begin)
  date_end <- lubridate::ymd(date_end)


  # loop
  temp_data <- lubridate::year(date_begin):lubridate::year(date_end) %>%
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
    filter(between(ReadingDate, date_begin, date_end)) %>%
    select(-ReadingDate)

  return(final_df)

}
