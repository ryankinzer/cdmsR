#' @title get_WaterTempData:
#'
#' @description Retrieve CDMS water temperature data from CDMS between provided dates.
#'
#' @param date_begin First date of desired date range formatted as YYYY-MM-DD.
#'
#' @param date_end Final Date of desired date range formatted as YYYY-MM-DD.
#'
#' @param location_id The CDMS Location ID of the desired location. see cdmsR::get_ProjectLocations()
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_WaterTempData <- function(date_begin,
                              date_end,
                              location_id = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

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
                        LocationId = location_id)

      # httr::modify_url(req_url, query = queryList)

      # GET request with query parameters
      req <- httr::GET(req_url,
                       query = queryList)


      httr::stop_for_status(req,
                            task = paste0('query water temperature records from CDMS.'))

      # parse the response
      req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
      df <- jsonlite::fromJSON(req_con, flatten = TRUE)
    }
    )


  # filter for provided start/end dates
  if(nrow(temp_data) != 0 ) {
    final_df <- temp_data %>%
      mutate(ReadingDate = lubridate::ymd_hms(ReadingDateTime),
             ReadingDate = lubridate::as_date(ReadingDate)) %>%
      filter(between(ReadingDate, date_begin, date_end)) %>%
      select(-ReadingDate)
  } else {
    final_df <- temp_data
    cat('get_WaterTempData(): No data returned for provided query.')
  }

  return(final_df)

}
