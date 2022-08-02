#' @title get_CarcassData_NEOR:
#'
#' @description Retrieve North East Oregon spawning ground survey carcass data from CDMS.
#'
#' @param survey_year desired survey year (YYYY). NULL returns all.
#'
#' @param grsme_only TRUE: Return only GRSME data. FALSE: Return all North East Oregon data.
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_CarcassData_NEOR <- function(survey_year = NULL,
                                 grsme_only = TRUE){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  if(grsme_only == TRUE) {cat('get_CarcassData_NEOR: Returning only GRSME data.') }
  else {cat('get_CarcassData_NEOR: Returning all North East Oregon data (GRSME and ODFW).')}

  if(!is.null(survey_year)) {
    if(!grepl('\\d{4}', survey_year))stop("survey_year must be a 4-digit year (YYYY).")
  }

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getsgscarcassdataneor')

  # ActivityID
  queryList <- list(SurveyYear = survey_year)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all North East Oregon carcass data records from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  if(grsme_only) {
    df <- df %>%
      filter(Subbasin %in% c('Imnaha', 'Wallowa-Lostine', 'Wilderness-Wenaha', 'Wilderness-Minam'))
  }

  return(df)

}
