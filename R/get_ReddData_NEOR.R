#' @title get_ReddData_NEOR:
#'
#' @description Retrieve North East Oregon spawning ground survey redd data from CDMS.
#'
#' @param SurveyYear desired survey year (YYYY).  NULL returns all.
#'
#' @param GRSME_ONLY TRUE: Return only GRSME data. FALSE: Return all ODFW data.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_ReddData_NEOR <- function(SurveyYear = NULL,
                              GRSME_ONLY = TRUE,
                              cdms_host = 'https://npt-cdms.nezperce.org'){

  if(GRSME_ONLY == TRUE) {cat('getSGSreddDataNEOR: Returning only GRSME data.') }
  else {cat('getSGSreddDataNEOR: Returning all North East Oregon data (GRSME and ODFW).')}

  if(!is.null(SurveyYear)) {
    if(!grepl('\\d{4}', SurveyYear))stop("SurveyYear must be a 4-digit year (YYYY).")
  }

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getsgsredddataneor')

  # ActivityID
  queryList <- list(SurveyYear = SurveyYear)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for SOMETHING from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  if(GRSME_ONLY) {
    df <- df %>%
      filter(Subbasin %in% c('Imnaha', 'Wallowa-Lostine', 'Wilderness-Wenaha', 'Wilderness-Minam'))
  }

  return(df)

}
