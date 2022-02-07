#' @title get_ReddData:
#'
#' @description Retrieve spawning ground survey redd data from CDMS (does not include NE Oregon).
#'
#' @param SurveyYear desired survey year (YYYY).  NULL returns all.
#'
#' @param Project desired project acronym.  NULL returns all.
#'
#' @param LocationLabel desired Location label. NULL returns all.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_ReddData <- function(SurveyYear = NULL,
                         Project = c('All', 'IRSSM', 'JCAPE', 'NPTH'),
                         LocationLabel = NULL,
                         cdms_host = 'https://npt-cdms.nezperce.org'){

  Project <- match.arg(Project)

  if(!is.null(SurveyYear)) {
    if(!grepl('\\d{4}', SurveyYear))stop("SurveyYear must be a 4-digit year (YYYY).")
  }

  # assign DatasetID (essentially project filter)
  if(Project == 'All') {DatasetID <- NULL}
  if(Project == 'IRSSSM') {DatasetID <- 4334} # 11055
  if(Project == 'JCAPE') {DatasetID <- 4321} # 11052
  if(Project == 'NPTH') {DatasetID <- 4322} # 11062

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getsgsredddata')

  # ActivityID
  queryList <- list(SurveyYear = SurveyYear,
                    DatasetID = DatasetID,
                    LocationLabel = LocationLabel)

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
