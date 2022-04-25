#' @title get_ReddData:
#'
#' @description Retrieve spawning ground survey redd data from CDMS (does not include NE Oregon).
#'
#' @param survey_year desired survey year (YYYY).  NULL returns all.
#'
#' @param project desired project acronym.NULL returns all.
#'
#' @param location_label desired Location label. NULL returns all. see cdmsR::get_ProjectLocations()
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_ReddData <- function(survey_year = NULL,
                         project = c('All', 'IRSSSM', 'JCAPE', 'NPTH'),
                         location_label = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  project <- match.arg(project)

  if(!is.null(survey_year)) {
    if(!grepl('\\d{4}', survey_year))stop("survey_year must be a 4-digit year (YYYY).")
  }

  # assign dataset_id (essentially project filter)
  if(project == 'All') {dataset_id <- NULL}
  if(project == 'IRSSSM') {dataset_id <- 4334} # 11055
  if(project == 'JCAPE') {dataset_id <- 4321} # 11052
  if(project == 'NPTH') {dataset_id <- 4322} # 11062

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getsgsredddata')

  # ActivityID
  queryList <- list(SurveyYear = survey_year,
                    DatasetID = dataset_id,
                    LocationLabel = location_label)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query redd data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
