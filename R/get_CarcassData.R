#' @title get_CarcassData:
#'
#' @description Retrieve spawning ground survey carcass data from CDMS (does not include NE Oregon).
#'
#' @param survey_year desired survey year (YYYY). NULL returns all.
#'
#' @param project desired project acronym. NULL returns all.
#'
#' @param location_label desired Location label. NULL returns all. see cdmsR::get_ProjectLocations()
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_CarcassData <- function(survey_year = NULL,
                            project = c('All', 'IRSSM', 'JCAPE', 'NPTH'),
                            location_label = NULL){

  project <- match.arg(project)

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  if(!is.null(survey_year)) {
    if(!grepl('\\d{4}', survey_year))stop("survey_year must be a 4-digit year (YYYY).")
  }

  # assign dataset_id (essentially project filter)
  if(project == 'All') {dataset_id <- NULL}
  if(project == 'IRSSSM') {dataset_id <- 4323} # 11055
  if(project == 'JCAPE') {dataset_id <- 4324} # 11052
  if(project == 'NPTH') {dataset_id <- 4325} # 11062

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getsgscarcassdata')

  # ActivityID
  queryList <- list(SurveyYear = survey_year,
                    DatasetID = dataset_id,
                    LocationLabel = location_label)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query carcass data records from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
