#' @title getDatasetView:
#'
#' @description get the big bucket for a given dataset, filtered by project,
#'  waterbody and/or location.
#'
#' @param datastoreID for CDMS dataset.
#'
#' @param species for Species of interest
#'
#' @param run for run associated with species
#'
#' @param pop_name for Population of interest
#'
#' @param survey_year year of interest.
#'
#' @param projectID for project of interest in CDMS project table.
#'
#' @param waterbodyID for stream in CDMS waterbodies table.
#'
#' @param locationID from location table.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @export
#'
#' @return NULL

getDatasetView <- function(datastoreID, projectID = NULL,
                           # survey_year, species = c('Chinook salmon', 'Steelhead'),
                           # run = c('Spring/summer', 'Summer', 'Fall'),
                           # pop_name = NULL, waterbodyID = NULL,
                           # locationID = NULL,
                           cdms_host = 'https://npt-cdms.nezperce.org'){

  # Species <- match.arg(species)
  # Species <-  paste0("'",Species,"'")
  #
  # Run <- match.arg(run)
  # Run <- paste0("'",Run,"'")
  #
  # stopifnot(!is.null(survey_year))
  #
  # if(length(survey_year) == 1){
  #   survey_year <- c(survey_year,survey_year)
  # }
  #
  # if(!is.null(pop_name)){
  #   pop_name <- paste0("'",pop_name,"'")
  # }

  # if(!is.null(StreamName)){
  #   StreamName <- paste0("'",StreamName,"'")
  # }


  # detail url
  # req_url <- paste0(cdms_host,'/services/api/v1/dataset/getfulldatasetview')
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfulldatasetview')  # if we hide new NPT services in NPTController.cs, /npt/ replaces /dataset/ (DatasetController.cs)


  # ActivityID
  queryList <- list(id = datastoreID,
                    # Species = species,
                    # Run = run,
                    # POP_NAME = pop_name,
                    # SurveyYear = survey_year,
                    ProjectId = projectID) #,
                    # WaterBodyId = waterbodyID,
                    # LocationId = locationID)
                    # StartYear = SurveyYear[1],
                    # EndYear = SurveyYear[2],
                    # MPG = MPG,
                    # POP = POP,
                    # StreamName = StreamName)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for datastoreID = ', datastoreID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
