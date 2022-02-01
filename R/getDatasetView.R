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
                           cdms_host = 'https://npt-cdms.nezperce.org'){

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfulldatasetview')

  # ActivityID
  queryList <- list(id = datastoreID,
                    ProjectId = projectID)

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
