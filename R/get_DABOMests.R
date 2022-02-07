#' @title get_DABOMests:
#'
#' @description Retrieve DABOM (Dam Adult Branch Occupancy Model) escapement estimates from CDMS.
#'
#' @param Variable filter to return only a specific variable. NULL returns all variables.
#'
#' @param SpawnYear four digit year filter (YYYY) on Spawn Year. NULL returns all years.
#'
#' @param Species filter to return data from a single species. NULL returns all species.
#'
#' @param Run filter to return data from a single run of fish. NULL returns all runs.
#'
#' @param TRT_POPID filter to return data from a single ICTRT population. NULL returns all populations.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_DABOMests <- function(Variable = c('All', "Female Escapement", "Age Escapement",
                                       "Female Proportion", "Age Proportion", "Population Escapement",
                                       "Site Escapement", "Detection Efficiency"),
                          SpawnYear = NULL,
                          Species = c('All', 'Chinook salmon', 'Steelhead'),
                          Run = c('All', 'Spring/Summer', 'Summer'),
                          TRT_POPID = NULL,
                          cdms_host = 'https://npt-cdms.nezperce.org'){

  Variable <- match.arg(Variable)
  Species <- match.arg(Species)
  Run <- match.arg(Run)

  if(!is.null(SpawnYear)) {
    if(!grepl('\\d{4}', SpawnYear))stop("BroodYear must be a 4-digit year (YYYY).")
  }

  if(Variable == 'All') { Variable <- NULL}
  if(Species == 'All') { Species <- NULL}
  if(Run == 'All') { Run <- NULL}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getiptdsescdata')

  # ActivityID
  queryList <- list(Variable = Variable,
                    SpawnYear = SpawnYear,
                    Species = Species,
                    Run = Run,
                    TRT_POPID = TRT_POPID)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query IPTDS Escapement data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
