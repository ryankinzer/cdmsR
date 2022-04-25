#' @title get_DABOMrecruits:
#'
#' @description Retrieve DABOM (Dam Adult Branch Occupancy Model) recruits estimates from CDMS.
#'
#' @param variable filter to return only a specific variable. NULL returns all.
#'
#' @param brood_year four digit year filter (YYYY) on Brood Year. NULL returns all.
#'
#' @param species filter to return data from a single species. NULL returns all.
#'
#' @param run filter to return data from a single run of fish. NULL returns all.
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_DABOMrecruits <- function(variable = c('All', 'Recruits', 'Spawners', 'lambda'),
                              brood_year = NULL,
                              species = c('All', 'Chinook salmon', 'Steelhead'),
                              run = c('All', 'Spring/Summer', 'Summer')){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  variable <- match.arg(variable)
  species <- match.arg(species)
  run <- match.arg(run)

  if(!is.null(brood_year)) {
    if(!grepl('\\d{4}', brood_year))stop("brood_year must be a 4-digit year (YYYY).")
  }

  if(variable == 'All') { variable <- NULL}
  if(species == 'All') { species <- NULL}
  if(run == 'All') { run <- NULL}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getiptdsrecruitsdata')

  # ActivityID
  queryList <- list(Variable = variable,
                    BroodYear = brood_year,
                    Species = species,
                    Run = run)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query DABOM Recruits data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
