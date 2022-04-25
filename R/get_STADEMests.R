#' @title get_STADEMests:
#'
#' @description Retrieve STADEM (STate space Adult Dam Escapement Model) estimates from CDMS.
#'
#' @param spawn_year four digit year filter (YYYY) on Spawn Year. NULL returns all.
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

get_STADEMests <- function(spawn_year = NULL,
                           species = c('All', 'Chinook salmon', 'Steelhead'),
                           run = c('All', 'Spring/Summer', 'Summer')){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  species <- match.arg(species)
  run <- match.arg(run)

  if(!is.null(spawn_year)) {
    if(!grepl('\\d{4}', spawn_year))stop("spawn_year must be a 4-digit year (YYYY).")
  }

  if(species == 'All') { species <- NULL}
  if(run == 'All') { run <- NULL}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getiptdslgrdata')

  # ActivityID
  queryList <- list(SpawnYear = spawn_year,
                    Species = species,
                    Run = run)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query STADEM estimates from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
