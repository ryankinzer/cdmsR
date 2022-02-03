#' @title get_STADEMests
#'
#' @description get data from
#'
#' @param SpawnYear four digit year filter (YYYY) on Brood Year. NULL returns all years.
#'
#' @param Species filter to return data from a single species. NULL returns all species.
#'
#' @param Run filter to return data from a single run of fish. NULL returns all runs.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_STADEMests <- function(SpawnYear = NULL,
                            Species = c('All', 'Chinook salmon', 'Steelhead'),
                            Run = c('All', 'Spring/Summer', 'Summer'),
                            cdms_host = 'https://npt-cdms.nezperce.org'){

  Species <- match.arg(Species)
  Run <- match.arg(Run)

  if(!is.null(SpawnYear)) {
    if(!grepl('\\d{4}', SpawnYear))stop("BroodYear must be a 4-digit year (YYYY).")
  }

  if(Species == 'All') { Species <- NULL}
  if(Run == 'All') { Run <- NULL}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getiptdslgrdata')

  # ActivityID
  queryList <- list(SpawnYear = SpawnYear,
                    Species = Species,
                    Run = Run)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query IPTDS Lower Granite Dam data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
