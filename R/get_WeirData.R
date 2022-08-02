#' @title get_WeirData:
#'
#' @description Retrieve FINS (Fisheries Inventory System) Trapping Module data from CDMS.
#'
#' @param facility desired FINS facility name. NULL returns all. see get_FINSvalues()
#'
#' @param species desired species. NULL returns all. see get_FINSvalues()
#'
#' @param run desired run of fish. NULL returns all.
#'
#' @param sex desired sex of fish. NULL returns all.
#'
#' @param origin desired origin of fish. NULL returns all. see get_FINSvalues()
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_WeirData <- function(facility = NULL,
                         species = NULL,
                         run = c('All', NA, 'Spring', 'Summer', 'Fall', 'Winter'),
                         sex = c('All', 'Female', 'Male', 'Unknown'),
                         origin = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  run = match.arg(run)
  sex = match.arg(sex)

  cat('If needed, use get_FINSvalues() to determine field values.')

  # set NULLs
  if(run == 'All') { run <- NULL}
  if(sex == 'All') { sex <- NULL}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfinsweirdata')

  # ActivityID
  queryList <- list(Facility = facility,
                    Species = species,
                    Run = run,
                    Sex = sex,
                    Origin = origin)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query FINS trapping data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
