#' @title get
#'
#' @description get data from
#'
#' @param Facility desired FINS facility name. NULL returns all. Discover values: getFINSvalues()
#'
#' @param Species desired species. NULL returns all. Discover values: getFINSvalues()
#'
#' @param Run desired run of fish. NULL returns all.
#'
#' @param Sex desired sex of fish. NULL returns all.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

getFINSweirData <- function(Facility = NULL,
                            Species = NULL,
                            Run = c('All', NA, 'Spring', 'Summer', 'Fall', 'Winter'),
                            Sex = c('All', 'Female', 'Male', 'Unknown'),
                            Origin = NULL,
                            cdms_host = 'https://npt-cdms.nezperce.org'){

  Run = match.arg(Run)
  Sex = match.arg(Sex)

  cat('Use getFINSvalues() to determine field values.')

  # set NULLs
  if(Run == 'All') { Run <- NULL}
  if(Sex == 'All') { Sex <- NULL}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfinsweirdata')

  # ActivityID
  queryList <- list(Facility = Facility,
                    Species = Species,
                    Run = Run,
                    Sex = Sex,
                    Origin = Origin)

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
