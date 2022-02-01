#' @title get
#'
#' @description get data from
#'
#' @param SpawnLocation desired Spawning Location. NULL returns all. Discover values: getFINSvalues()
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

getFINSspawningData <- function(SpawnLocation = NULL,
                                Stock = NULL,
                                Species = NULL,
                                Run = c('All', NA, 'Spring', 'Summer', 'Fall', 'Winter'),
                                Sex = c('All', 'Female', 'Male', 'Unknown'),
                                Origin = NULL,
                                cdms_host = 'https://npt-cdms.nezperce.org'){

  Run = match.arg(Run)
  Sex = match.arg(Sex)

  cat('See the getFINSvalues() function for help determining field values.')

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfinsspawningdata')

  # ActivityID
  queryList <- list(id = NULL)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query P4 data based on inputs.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
