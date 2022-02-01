#' @title get
#'
#' @description get data from
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

getJuvAbundanceData <- function(RST = c('All', 'Imnaha River', 'Johnson Creek', 'Lolo Creek', 'Newsome Creek', 'Secesh River', 'South Fork Clearwater River'),
                                SpeciesRun = NULL, MigratoryYear = NULL, BroodYear = NULL, Origin = c('All', 'Hatchery', 'Natural'),
                                cdms_host = 'https://npt-cdms.nezperce.org'){

  RST <- match.arg(RST)
  Origin <- match.arg(Origin)

  if(Origin == 'All') { Origin <- NULL}

  # generate location label
  if(RST == 'All') { LocationLabel <- NULL}
  if(RST == 'Imnaha River') { LocationLabel <- 'Imnaha River: Imnaha River RST'}
  if(RST == 'Johnson Creek') { LocationLabel <-  'Johnson Creek: Johnson Creek RST'}
  if(RST == 'Lolo Creek') { LocationLabel <-  'Lolo Creek: Lolo Creek RST'}
  if(RST == 'Newsome Creek') { LocationLabel <-  'Newsome Creek: Newsome Creek RST'}
  if(RST == 'Secesh River') { LocationLabel <-  'Secesh River: Lower Secesh River RST'}
  if(RST == 'South Fork Clearwater River') { LocationLabel <-  'South Fork Clearwater River: SF Clearwater River RST'}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getjuvabundancedata')

  # ActivityID
  queryList <- list(SpeciesRun = SpeciesRun,
                    MigratoryYear = MigratoryYear,
                    BroodYear = BroodYear,
                    Origin = Origin,
                    LocationLabel = LocationLabel)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query Juvenile Abundance estimates from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
