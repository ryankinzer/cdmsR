#' @title get_JuvSurvival:
#'
#' @description Retrieve juvenile survival estimates to Lower Granite Dam from CDMS.
#'
#' @param rst desired Rotary Screw Trap. Defaults to all traps.
#'
#' @param species_run filter for specfic species and run of fish. NULL returns all species and runs.
#'
#' @param migratory_year four digit year filter (YYYY) on Migratory Year. NULL returns all years.
#'
#' @param brood_year four digit year filter (YYYY) on Brood Year. NULL returns all years.
#'
#' @param origin desired origin of fish. Defaults to all origins.
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_JuvSurvival <- function(rst = c('All', 'Imnaha River', 'Johnson Creek', 'Lolo Creek', 'Newsome Creek', 'Secesh River', 'South Fork Clearwater River'),
                            species_run = NULL,
                            migratory_year = NULL,
                            brood_year = NULL,
                            origin = c('All', 'Hatchery', 'Natural')){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  rst <- match.arg(rst)
  origin <- match.arg(origin)

  if(origin == 'All') { origin <- NULL}

  # generate location label
  if(rst == 'All') { LocationLabel <- NULL}
  if(rst == 'Imnaha River') { LocationLabel <- 'Imnaha River: Imnaha River RST'}
  if(rst == 'Johnson Creek') { LocationLabel <-  'Johnson Creek: Johnson Creek RST'}
  if(rst == 'Lolo Creek') { LocationLabel <-  'Lolo Creek: Lolo Creek RST'}
  if(rst == 'Newsome Creek') { LocationLabel <-  'Newsome Creek: Newsome Creek RST'}
  if(rst == 'Secesh River') { LocationLabel <-  'Secesh River: Lower Secesh River RST'}
  if(rst == 'South Fork Clearwater River') { LocationLabel <-  'South Fork Clearwater River: SF Clearwater River RST'}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getjuvsurvivaldata')

  # ActivityID
  queryList <- list(SpeciesRun = species_run,
                    MigratoryYear = migratory_year,
                    BroodYear = brood_year,
                    Origin = origin,
                    LocationLabel = LocationLabel)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query Juvenile Survival estimates from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
