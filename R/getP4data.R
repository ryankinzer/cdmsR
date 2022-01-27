#' @title getP4data()
#'
#' @description Retrieve p4 data (from CDMS) for all sites or a single site.
#'
#' @param migration_year Desired migration year (YYYY)
#'
#' @param event_site Desired event site. Leave NULL to retrieve all sites.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

getP4data <- function(migration_year,  event_site = c('All', 'IMNTRP', 'JOHTRP', 'LOLTRP', 'SECTRP', 'SFCTRP'),
                          cdms_host = 'https://npt-cdms.nezperce.org'){

  # Throw errors
  {if(is.null(migration_year))stop("migration_year must be provided (YYYY)")}

  if(event_site == 'All') { event_site <- NULL }

  # build URL for API
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getp4data')
  queryList <- list(Year = migration_year,
                    EventSite = event_site)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)

  httr::stop_for_status(req,
                        task = paste0('query ', event_site, 'p4 records from migration year ', migration_year, '.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(final_df)

}
