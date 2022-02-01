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

getP4data <- function(MRRProject = c('All', 'CDR', 'JLV', 'IMN', 'NPC', 'SCS'),
                      EventSite = c('All', 'IMNTRP', 'JOHTRP', 'LOLTRP', 'SECTRP', 'SFCTRP'),
                      EventType = c('All', 'Recapture', 'Mark', 'Recovery', 'Tally', 'Passive Recapture'),
                      SRRcode = NULL,
                      MigrationYear = NULL,
                      BroodYear = NULL,
                      CalendarYear = NULL,
                      cdms_host = 'https://npt-cdms.nezperce.org'){

  MRRProject <- match.arg(MRRProject)
  EventSite <- match.arg(EventSite)
  EventType <- match.arg(EventType)

  # errors
  if(!is.null(MigrationYear)) {
    if(!grepl('\\d{4}', MigrationYear))stop("MigrationYear must be a 4-digit year (YYYY).")
  }

  if(!is.null(BroodYear)) {
    if(!grepl('\\d{4}', BroodYear))stop("BroodYear must be a 4-digit year (YYYY).")
  }

  if(!is.null(CalendarYear)) {
    if(!grepl('\\d{4}', CalendarYear))stop("CalendarYear must be a 4-digit year (YYYY).")
  }

  # set NULLs
  if(MRRProject == 'All') { MRRProject <- NULL }
  if(EventSite == 'All') { EventSite <- NULL }
  if(EventType == 'All') { EventType <- NULL }


  # build URL for API
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getp4data')

  queryList <- list(MRRProject = MRRProject,
                    EventSite = EventSite,
                    EventType = EventType,
                    SRRcode = SRRcode,
                    MigrationYear = MigrationYear,
                    BroodYear = BroodYear,
                    CalendarYear = CalendarYear)


  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)

  httr::stop_for_status(req,
                        task = paste0('query p4 records from CDMS'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
