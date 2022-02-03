#' @title get_P4Data
#'
#' @description Retrieve p4 data (from CDMS) for all sites or a single site.
#'
#' @param MRRProject PTAGIS Coordinator Code. NULL returns all.
#'
#' @param EventSite Desired event site. NULL returns all sites.
#'
#' @param EventType Desired event type. NULL returns all event types.
#'
#' @param CaptureMethod Desired capture method. NULL returns all methods.
#'
#' @param SRRcode Desired species/run/rear code. NULL returns all species.
#'
#' @param MigrationYear Desired migration year (YYYY) NULL returns all years
#'
#' @param BroodYear  Desired brood Year (YYYY). NULL returns all years
#'
#' @param CalendarYear Desired calendar year (YYYY) NULL returns all years.
#'
#'#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_P4Data <- function(MRRProject = c('All', 'CDR', 'JLV', 'IMN', 'NPC', 'SCS'),
                      EventSite = c('All', 'JOHTRP', 'SECTRP', 'LSFTRP', 'JOHNSC',
                                    'MCCA", "IMNTRP', 'NPTH", "SFCTRP', 'NEWSOC',
                                    'CLWRSF', 'LOLOC', 'LOLTRP'),
                      EventType = c('All', 'Recapture', 'Mark', 'Recovery', 'Tally', 'Passive Recapture'),
                      CaptureMethod = c('All', 'SCREWT', 'FYKNET', 'BSEINE', 'DIPNET', 'SHOCK'),
                      SRRcode = NULL,
                      MigrationYear = NULL,
                      BroodYear = NULL,
                      CalendarYear = NULL,
                      cdms_host = 'https://npt-cdms.nezperce.org'){

  MRRProject <- match.arg(MRRProject)
  EventSite <- match.arg(EventSite)
  EventType <- match.arg(EventType)
  CaptureMethod <- match.arg(CaptureMethod)

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
  if(CaptureMethod == 'All') { CaptureMethod <- NULL }

  # build URL for API
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getp4data')

  queryList <- list(MRRProject = MRRProject,
                    EventSite = EventSite,
                    EventType = EventType,
                    CaptureMethod = CaptureMethod,
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
