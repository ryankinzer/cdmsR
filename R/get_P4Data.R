#' @title get_P4Data:
#'
#' @description Retrieve p4 data from CDMS (e.g. Rotary Screw Trapping, Beach Seine, Hook and Line).
#'
#' @param mrr_project PTAGIS Coordinator Code. NULL returns all.
#'
#' @param event_site Desired event site. NULL returns all sites.
#'
#' @param event_type Desired event type. NULL returns all event types.
#'
#' @param capture_method Desired capture method. NULL returns all methods.
#'
#' @param srr_code Desired species/run/rear code. NULL returns all species.
#'
#' @param migration_year Desired migration year (YYYY) NULL returns all years
#'
#' @param brood_year  Desired brood Year (YYYY). NULL returns all years
#'
#' @param calendar_year Desired calendar year (YYYY) NULL returns all years.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_P4Data <- function(mrr_project = c('All', 'CDR', 'JLV', 'IMN', 'NPC', 'SCS'),
                       event_site = c('All', 'JOHTRP', 'SECTRP', 'LSFTRP', 'JOHNSC',
                                     'MCCA', 'IMNTRP', 'NPTH', 'SFCTRP', 'NEWSOC',
                                     'CLWRSF', 'LOLOC', 'LOLTRP'),
                       event_type = c('All', 'Recapture', 'Mark', 'Recovery', 'Tally', 'Passive Recapture'),
                       capture_method = c('All', 'SCREWT', 'FYKNET', 'BSEINE', 'DIPNET', 'SHOCK'),
                       srr_code = NULL,
                       migration_year = NULL,
                       brood_year = NULL,
                       calendar_year = NULL){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  mrr_project <- match.arg(mrr_project)
  event_site <- match.arg(event_site)
  event_type <- match.arg(event_type)
  capture_method <- match.arg(capture_method)

  # errors
  if(!is.null(migration_year)) {
    if(!grepl('\\d{4}', migration_year))stop("migration_year must be a 4-digit year (YYYY).")
  }

  if(!is.null(brood_year)) {
    if(!grepl('\\d{4}', brood_year))stop("brood_year must be a 4-digit year (YYYY).")
  }

  if(!is.null(calendar_year)) {
    if(!grepl('\\d{4}', calendar_year))stop("calendar_year must be a 4-digit year (YYYY).")
  }

  # set NULLs
  if(mrr_project == 'All') { mrr_project <- NULL }
  if(event_site == 'All') { event_site <- NULL }
  if(event_type == 'All') { event_type <- NULL }
  if(capture_method == 'All') { capture_method <- NULL }

  # build URL for API
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getp4data')

  queryList <- list(MRRProject = mrr_project,
                    EventSite = event_site,
                    EventType = event_type,
                    CaptureMethod = capture_method,
                    SRRcode = srr_code,
                    MigrationYear = migration_year,
                    BroodYear = brood_year,
                    CalendarYear = calendar_year)


  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)

  httr::stop_for_status(req,
                        task = paste0('query P4 data from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
