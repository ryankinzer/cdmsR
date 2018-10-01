#' @title getDatasetView:
#'
#' @description
#'
#' @param datastoreID for CDMS dataset.
#'
#' @param Species a character string of either Chinook salmon, Steelhead, or Bull trout, default Chinook salmon
#'
#' @param Run a character string of either Spring, Spring/summer, Summer, Fall, or Winter, default Spring/summer
#'
#' @param SurveyYear a single or two dimension vector consisting of the only year, single vector, or start year and last year of interest
#'
#' @param MPG
#'
#' @param POP
#'
#' @param StreamName
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples getDatasetView(datastoreID, cdms_host = 'https://cdms.nptfisheries.org')
#'
#' @import httr jsonlite
#' @export
#' @return NULL

getDatasetView <- function(datastoreID, Species = c('Chinook salmon', 'Steelhead', 'Bull trout', 'Coho salmon'),
                           Run = c('Spring', 'Spring/summer', 'Summer', 'Fall', 'Winter'),
                           SurveyYear = NULL,
                           MPG = NULL, POP = NULL, StreamName = NULL,
                           cdms_host = 'https://cdms.nptfisheries.org'){

  Species <- match.arg(Species)
  Run <- match.arg(Run)

  stopifnot(!is.null(SurveyYear))

  if(length(SurveyYear) == 1){
    SurveyYear <- c(SurveyYear,SurveyYear)
  }

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  Species <-  paste0("'",Species,"'")

  Run <- paste0("'",Run,"'")

  if(!is.null(MPG)){
    MPG <- paste0("'",MPG,"'")
  }

  if(!is.null(POP)){
    POP <- paste0("'",POP,"'")
  }

  if(!is.null(StreamName)){
    StreamName <- paste0("'",StreamName,"'")
  }


  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/dataset/getfulldatasetview')

  # ActivityID
  queryList <- list(id = datastoreID,
                    Species = Species,
                    Run = Run,
                    StartYear = SurveyYear[1],
                    EndYear = SurveyYear[2],
                    MPG = MPG,
                    POP = POP,
                    StreamName = StreamName)

  httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query all data records for datastoreID = ', datastoreID, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
