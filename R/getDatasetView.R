#' @title getDatasetView:
#'
#' @description
#'
#' @param datastoreID for CDMS dataset.
#'
#' @param MPG
#'
#' @param POP
#'
#' @param StreamName
#'
#' @param SurveyYear
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

getDatasetView <- function(datastoreID, MPG = NULL, POP = NULL, StreamName = NULL, SurveyYear = NULL, cdms_host = 'https://cdms.nptfisheries.org'){
  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/dataset/getfulldatasetview')

  # ActivityID
  queryList <- list(id = datastoreID,
                    MPG = MPG,
                    POP = POP,
                    StreamName = StreamName,
                    SurveyYear = SurveyYear)

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
