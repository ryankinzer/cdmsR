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

getDatasetView <- function(datastoreID, Species = NULL, Run = NULL,
                           MPG = NULL, POP = NULL, StreamName = NULL,
                           SurveyYear = NULL,
                           cdms_host = 'https://cdms.nptfisheries.org'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  if(!is.null(Species)){
    Species <-  paste0("'",Species,"'")
  }

  if(!is.null(Run)){
    Run <- paste0("'",Run,"'")
  }

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
                    MPG = MPG,
                    POP = POP,
                    StreamName = StreamName,
                    SurveyYear = SurveyYear)

  modify_url(req_url, query = queryList)

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
