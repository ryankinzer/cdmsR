#' @title get_HeaderRecords:
#'
#' @description get all header records for a given dataset.
#'
#' @param dataset_id CDMS Dataset ID. see cdmsR::get_ProjectDatasets()
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL

get_HeaderRecords <- function(dataset_id){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/dataset/getheadersdatafordataset')

  # ActivityID
  queryList <- list(id = dataset_id)

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query header records for dataset_id = ', dataset_id, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  df <- dplyr::select(df, HeaderID = Id, ActivityID = ActivityId, dplyr::everything())

  return(df)

}
