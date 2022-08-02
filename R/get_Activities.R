#' @title get_Activities:
#'
#' @description get all activities in CDMS.
#'
#' @param dataset_id CDMS Dataset ID. see cdmsR::get_ProjectDatasets()
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL

get_Activities <- function(dataset_id){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/activity/getdatasetactivitiesview')

  # ActivityID
  queryList <- list(id = dataset_id)

  #httr::modify_url(req_url, query = queryList)
  # Should be:
  # https://npt-cdms.nezperce.org/services/api/v1/activity/getdatasetactivitiesview?id=4335

  # GET request with query parameters
  req <- httr::GET(req_url,
                        query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query activity records for dataset_id = ', dataset_id, ' from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  df <- dplyr::select(df, ActivityID = Id, dplyr::everything())

  return(df)

}
