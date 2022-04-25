#' @title get_FINSvalues:
#'
#' @description helper function to provide acceptable values for FINS data queries:
#' get_WeirData() and get_SpawningData()
#'
#' @param module desired FINS module: Trapping or Spawning.
#'
#' @param field desired field chosen to display possible values.
#'
#' @author Tyler Stright, Ryan Kinzer
#'
#' @export
#'
#' @return NULL

get_FINSvalues <- function(module = c('Trapping', 'Spawning'),
                           field = c('Facility', 'Spawning Location', 'Species', 'Origin', 'Stock')){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  # check values
  module <- match.arg(module)
  field <- match.arg(field)

  # throw errors
  {if(module == 'Trapping' & field %in% c('Spawning Location', 'Stock'))stop('"Spawning Location" and "Stock" are only associated with the FINS Spawning module.')}
  {if(module == 'Spawning' & field == 'Facility')stop('"Facility" is only associated with the FINS Trapping module.')}

  # set table value.
  if(module == 'Trapping') { table <- 'AdultWeir_Data_kus'}
  if(module == 'Spawning') { table <- 'AdultSpawning_Data_kus'}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfinsvalues')

  # ActivityID
  queryList <- list(Table = table,
                    Field = field)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query FINS ', module, ' module field values.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
