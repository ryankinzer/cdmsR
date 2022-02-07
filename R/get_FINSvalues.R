#' @title get_FINSvalues:
#'
#' @description helper function to provide acceptable values for FINS data queries:
#' getFINSweirData() and getFINSspawningData().
#'
#' @param Module desired FINS module: Trapping or Spawning.
#'
#' @param Field desired field chosen to display possible values.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Tyler Stright
#'
#' @export
#'
#' @return NULL

get_FINSvalues <- function(Module = c('Trapping', 'Spawning'),
                           Field = c('Facility', 'Spawning Location', 'Species', 'Origin', 'Stock'),
                           cdms_host = 'https://npt-cdms.nezperce.org'){

  # check values
  Module <- match.arg(Module)
  Field <- match.arg(Field)

  # throw errors
  {if(Module == 'Trapping' & Field %in% c('Spawning Location', 'Stock'))stop('"Spawning Location" and "Stock" are only associated with the FINS Spawning module.')}
  {if(Module == 'Spawning' & Field == 'Facility')stop('"Facility" is only associated with the FINS Trapping module.')}

  # set table value.
  if(Module == 'Trapping') { table <- 'AdultWeir_Data_kus'}
  if(Module == 'Spawning') { table <- 'AdultSpawning_Data_kus'}

  # detail url
  req_url <- paste0(cdms_host,'/services/api/v1/npt/getfinsvalues')

  # ActivityID
  queryList <- list(Table = table,
                    Field = Field)

  # httr::modify_url(req_url, query = queryList)

  # GET request with query parameters
  req <- httr::GET(req_url,
                   query = queryList)


  httr::stop_for_status(req,
                        task = paste0('query FINS module field values.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")
  df <- jsonlite::fromJSON(req_con, flatten = TRUE)

  return(df)

}
