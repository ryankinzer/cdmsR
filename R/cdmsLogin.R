#' @title cdmsLogin: Login to CDMS.
#'
#' @description The function passes username and password creditianls to the
#'   specified Centralized Data Management System (CDMS). After being
#'   successfull logged in a message will display in the console stating the
#'   full name of the user and a cookie will be stored for all future api calls
#'   for data retrieval. The cookie expires in 24-hours and requires
#'   \code{cdmsLogin()} to be re-run to gain access.
#'
#' @param username CDMS username.
#'
#' @param api_key provided through the user preferences page of the CDMS user-interface.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @examples cdmsLogin(username = 'username', api_key = 'key', cdms_host = 'host')
#'
#' @import keyring
#'
#' @export
#'
#' @return NULL
#'
cdmsLogin <- function(username = NULL, api_key = NULL, cdms_host = NULL){

  refresh <- !is.null(username) && !is.null(api_key) && !is.null(cdms_host)
  stored <- is.null(username) && is.null(api_key) && is.null(cdms_host)

  # provide all or none
  stopifnot("Values for username, api_key, and cdms_host must all be either provided or left NULL." = refresh | stored)

  if(refresh) {
    keyring::key_set_with_value(service = 'username', password = username)
    keyring::key_set_with_value(service = 'api_key', password = api_key)
    keyring::key_set_with_value(service = 'cdms_host', password = cdms_host)
    }

  if(stored) {
    # throw error if credentials not present
    stopifnot("No CDMS credentials are currently stored. Please provide username, api_key, and cdms_host." = c('api_key', 'username', 'cdms_host') %in% keyring::key_list()$service)

    username <- keyring::key_get(service = 'username')
    api_key <- keyring::key_get(service = 'api_key')
    cdms_host <- keyring::key_get(service = 'cdms_host')
  }

  # create temporary file for current session
  .x <- charToRaw(cdms_host)
  save(.x, file = file.path(tempdir(), 'chtmp.rda'))

  req_url <- paste0(cdms_host, '/services/api/v1/account/login')

  creds <- jsonlite::toJSON(list(Username = username, Password = api_key), auto_unbox = T)

  auth <- httr::POST(req_url, httr::add_headers(prefer = "respond-async"), httr::content_type_json(), body = creds)

  #warn_for_status(r)
  #stop_for_status(auth, task = paste0('login to ', cdms_host))
  #user_info <- httr::content(auth, "parsed", "application/json", encoding = "UTF-8")[[3]]
  user_info <- httr::content(auth, "parsed", encoding = "UTF-8")[[3]]
  s_code <- auth$status_code

  if(s_code == 200){
    cat(paste0('Logged in as: ', user_info$Fullname,'\n'))
  }

  return(auth)

}
