#' @title cdmsLogin: Login to CDMS.
#'
#' @description The function passes username and password creditianls to the
#'   specified Centralized Data Management System (CDMS). After being
#'   successfull logged in a message will display in the console stating the
#'   full name of the user and a cookie will be stored for all future api calls
#'   for data retrieval. The cookie expires in 24-hours and requires
#'   \code{cdmsLogin()} to be re-run to gain access.
#'
#' @param username for CDMS.
#'
#' @param api_key provided through the user preferences page of the CDMS user-interface.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer
#'
#' @examples cdmsLogin(username = 'username', api_key = 'key',
#'  cdms_host = 'https://npt-cdms.nezperce.org')
#'
#' @export
#'
#' @return NULL
#'
cdmsLogin <- function(username, api_key, cdms_host = 'https://npt-cdms.nezperce.org'){

  stopifnot(!is.null(username),
            !is.null(api_key))

  #password <- encription_stuff(password)

  req_url <- paste0(cdms_host, '/services/api/v1/account/login')
  #"https://npt-cdms.nezperce.org/services/api/v1/account/login"

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
