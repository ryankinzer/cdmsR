#' @title getProjectFiles:
#'
#' @description gets all files associated with a project.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
getProjectFiles <- function(ProjectId, cdms_host = 'https://npt-cdms.nezperce.org/'){

  # must login into CDMS to obtain cookie
  # requires httr, jsonlite packages

  #cdms_host <- match.arg(cdms_host)

  # copied from dbo.FileTypes
  filetypes_df <- data.frame(
    Id = c(1:5,8:9),
    FileType = c('Image', 'PDF', 'Word', 'Excel', 'Unknown', 'Text', 'CSV')
  )

  # project url
  req_url <- paste0(cdms_host,'/services/api/v1/file/getprojectfiles?id=',ProjectId)

  # GET request with query parameters
  req <- httr::GET(req_url)

  httr::stop_for_status(req,
                        task = paste0('query project files from CDMS.'))

  # parse the response
  req_con <- httr::content(req, type = 'text', encoding = "UTF-8")

  df <- jsonlite::fromJSON(req_con)

  if(class(df) == "data.frame") {

    userinfo <- df[,2] %>%
      select(Id, Fullname) %>%
      distinct()

    df <- df[,-c(1,2)]

    df <- df %>%
      left_join(userinfo, by= c('UserId'='Id')) %>%
      left_join(filetypes_df, by = c('FileTypeId'= 'Id'))

    return(df)

  } else {

    return(NULL)
  }
}

