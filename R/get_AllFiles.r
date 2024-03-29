#' @title get_AllFiles:
#'
#' @description gets all files stored in CDMS.
#'
#' @param cdms_host the web URL for the targeted CDMS user-interface page.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_AllFiles <- function(cdms_host = 'https://npt-cdms.nezperce.org/'){

  projects <- get_Projects(cdms_host=cdms_host) %>% pull(Id)

  all_files <- map_dfr(.x = projects,
                       .f = function(.x){

                         tmp <- get_ProjectFiles(ProjectId = .x, cdms_host = cdms_host)

                         if(class(tmp) == "data.frame") {

                           return(tmp)

                         } else {

                           return(NULL)
                         }
                       })

  return(all_files)
}
