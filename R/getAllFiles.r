#' @title getAllFiles:
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
getAllFiles <- function(cdms_host = 'https://npt-cdms.nezperce.org/'){

  projects <- getProjects(cdms_host=cdms_host) %>% pull(Id)

  all_files <- map_dfr(.x = projects,
                       .f = function(.x){

                         tmp <- getProjectFiles(ProjectId = .x, cdms_host = cdms_host)

                         if(class(tmp) == "data.frame") {

                           return(tmp)

                         } else {

                           return(NULL)
                         }
                       })

  return(all_files)
}
