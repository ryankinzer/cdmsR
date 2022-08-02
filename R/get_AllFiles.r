#' @title get_AllFiles:
#'
#' @description gets all files stored in CDMS.
#'
#' @author Ryan Kinzer, Tyler Stright
#'
#' @export
#'
#' @return NULL
#'
get_AllFiles <- function(){

  load(file = file.path(tempdir(), 'chtmp.rda'))
  cdms_host <- rawToChar(.x)

  projects <- get_Projects() %>% pull(Id)

  all_files <- map_dfr(.x = projects,
                       .f = function(.x){

                         tmp <- get_ProjectFiles(project_id = .x)

                         if(class(tmp) == "data.frame") {

                           return(tmp)

                         } else {

                           return(NULL)
                         }
                       })

  return(all_files)
}
