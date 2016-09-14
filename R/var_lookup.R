#' Search Sub Folders
#'
#' Search subfolders for a file type with a particular column name and optionally validate.
#'
#' @param subfolder_paths A vector of directory paths to search in (from
#' \code{\link[base]{list.dirs}} or \code{get_subfolder_path}).
#' @param colname A column name to search for in the designated file type
#' (\code{pattern}).
#' @param fun A valiData validating function (optional).
#' @param pattern The file type to search for.
#' @param \ldots Additional arguments given to the \code{fun}.
#' @return Gives data.frame and info on matching criteria.
#' @export
#' @examples
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(magrittr, dplyr, cl)
#'
#' ## get csv files that have "AcademicStanding" header
#' potential_student_files <- cl::l_drive_go("/FTP") %>%
#'     list.dirs() %>%
#'     assign("dirs", ., .GlobalEnv) %>%
#'     ## get_subfolder_path("MyFolder") %>%
#'     cl::search_subfolders("AcademicStanding")
#'
#' ## filter results
#' potential_student_files %>%
#'     filter(perc_non_miss > 0) %>%
#'     print.data.frame()
#' }
search_subfolders <- function(subfolder_paths, colname, fun=NULL, pattern = ".csv$", ...){

  csv_list <- unlist(lapply(subfolder_paths, list.files, pattern = pattern, ignore.case=TRUE, full.names = TRUE) )

  file_info <- lapply(csv_list
                      , function(x){y <- suppressWarnings(readr::read_csv(x, n_max=100))
                      # does it have colname
                      # if(any(grepl('^Q97', colnames(y)))) browser()
                      has_colname_vec <- tolower(gsub("[^[:print:]]", "", names(y))) %in% tolower(colname)

                      if (any(has_colname_vec)){
                        # perc not missing
                        perc = valiData::vc_non_response(y[,has_colname_vec, drop=FALSE][[1]])[["proportion"]]
                        #perc = sum(complete.cases(y[,has_colname_vec, drop=FALSE]))/nrow(y)
                        #if (length(perc)==0){
                        # perc = 0
                        #}
                      } else {
                        perc = 0
                      }

                      # do non-missing follow pattern
                      validates <- NA
                      if(any(has_colname_vec) & perc>0 & !is.null(fun)) {
                        ## For Now (until printing is re-written for vc_ functions)
                        ## this was to handle the catting of messages:
                        tc <- textConnection(NULL, "w")
                        sink(tc)
                        validates <- match.fun(fun)(y[,has_colname_vec, drop=FALSE][[1]],...)
                        sink()
                        close(tc)
                      }

                      #if(!is.na(validates) && validates) browser()

                      return(dplyr::data_frame(name = subfolder_paths[[1]]
                                               , file = x
                                               , has_col = any(has_colname_vec),
                                               perc_non_miss = perc, validates = validates))
                      }) %>%
    dplyr::bind_rows()

  if(all(is.na(file_info[["validates"]]))) file_info[["validates"]] <- NULL
  file_info

}





#' Extract Paths Within a Folder
#'
#' Extract All Paths Within a Folder
#'
#' @param listicle A list of paths from\code{\link[base]{list.dirs}}.
#' @param subfolder A subfolder name to search in.
#' @return Returns matching files (ignoring case) in subfolder.
#' @export
get_subfolder_path <- function(listicle, subfolder){

  listicle[grepl(paste0(subfolder,"$")
                 , listicle
                 , ignore.case = TRUE)
           ]
}
