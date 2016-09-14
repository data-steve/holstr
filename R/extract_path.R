#' Extract Parts of a Path
#'
#' Extract portions of a path.
#'
#' @param x A vector of path(s).
#' @param n Corresponds to n in \code{\link[utils]{head}} & \code{\link[utils]{tail}}.
#' Gives 1 to n of the beginning (\code{fun = head}) or end (\code{fun = tail}) of string.
#' Note this is over rode if \code{inds} is supplied.
#' @param fun Either the \code{\link[utils]{head}} or \code{\link[utils]{tail}} function.
#' @param inds An index of elements of the path to use (for when you don't want to include
#' beginning or end of the path).
#' @return Returns a partial path(s).
#' @keywords path
#' @export
#' @examples
#' extract_path(x)
#' extract_path(x, 2)
#' extract_path(x, -2)
#' extract_path(x, fun=tail)
#' extract_path(x, 2, fun=tail)
#' extract_path(x, inds=2:3)
#' x <- c(
#'     "C:/Users/trinker/Desktop/commencement_project",
#'     "C:\\Users\\trinker\\Desktop\\commencement_project"
#' )
extract_path <- function(x, n = 1, fun = head, inds = NULL){
  y <- strsplit(x, "\\\\|/")
  if (is.null(inds)){
    y <- lapply(y, function(z) match.fun(fun)(z, n=n))
  } else {
    y <- lapply(y, function(z) z[inds])
  }
  sapply(y, paste, collapse="/")
}
