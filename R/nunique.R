#' Find Length of Unique Terms in Vector
#'
#' Find Length of Unique Terms in Vector.
#'
#' @param x vector
#' @export
#' @examples nunique(c("A", "A", "B", "C")) # should = 3
nunique <- function (x) {
    length(unique(x))
}

