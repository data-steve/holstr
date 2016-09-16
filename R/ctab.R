#' List Table to Matrix Table
#'
#' List Table to Matrix Table.
#'
#' @param x list table to matrix table
#' @export
#' @examples
#' vec <- c("A", "A", "B", "C")
#' cv  <- sapply(unique(vec), function(x) length(vec[vec==x]))
#' ctab(cv)
ctab <- function (x) {
    cbind(values = unlist(x))
}

