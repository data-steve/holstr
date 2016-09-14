#' Do GREP and MATCH in One Function
#'
#' Do GREP and MATCH in One Function.
#'
#' @param sequ vector of greps
#' @param dta list or vector to grep and match on
#' @export
#' @examples
#' grep_match(c("^a", "^d", "^b"), c("house", "bird", "dog", "apple"))
grep_match <- function(sequ, dta, sort=FALSE) {
  regg  <- paste0("grep('"
                  , paste(sequ, collapse="|")
                  , "', c("
                  , paste( shQuote(dta),collapse=',')
                  , "), value = TRUE)")
  prs <- eval(parse(text=regg))
  if (sort){
    unlist(sapply(sequ, function(x) sort(prs[grep(x,prs)]), USE.NAMES = FALSE))
  } else {
    unlist(sapply(sequ, function(x) prs[grep(x,prs)], USE.NAMES = FALSE))
  }
}


