#' can.posix
#' 
#' @param x vector of character; values to parse
#'
#' @return vector of logical. If the values
#' can be interpreted as dates, it returns TRUE
#'
#'   
#' @export

can.posix <- function(x, nTrials=kTrials, nErrors=kErrors) {
  fmt <- which.format(x, nTrials=kTrials, nErrors=kErrors)
  return (! is.na(fmt))
}
