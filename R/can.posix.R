#' can.posix
#' 
#' @param x vector of character; values to parse
#'
#' @param nTrials integer; number of strings to check before deciding
#' the format
#' 
#' @param nErrors integer; the number of unparsable strings to allow. 
#' If the number of unparseable strings exceeds nErrors, return NA,
#' which means that the strings are either not dates, or not a consistent
#' format.
#'
#' @return logical. If the values can be interpreted as dates (with
#' the same format), it returns TRUE. It returns false if they cannot 
#' be interpreted as dates, or if there is no consistent format.
#'
#'   
#' @export

can.posix <- function(x, nTrials=kTrials, nErrors=kErrors) {
  fmt <- which.format(x, nTrials=kTrials, nErrors=kErrors)
  return (! is.na(fmt))
}
