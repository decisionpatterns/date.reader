#' type.convert
#' 
#' @param x vector of character; values to parse
#'
#' @return vector of parsed values. If the values
#' can be interpreted as dates, it returns a vector
#' of POSIXct objects. Otherwise, it calls utils::type.convert
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @export

type.convert <- function(x, ...) {
  fmt <- which.format(x, nTrials=kTrials, nErrors=kErrors)
  if (is.na(fmt)) {
    return(utils::type.convert(x, ...))
  }
  return(as.POSIXct.character(x, fmt=fmt))
}
