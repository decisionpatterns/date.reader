#' type.convert
#' 
#' @param x character; 
#'
#' @return a vector of parsed values. If values
#' can be interpreted as dates, returns a POSIXct vector otherwise, the values 
#' from utils::type.convert
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#' 
#' @examples 
#'   x <- type.convert( c("2014-08-05" ) )
#'   x
#'   class(x)  # "POSIXct" "POSIXt" 
#'   
#' @export

type.convert <- function(x, ...) {
  
  format <- which.format(x, nTrials=kTrials, nErrors=kErrors)
  
  if ( is.na(format) ) {
    return( utils::type.convert(x, ...) )
  }
  
  return( as.POSIXct.character(x, format=format, ...) )

}
