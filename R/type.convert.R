#' type.convert
#' 
#' @param x character; 
#' @param ... list; extra arguments (from utils::type.convert, not used)
#'
#' @return a vector of parsed values. If values
#' can be interpreted as dates, returns a POSIXct vector otherwise, the values 
#' from utils::type.convert
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#' 
#' @examples 
#'   x <- type.convert( c("2014-08-05" ) )
#'   x
#'   class(x)  # "POSIXct" "POSIXt" 
#'   
#' @export

type.convert <- function(x, ...) {
  
  format <- which.format(x)

  if ( is.na(format) ) {
    return( utils::type.convert(x, ...) )
  }
  
  return( string.to.POSIXct(x, format=format) )

}
