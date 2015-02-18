#' can.posix
#' 
#' Determine if an object can be coerced to a POSIXct vector 
#' 
#' @param x character; values to parse
#'
#' @param autostart integer; number of strings to check before deciding
#' the format
#' 
#' @param nErrors integer; the number of unparsable strings to allow. 
#' If the number of unparseable strings exceeds nErrors, return NA,
#' which means that the strings are either not dates, or not a consistent
#' format.
#'
#'
#' @return logical. If the values can be interpreted as dates (with
#' the same format), it returns TRUE. It returns false if they cannot 
#' be interpreted as dates, or if there is no consistent format.
#'
#' @seealso 
#'   \code{\link{which.orders}} \cr
#'   \code{canCoerce} from the methods package for similar methods
#'   
#' @examples 
#'   can.posix("12-11-10")
#'   can.posix("13-13-13")
#'      
#' @include which.orders.R
#'         
#' @export

can.posix <- function( 
    x
  , autostart = options::get_option( date.reader$autostart, length(x) )
  , nErrors   = options::get_option( date.reader$nErrors, 0 )
) {
  
  if( autostart > length(x) ) autostart <- length(x)
    
  orders <- which.orders( x, autostart=autostart, nErrors=nErrors, force=TRUE )

  return ( ! is.na(orders) )
  
}
