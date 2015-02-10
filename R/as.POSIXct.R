#' as.POSIXct converts objects to POSIXct, if possible
#'
#' @title as.POSIXct The generic conversion function to POSIXct
#' @param x character; vector to convert to a POSIX date
#' @param tz character; optional time zone
#' @param ... list; other optional arguments (from as.POSIXct, ignored)
#'
#' @note 
#'   The default timezone matches UTC like \code{lubridate} and not 
#'   the default of \code{\link[base]{as.POSIXct}} from the base package.
#'
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[base]{strptime}} for \code{format} strings
#'   \code{\link[lubridate]{parse_date_time}} for \code{orders} strings
#'   
#' @examples 
#'   # -tk
#'  dts <- c( '20140210', '19791118', '19720329' )  
#'  as.POSIXct( dts ) 
#'      
#'  dts <- rep( '20140210', 10 )
#'  as.POSIXct( dts )  
#'   
#'  dts <- c( '14-02-10', '79-11-18' ) # FAIL(?)
#'  as.POSIXct( dts )
#'
#'  
# @include which.orders.R
#' @export 

as.POSIXct <- function( x, tz=getOption("date.reader.tz"), ... ) UseMethod('as.POSIXct')

#' @rdname as.POSIXct
#' @method as.POSIXct character
#' @export

as.POSIXct.character <- function( x, tz=getOption("date.reader.tz"), ... ) {
  orders <- which.orders(x, force=TRUE)
  if ( is.na(orders) ) return(NA)
  return( lubridate::parse_date_time(x, orders, tz=tz) )
}

#' @rdname as.POSIXct
#' @method as.POSIXct factor
#' @export
as.POSIXct.factor <- function( x, tz=getOption("date.reader.tz"), ...) {
  txt <- as.character(x)
  as.POSIXct.character(txt, tz=tz, ...)
}
