#' as.POSIXct.character
#' 
#' Convers character to POSIXct
#' 
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
# @include which.format.R   
#' @method as.POSIXct character
#' @export

as.POSIXct.character <- function( x, tz=getOption("date.reader")$tz, ...) {
  fmt <- which.format(x)
  if ( is.na(fmt) ) return(NA)

  return( string.to.POSIXct(x, tz=tz, orders=fmt) )
}


#' string.to.POSIXct
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' 
#' @param orders character; name of the format (e.g. 'ymd' ). If this parameter 
#' is \code{NA}, then the function will try to guess the format. The possible 
#' values for orders is the same as in \code{\link[lubridate]{parse_date_time}}
#' 
#' @param tz character; optional time zone
#'
#' @details 
#'   - tk
#' 
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[base]{strptime}} for \code{format} strings \cr
#'   \code{\link[lubridate]{parse_date_time}} for \code{orders} strings
#'   
#' @examples 
#'  
#'   dts <- c( '20140210', '19791118', '19720329' )  
#'   date.reader:::string.to.POSIXct( dts, NULL) 
#'      
#'   dts <- rep( '20140210', 10 )
#'   date.reader:::string.to.POSIXct( dts, NULL )  
#'   
#'   dts <- c( '14-02-10', '79-11-18' ) # FAIL(?)
#'   date.reader:::string.to.POSIXct( dts, NULL )  
#'  
#'   date.reader:::string.to.POSIXct( dts, 'ymd' )
#' 
#' @rdname as.POSIXct.R
#' @include which.format.R parse.date.R

string.to.POSIXct <- function( x, orders, tz=getOption("date.reader")$tz ) {
 
  check.regex <- if ( is.null(orders) ) TRUE else FALSE 

  f <- function(txt) {
    if ( is.null(orders) ) {
      orders <- which.format(txt)
    }
    if ( is.null(orders) ) {
      return(NA)
    }
    ret <- .parse.date(txt, orders, tz=tz, check.regex=check.regex)
    return(ret)
  }
  
  x <- as.character(x)
  ret <- lapply(x,f)
  ret <- do.call(c,ret)
  
  return(ret)
  
}
