#' as.POSIXct.character
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' @param tz character; optional time zone
#' @param ... list; other optional arguments (from as.POSIXct, ignored)
#' 
#'
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[base]{strptime}} for \code{format} strings
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


as.POSIXct.character <- function( x, tz="UTC", ...) {
  fmt <- which.format(x)
  if (is.na(fmt)) {
    return(NA)
  }
  return(string.to.POSIXct(x, tz=tz, format=fmt))
}


#' string.to.POSIXct
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' 
#' @param format character; name of the format for the values. If
#' this parameter is NA, then the function will try to guess the format
#' for each value
#' @param tz character; optional time zone
#' 
#'
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[base]{strptime}} for \code{format} strings
#'   
#' @examples 
#' 
#' \dontrun{ 
#'   dts <- c( '20140210', '19791118', '19720329' )  
#'   string.to.POSIXct( dts ) 
#'      
#'   dts <- rep( '20140210', 10 )
#'   string.to.POSIXct( dts )  
#'   
#'   dts <- c( '14-02-10', '79-11-18' ) # FAIL(?)
#'   string.to.POSIXct( dts )  
#'  
#'   string.to.POSIXct( dts, format='ymd' )
#' } 
#' @rdname as.POSIXct.R
#' @include which.format.R parse.date.R

string.to.POSIXct <- function( x, format, tz=getOption("date.reader")$tz ) {
 
  check.regex <- if ( is.null(format) ) TRUE else FALSE 

  f <- function(txt) {
    if ( is.null(format) ) {
      format <- which.format(txt)
    }
    if ( is.null(format) ) {
      return(NA)
    }
    ret <- .parse.date(txt, format, tz=tz, check.regex=check.regex)
    return(ret)
  }
  
  x <- as.character(x)
  ret <- lapply(x,f)
  ret <- do.call(c,ret)
  
  return(ret)
  
}
