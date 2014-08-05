#' as.POSIXct.character
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' 
#' @param fmt character; name of the format for the values. If
#' this parameter is NA, then the function will try to guess the format
#' for each value
#'
#' @return 
#'   POSIXct vector (or NA, if a value cannot be parsed)
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'   # -tk
#'  dts <- c( '20140210', '19791118', '19720329' ) 
#'  as.POSIXct.character( dts )  
#'  dts <- rep( '20140210', 100 )
#'  dts <- c( '14-02-10', '79-11-18' ) # FAIL(?)
#'  as.POSIXct.character( dts )  
#'    
#' @export

as.POSIXct.character <- function( x, fmt=NA ) {
  
  f <- function(txt) {
    if ( is.na(fmt) ) {
      fmt <- which.format(txt)
    }
    if ( is.na(fmt) ) {
      return(NA)
    }
    return( .parse.date(txt, fmt) )
  }
  
  result <- lapply(x,f)
  result <- do.call(c,result)
  
  return(result)
  
}
