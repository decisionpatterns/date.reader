#' as.POSIXct.character
#' 
#' Convers character to POSIXct
#' 
#' @param x character; vector to convert to a POSIX date
#' 
#' @param format character; name of the format for the values. If
#' this parameter is NA, then the function will try to guess the format
#' for each value
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
#'  as.POSIXct( dts, format='ymd' )
#'    
#' @export

as.POSIXct.character <- function( x, format=NA ) {
  
  f <- function(txt) {
    if ( is.na(format) ) {
      format <- which.format(txt)
    }
    if ( is.na(format) ) {
      return(NA)
    }
    return( .parse.date(txt, format) )
  }
  
  result <- lapply(x,f)
  result <- do.call(c,result)
  
  return(result)
  
}
