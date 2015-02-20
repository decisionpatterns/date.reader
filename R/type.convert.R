#' type.convert
#' 
#' Coerces a character vector to POSIXct if possible. An extension to 
#' \code{utils::type.convert}.
#' 
#' @param x character; vector to be coerces to POSIXct if possible 
#' 
#' @param ... list; extra arguments (from utils::type.convert, not used)
#'
#' @details
#' If the input has a recognized date format, then it is parsed and converted 
#' to POSIXct. Otherwise, the input is passed to 
#' \code{utils::type.convert}.
#'
#' @return a vector of parsed values. If values can be interpreted as dates, 
#' returns a POSIXct vector otherwise, the values returned from 
#' \code{utils::type.convert}
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   \code{\link[utils]{type.convert}}
#' 
#' @examples 
#'     
#'   type.convert( "2014-08-05" )
#'   type.convert( c("2014-08-05", "2014-08-12") )   
#'
#' @note this function will only attempt to convert a vector to POSIXct if every
#' item in the vector has the same format.
#' 
#' @export

type.convert <- function(x, ...) {
  
  orders <- which.orders(x, ...)

  if ( is.na(orders) ) {
    return( utils::type.convert(x, ...) )
  }
  tz <- get_option( date.reader$tz, 'UTC' )
  
  return( lubridate::parse_date_time(x, orders, tz=tz) )

}
