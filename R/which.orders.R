#' which.orders 
#' 
#' detecting the date 'orders' of character vector  
#'
#' @param x character; values to convert to a POSIX date
#'
#' @param orders character; orders that can be returned
#'
#' @param autostart integer; number of elements of \code{x} to check 
#' to determine the orders of \code{x}
#'
#' @param nErrors numeric; A non-negative number:  
#' if >= 1, the number of unparsable strings to allow
#' if <  1, the fraction of values that are allowed to be unparsable before 
#' giving up.
#'
#' @param force logical; TRUE means try to interpret as date, even if number
#' of strings is less than autostart. This is only relevant for the all-digits
#' format: 20141222; we don't want to consider this a date unless there are
#' sufficient data, or if force == TRUE.
#'
#' @param ... list; extra parameters (unused)
#'
#' @description 
#' 
#' If the number of unparseable strings exceeds nErrors, return NA,
#' which means that the strings are either not dates, or not a consistent
#' format.
#' 
#' @return 
#'   character, name for the date orders, e.g. mdy, ymd_hm, etc.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[lubridate]{guess_formats}} \cr
#'   
#' @examples 
#'   # Same order, same format
#'   x <- c("January 11, 2014", "February 15, 1958", "March 3, 1969")
#'   which.orders(x)
#'  
#'   # Same order, different formats  
#'   x <- c("January 11, 2014", "February 15, 1958", "3/3/1969")
#'   which.orders(x, autostart=3, nErrors=1)
#'           
#'   which.orders( 20150101:20150131 )
#'   which.orders( 20150101:20150131, autostart = 50  )
#'   which.orders( 2015001:2015365 )   # NA
#'                
#' @import options    
#' @importFrom lubridate guess_formats
#' @export
  
which.orders <- function( 
    x
  , orders    = get_option( date.reader$orders, all.orders )
  , autostart = get_option( date.reader$autostart, 30 )
  , nErrors   = get_option( date.reader$nErrors, 0 )
  , force     = FALSE
  , ...
) {
  
  # Initialization
  autostart.actual <-  min(length(x), autostart)
  
  nErrors          <- 
    if (nErrors < 1)  # a fraction
     nErrors * autostart.actual else 
     round( nErrors*autostart.actual/autostart )
  

  # Autostart subset
  indices <- round( seq(from=1, to=length(x), length.out=autostart.actual) )
  z <- x[indices]
  
  formats <- lubridate::guess_formats(z, orders)
  if (is.null(formats)) {
    return(NA)
  }
  orders <- unique(names(formats))

  get_errors <- function(ord) {
    z1 <- .parse.date.strict(z, ord)
    sum(is.na(z1))
  }
  
  suppressWarnings( 
    errs <- lapply(orders, get_errors)
  )
  index <- which.min(errs)
  orders <- orders[[index]]
  
  if (! force) {
    # SPECIAL CASE: if all digits, don't assume it a date unless there is
    # sufficient data, or if force id TRUE
    all.digits <- all( ! grepl("\\D", z) )
    if (all.digits) {
      if (autostart.actual < autostart) return(NA)
    }
    nErrors.actual <- errs[[index]]
    if (nErrors.actual > nErrors) return(NA)
    
  }
  return(orders)

}
