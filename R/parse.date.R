#' Internal functions for parsing date strings
#' 
#' @name parse.date
NULL

#' .parse.date.strict
#' 
#' @param txt character; value(s) to convert to a POSIX date
#' 
#' @param orders character; name of the order, e.g. "mdy"
#' orders are described in 
#'   \code{\link[lubridate]{parse_date_time}}. Each order string is 
#'   series of formatting characters as listed \code{\link[base]{strptime}} but
#'   without the "%" prefix, for example "ymd" will match all the possible 
#'   dates in year, month, day order. Formatting orders might include 
#'   arbitrary separators. These are discarded. See details for implemented 
#'   formats.
#'   
#' @param tz character; optional time zone
#'
#' @note Internal function, a wrapper for lubridate functions
#' 
#' @note If a vector input is given, the values that cannot be parsed
#' will be given the value NA
#'
#' @return 
#' POSIXct object representing a date (or datetime), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'      
#' @rdname parse.date

.parse.date.strict <- function( 
    txt
  , orders
  , tz=options::get_option( date.reader$tz, 'UTC' ) 
) {
  orders <- tolower(orders)
   
  fun <- function(x) {
    suppressWarnings(
      z <- lubridate::parse_date_time(x, orders, tz=tz))
    if (is.na(z)) return(NA)
    m <- lubridate::month(z)
    m1 <- .get_month_num(x)
    if (m1 > 0) {
      if (m1 != m) return(NA)
    }
    return(z)
  }
  do.call("c", lapply(txt, fun))
}

#' .parse.date.hetero
#' parses a vector of date strings. May be different formats
#'
#' @return 
#' POSIXct object representing a date (or datetime), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'    date.reader:::.parse.date.hetero(
#'      c("January 11, 2014", "2014/02/16", "2014-08-05", "20140805", "14/08/05")
#'    )
#' @note Internal function, a wrapper for lubridate functions
#' @note "orders" argument here is a vector of orders to choose from
#'  
#' @rdname parse.date
#' @import options

.parse.date.hetero <- function(
    txt
  , orders = all.orders
  , tz = options::get_option( date.reader$tz, 'UTC' ) 
) {
  fun <- function(x) {
    ord <- which.orders(x, orders=orders)
    .parse.date.strict(x, ord, tz=tz)
  }
  do.call("c", lapply(txt, fun))
}


#' @rdname parse.date
kMonthWords <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct","nov","dec")

#' @rdname parse.date
kMonthName <- paste(
    c("((", paste(kMonthWords, collapse=")|("), "))"), collapse="")



#' .get_month_num
#' If a string contains a fragment of the name of a month, 
#' it returns the month number.
#' @param txtstr character; the string to test
#' @return numeric; the month number, or -1 if none
#' @examples 
#'      date.reader:::.get_month_num("blahblahjanuaryblahblah")
#' @rdname parse.date
#' @note Internal function, not exported

.get_month_num <- function(txtstr) {
  txtstr <- tolower(txtstr)
  res <- regexpr(kMonthName, txtstr)
  if (res == -1) return(-1)
  txtstr <- substr(txtstr, res, res+2)
  return(which(kMonthWords==txtstr))
}

#' @rdname parse.date
all.orders <- c(
  "mdy",
  "mdy_hms",
  "mdy_hm",
  "mdy_h",
  "dmy",
  "dmy_hms",
  "dmy_hm",
  "dmy_h", 
  "ymd_hms",
  "ymd_hm",
  "ymd_h",
  "ymd"
)
