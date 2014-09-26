#' .parse.date
#' 
#' @param txt character; value to convert to a POSIX date
#' @param orders character; name of the format, e.g. "MDY"
#' @param tz character; optional time zone
#' @param check.regex Logical; if TRUE, check syntax of \code{txt} before 
#'   calling lubridate function.
#'
#' @return 
#' POSIXct object representing a date (or datetime), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'    date.reader:::.parse.date( "January 11, 2014", "MDY" )
#'    date.reader:::.parse.date( "2014/02/16", "YMD" )
#'    date.reader:::.parse.date( "2014-08-05", "YMD" )
#'    date.reader:::.parse.date( "20140805", "ymd.numeric")
#'    date.reader:::.parse.date( "14/08/05", "ymd.alt")
#'     
#' @note Internal function, a wrapper for lubridate functions
#'  
#' @rdname parse.date

.parse.date <- function( txt, orders, tz=getOption("date.reader")$tz 
  , check.regex=TRUE
) {

  txt <- gsub( "a.m.", "am", txt, ignore.case=TRUE )
  txt <- gsub( "p.m.", "pm", txt, ignore.case=TRUE )
  
  if (check.regex) {
    regex <- lookup.regex(orders)
    if (is.na(regex)) {
      return(NA)
    }
    if (! grepl(regex, txt, ignore.case=TRUE)) {
      return(NA)
    }
  }
  orders <- tolower(orders)
  index <- gregexpr(".", orders, fixed=TRUE)
  index <- as.integer(index[[1]])
  
  if (index > 1) {
    orders <- substr(orders, 1, index-1)
  }
  return(lubridate::parse_date_time(txt, orders, tz=tz))  
}
