#' .parse.date
#' 
#' @param txt character; value to convert to a POSIX date
#' @param orders character; name of the format, e.g. "MDY"
#' @param orders character vector of date-time formats as described in 
#'   \code{\link[lubridate]{parse_date_time}}. Each order string is 
#'   series of formatting characters as listed \code{\link[base]{strptime}} but
#'   without the "%" prefix, for example "ymd" will match all the possible 
#'   dates in year, month, day order. Formatting orders might include 
#'   arbitrary separators. These are discarded. See details for implemented 
#'   formats.
#' @param tz character; optional time zone
#' @param check.regex Logical; if TRUE, check syntax of \code{txt} before 
#'   calling lubridate function.
#'
#' @note Internal function, a wrapper for lubridate functions
#'
#' @return 
#' POSIXct object representing a date (or datetime), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'    date.reader:::.parse.date( "January 11, 2014", "mdy" )
#'    date.reader:::.parse.date( "January 11, 2014", "MDY" )
#'    
#'    date.reader:::.parse.date( "2014/02/16", "YMD" )
#'    date.reader:::.parse.date( "2014-08-05", "YMD" )
#'    date.reader:::.parse.date( c("2014-08-05","2014-08-06" ), "YMD" )
#'    date.reader:::.parse.date( "20140805", "ymd.numeric")
#'    date.reader:::.parse.date( "14/08/05", "ymd.alt")
#'    date.reader:::.parse.date( "2/2/15", "dmy" )
#'          
#' @rdname parse.date

.parse.date <- function( 
    txt
  , orders
  , tz = getOption('date.reader.tz')
  , check.regex = TRUE
) {

  txt <- gsub( "a.m.", "am", txt, ignore.case=TRUE )
  txt <- gsub( "p.m.", "pm", txt, ignore.case=TRUE )
  
  # NOT SURE WHY THIS IS DONE?
  if(check.regex) {
    regex <- lookup.regex(orders)
    
    if( is.na(regex) ) return(NA)
    if( ! grepl(regex, txt, ignore.case=TRUE) ) return(NA)  # VECTORIZE
    
  }
  
  # STRIP x.y -> x
  orders <- tolower(orders)
  orders <- gsub( '\\..*$', '', orders )
  
  
  # index <- gregexpr( ".", orders, fixed=TRUE )  # Find dot(.) in orders
  # index <- as.integer(index[[1]])               # coerce to integer ?
  # if( index > 1 ) orders <- substr(orders, 1, index-1) 
  
  
  return( 
    lubridate::parse_date_time(x=txt, orders=orders, tz=tz) 
  )  
  
}
