#' .parse.date
#' 
#' @param txt character; value to convert to a POSIX date
#' @param format character; name of the format, e.g. "MDY"
#'
#' @return 
#' POSIXct object representing a date (or date-time), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'  .parse.date( "January 11, 2014", "MDY" )
#'  .parse.date( "2014/02/16", "YMD" )
#'  .parse.date( "2014-08-05", "YMD" )
#'  .parse.date( "20140805", "ymd.numeric")
#'  .parse.date( "14/08/05", "ymd.alt")
#'    
#' @note Internal function, a wrapper for lubridate functions
#' @rdname parse.date 

.parse.date <- function(txt, format, tz="", check.regex=TRUE, ...) {
  
  txt <- gsub( "a.m.", "am", txt, ignore.case=TRUE )
  txt <- gsub( "p.m.", "pm", txt, ignore.case=TRUE )
  if (check.regex) {
    regex <- lookup.regex(format)
    if (is.na(regex)) {
      return(NA)
    }
    if (! grepl(regex, txt, ignore.case=TRUE)) {
      return(NA)
    }
  }

  f <- lookup.lubridate.fun(format)
  suppressWarnings(
  if (is.na(f)) {
      return(NA)
  })
  suppressWarnings({
    return( f(txt, tz=tz, ...) )
  }
  )
}
