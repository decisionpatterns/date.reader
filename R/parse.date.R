#' .parse.date
#' 
#' @param txt character; value to convert to a POSIX date
#' @param format character; name of the format, e.g. "MDY"
#' @param tz character; optional time zone
#' @param check.regex Logical; if TRUE, check syntax of 
#' txt before calling lubridate function.
#'
#' @return 
#' POSIXct object representing a date (or date-time), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'  date.reader:::.parse.date( "January 11, 2014", "MDY" )
#'  date.reader:::.parse.date( "2014/02/16", "YMD" )
#'  date.reader:::.parse.date( "2014-08-05", "YMD" )
#'  date.reader:::.parse.date( "20140805", "ymd.numeric")
#'  date.reader:::.parse.date( "14/08/05", "ymd.alt")
#'    
#' @note Internal function, a wrapper for lubridate functions
#' @include apply.lubridate.fun.R 
#' @rdname parse.date

.parse.date <- function(txt, format, tz=NULL, check.regex=TRUE) {
  if (is.null(tz)) {
    tz <- getOption("date.reader.tz", default="UTC")
  }

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

  return(apply.lubridate.fun(format, txt, tz))
  
}
