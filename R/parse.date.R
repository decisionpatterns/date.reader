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
#'    
#' @note Internal function, a wrapper for lubridate functions
#' @rdname parse.date 

.parse.date <- function(txt, format) {
  
  txt <- gsub( "a.m.", "am", txt, ignore.case=TRUE )
  txt <- gsub( "p.m.", "pm", txt, ignore.case=TRUE )

  suppressWarnings(
    tryCatch({
      format <- lookup.format(format)
      if (is.na(format)) {
        return(NA)
      }
      if (! grepl(format, txt, ignore.case=TRUE)) {
        return(NA)
      }
      f <- lookup.lubridate.fun(format)
      if (is.na(f)) {
          return(NA)
      }
      return( f(txt) )
    }))
  return(NA)
}
