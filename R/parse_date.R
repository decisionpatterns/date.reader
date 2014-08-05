source("R/parse_regex.R")

#' parse.date.aux
#' 
#' @param txt character; value to convert to a POSIX date
#' @param fmt character; name of the format, e.g. "MDY"
#'
#' @return 
#' POSIXct object representing a date (or date-time), or NA
#' if the input cannot be parsed.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'   # -tk
#'  parse.date.aux("January 11, 2014", "MDY")
#'  parse.date.aux("2014/02/16", "YMD")
#'    
#' @note Internal function, a wrapper for lubridate functions
#'     

parse.date.aux <- function(txt, fmt) {
  
    txt <- gsub("a.m.", "am", txt, ignore.case=TRUE)
    txt <- gsub("p.m.", "pm", txt, ignore.case=TRUE)

    suppressWarnings(
        tryCatch({
            format <- lookup.format(fmt)
            if (is.na(format)) {
              return(NA)
            }
            if (! grepl(format, txt, ignore.case=TRUE)) {
              return(NA)
            }
            f <- lookup.lubridate.fun(fmt)
            if (is.na(f)) {
                return(NA)
            }
            return(f(txt))
        }))
    return(NA)
}
