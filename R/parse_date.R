library(lubridate)
source("parse_regex.R")

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
#'   \code{\link[base]{as.POSIX}}
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

#' which.format.aux
#' 
#' @param txt character; value to convert to a POSIX date
#'
#' @return 
#' character representing the name of the date format
#'
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @examples 
#'   # -tk
#'  which.format.aux("January 11, 2014")
#'  which.format.aux("2014/02/16")
#'    
#' @note Internal function
#'   
which.format.aux <- function(txt) {
    for (fmt in kFormatNames) {
        if (grepl("YYYY", fmt)) {
            if (grepl("[^0-9]", txt)) {
                next
            }
        }
        z <- parse.date.aux(txt, fmt)
            if (! is.na(z)) {
                return(fmt)
            }
        }
    return(NA)
}

#' which.format
#' 
#' @param x vector of characters; values to convert to a POSIX date
#' @param nTrials integer; number of strings to check before deciding
#' the format
#' @param nErrors integer; the number of unparsable strings to allow. 
#' If the number of unparseable strings exceeds nErrors, return NA,
#' which means that the strings are either not dates, or not a consistent
#' format.
#'
#' @return 
#' character representing the name of the date format
#'
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @examples 
#'   # -tk
#'  x <- c("January 11, 2014", "February 15, 1958", "2015/03/23")
#'  which.format(x, nTrials=3, nErrors=1)
#'    
#' @note Internal function
#'   
which.format <- function(x, nTrials=1, nErrors=0) {
    nTrials.actual <- min(length(x), nTrials)
    z <- x[1:nTrials.actual]
    formats <- unlist(lapply(z, which.format.aux))
    w <- table(formats)
    if (length(w) == 0) {
        return(NA)
    }
    n <-max(w)
    format <- names(which(w==n))
    format <- format[[1]]
    nErrors.actual <- nTrials.actual - n
    if (nErrors.actual > nErrors) {
        return(NA)
    }
    if (grepl("YYYY", format)) {
        if (nTrials.actual < nTrials) {
            return(NA)
        }
    }
    return(format)
}

#' as.POSIXct.character
#' 
#' @param x vector of character; values to convert to a POSIX date
#' @param fmt character; name of the format for the values. If
#' this parameter is NA, then the function will try to guess the format
#' for each value
#'
#' @return vector of POSIXct values (or NA, if a value cannot be parsed)
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @examples 
#'   # -tk
#'  dts <- c( '20140210', '19791118', '19720329' ) 
#'  dts <- rep( '20140210', 100 )
#'  dts <- c( '14-02-10', '79-11-18' )
#'  as.POSIXct.character( dts )  
#'    
#'     
#' @export
as.POSIXct.character <- function(x, fmt=NA) {
    f <- function(txt) {
        if (is.na(fmt)) {
            fmt <- which.format(txt)
        }
        if (is.na(fmt)) {
            return(NA)
        }
        return(parse.date.aux(txt, fmt))
    }
    result <- lapply(x,f)
    result <- do.call(c, result)
    return(result)
}


#' type.convert
#' 
#' @param x vector of character; values to parse
#'
#' @return vector of parsed values. If the values
#' can be interpreted as dates, it returns a vector
#' of POSIXct objects. Otherwise, it calls utils::type.convert
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @export
type.convert <- function(x, ...) {
  fmt <- which.format(x, nTrials=kTrials, nErrors=kErrors)
  if (is.na(fmt)) {
    return(utils::type.convert(x, ...))
  }
  return(as.POSIXct.character(x, fmt=fmt))
}

#' read.table
#' 
#' @param file character; path to a file to be read in
#'
#' @return data frame. If a column can be interpreted as
#' dates, then it translates the value into POSIXct objects.
#' Otherwise, it acts as utils::read.table
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @export
read.table <- function(...) {
  dat <- utils::read.table(...)
  for (col.idx in seq_len(ncol(dat))) {
    x <- dat[, col.idx]
    fmt <- which.format(x)
    if (!is.na(fmt)) {
      x <- as.POSIXct(x, fmt=fmt)
      dat[, col.idx] <- x
        break
    }
  }
  dat
}
