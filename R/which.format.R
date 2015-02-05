#' Utilities for detecting the format of date strings
#' 
#' @name which.format
NULL

#' which.format
#' 
#' @param x character; values to convert to a POSIX date
#' 
#' @param autostart integer; number of strings to check before deciding
#' the format
#' 
#' @param nErrors numeric; A non-negative number:  
#' if >= 1, the number of unparsable strings to allow
#' if <  1, the fraction of values that are allowed to be unparsable before 
#' giving up.
#'
#' @description 
#' 
#' If the number of unparseable strings exceeds nErrors, return NA,
#' which means that the strings are either not dates, or not a consistent
#' format.
#' 
#' @return 
#'   character, name for the date format, e.g. mdy, ymd_hm, etc.
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}} \cr
#'   \code{\link[lubridate]{guess_formats}} \cr
#'   
#' @examples 
#'   x <- c("January 11, 2014", "February 15, 1958", "March 3, 1969")
#'   which.format(x, autostart=3, nErrors=1)
#'    
#' @export
  
which.format <- function(
    x
  , autostart = getOption('date.reader.autostart')
  , nErrors   = getOption('date.reader.nErrors')
  
) {
  
  # autostart
  autostart.actual <- min(length(x), autostart)
  indices = round( seq( from=1, to=length(x), length.out=autostart.actual ) )
  
  # nErrors
  
  z <- x[indices]
  formats <- unlist(lapply(z, .which.format) )  # FIX: should assume a consistent format
  w <- table(formats)

  # TRAP NO FORMATS
  if( length(w) == 0 ) return(NA)

  
  n <-max(w)
  format <- names(which(w==n))
  format <- format[[1]]
  nErrors.actual <- autostart.actual - n
  
  if( nErrors.actual > nErrors) {
    return(NA)
  }
  
  if(  grepl(".numeric", format) ) 
    if( autostart.actual < autostart) 
      return(NA)
    
  return(format)
  
}


# .which.format
# 
# Determine the format for a string value
# 
# @param txt character; value to convert to a POSIXct date
# 
# @description
# \code{.which.format} is an internal function that checks \code{txt} against
# a series of regular expressions to determine the format of 
# 
# 
# @return 
# character; representing the name of the date format such as \strong{mdy}, 
# \strong{ymd_hm}, etc.
# 
# @seealso 
#   \code{\link[base]{as.POSIXct}} \cr
# @examples 
#    date.reader:::.which.format("January 11, 2014") # mdy
#    date.reader:::.which.format("2014/02/16")       # ymd
#    date.reader:::.which.format("2/2/2015")        
# @note Internal function, not exported
# @rdname which.format
   
.which.format <- function(txt) {

  for (fmt in names(kFormats) ) {
    if(  grepl(".numeric$", fmt) && grepl("[^0-9]", txt) ) next      
    z <- .parse.date(txt, fmt)
    if( ! is.na(z) ) return(fmt)
    
  }
  
  return(NA)
  
}


# .which.format2 <- function(txt) {
#   
#   for (fmt in names(kFormats) ) {
#     if(  grepl(".numeric$", fmt) && grepl("[^0-9]", txt) ) next      
#     z <- .parse.date(txt, fmt)
#     if( ! is.na(z) ) return(fmt)
#     
#   }
#   
#   return(NA)
#   
#   
# }