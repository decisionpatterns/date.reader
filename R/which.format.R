#' which.format
#' 
#' @param x character; values to convert to a POSIX date
#' 
#' @param nTrials integer; number of strings to check before deciding
#' the format
#' 
#' @param nErrors integer; the number of unparsable strings to allow. 
#' If the number of unparseable strings exceeds nErrors, return NA,
#' which means that the strings are either not dates, or not a consistent
#' format.
#'
#' @return 
#'   character, name for the date format
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
#' @export
  
which.format <- function(x, nTrials=1, nErrors=0) {
  
  nTrials.actual <- min(length(x), nTrials)
  z <- x[1:nTrials.actual]
  formats <- unlist(lapply(z, .which.format))
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


#' .which.format
#' 
#' @param txt character; value to convert to a POSIXct date
#'
#' @return 
#' character representing the name of the date format
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'  .which.format("January 11, 2014")
#'  .which.format("2014/02/16")
#'    
#' @note Internal function that is not exported
#' @rdname which.format
#' @export
   
.which.format <- function(txt) {
  for (fmt in all.regex.names()) {
    if (grepl(".numeric", fmt)) {
      if (grepl("[^0-9]", txt)) {
        next
      }
    }
    z <- .parse.date(txt, fmt)
    if (! is.na(z)) {
      return(fmt)
    }
  }
  return(NA)
  
}
