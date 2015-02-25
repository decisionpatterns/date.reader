#' Internal functions for normalizing the column classes spec for read.table
#' 
#' @name compute.classes
NULL

#' .compute.classes
#' 
#' .compute.classes
#'
#' @param colClasses character; spec for classes of a data table or data frame
#'
#' @param x data; the data table or data frame
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
#' @examples 
#' # Named arguments
#'   x <- list(age="numeric", name="character", state="character")
#'   df <- data.frame(
#'    name=c("fred", "barney"), 
#'    age=c(52, 47), 
#'    ssn=c("123-45-6789", "012-34-5678"),
#'    weight=c(214, 145))
#'   x <- date.reader:::.compute.classes(x,df)
#' # Note should be c("character", "numeric", NA, NA)
#'   
#' # Unnamed arguments
#'   x <- list("character", "numeric")
#'   x <- date.reader:::.compute.classes(x,df) 
#'   # Note should be c("character", "numeric", "character", "numeric")
#'   
#' # NULL argument
#'   x <- NULL
#'   x <- date.reader:::.compute.classes(x,df) 
#'   # Note should be c(NA, NA, NA, NA)
#'   
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'      
#' @rdname compute.classes

.compute.classes <- function( 
    colClasses
  , x
) {
  if (is.null(colClasses)) {
    colClasses <- rep(NA, ncol(x))
    return(colClasses)
  }
  nams1 <- names(colClasses)
  nonempty.names <- setdiff(nams1, "")
  named.colClass.elements <- (length(nonempty.names) > 0)
  if (named.colClass.elements) {
    nams <- names(x)
    bad_names <- setdiff(nams1, nams)
    if (length(bad_names) > 0)
      warning("not all columns named in 'colClasses' exist")
    good_names <- intersect(nams,nams1)
    retval <- rep(NA, ncol(x))
    for (i in 1:ncol(x)) {
      nam <- nams[[i]]      
      if (nam %in% good_names) {
        retval[[i]] <- colClasses[[nam]]
      }
    }
    return(retval)
  }
  
  retval <- rep(NA, ncol(x))
  index <- 0
  for (i in 1:ncol(x)) {
    if (index == length(colClasses)) {
      index <- 0
    }
    index <- index+1
    retval[[i]] <- colClasses[[index]]
  }
  return(retval)
}
