#' lookup.regex
#' 
#' find a regexp by name
#' 
#' @param name character; the name given to the regex
#'
#' @return 
#' the regex with that name
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'   date.reader:::lookup.regex("mdy")
#'    
#' @include Constants.R

lookup.regex <- function(name) {
  name <- tolower(name)
  ret <- kFormats[[name]]
  if ( is.null(ret) ) {
    return(NA)
  }
  return(ret)
}
