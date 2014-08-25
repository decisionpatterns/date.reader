#' read.table
#' 
#' @param ... additional arguments passed to \code{utils::read.table} 
#'
#' @return data frame. 
#' 
#' Same as \code{utils::read.table} except when a character or factor column can
#' be coerced to a POSIXct date. 
#'
#' @seealso 
#'   \code{\link[utils]{read.table}}
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples
#'   # -tk
#'      
#' @export

read.table <- function(...) {
  
  dat <- utils::read.table(...)
  
  for (col.idx in seq_len(ncol(dat))) {
    
    x <- dat[, col.idx]
    format <- which.format(x)
    
    if (!is.na(format)) {
      x <- as.POSIXct( x, format=format )
      dat[, col.idx] <- x
      break
    }
  }
  dat
}
