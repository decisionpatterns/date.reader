#' read.table
#' 
#' @param ... additional arguments passed to \code{utils::read.table} 
#'
#' @return data frame. 
#' 
#' If a character column can be interpreted as dates, then it translates the 
#' value into POSIXct objects.  Otherwise, it acts as utils::read.table
#'
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @examples
#'   # -tk
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
