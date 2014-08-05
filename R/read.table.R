#' read.table
#' 
#' @param file character; path to a file to be read in
#'
#' @return data frame. If a column can be interpreted as
#' dates, then it translates the value into POSIXct objects.
#' Otherwise, it acts as utils::read.table
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
