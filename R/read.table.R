#' read.table
#' 
#' @param ... additional arguments passed to \code{utils::read.table}
#' @param file the file or connection to read the table from
#'
#' @return data frame. 
#' 
#' If a character column can be interpreted as dates, then it translates the 
#' value into POSIXct objects.  Otherwise, it acts as utils::read.table
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples
#'   name <- c("bob", "fred", "sally")
#'   birthday <- c("01/22/1993", "02/25/1980", "03/31/1970")
#'   birthday <- as.factor(birthday)
#'   table <- data.frame(name,birthday)
#'   
#'   conn <- textConnection("read.table.txt", 'w')
#'   write.table(table, file=conn, quote=FALSE, sep="\t", row.names=FALSE)
#'   
#'   conn <- textConnection(read.table.txt)
#'   table.new <- read.table(conn, header=TRUE, sep="\t")
#'      
#' @export

read.table <- function(file, ...) {
  args <- list(file, ...)

  colClasses.old <- args[['colClasses']]
  colClasses <- colClasses.old
  
  # two cases: colClasses has named elements
  nams <- names(colClasses)
  nonempty.names <- setdiff(nams, "")
  named.colClass.elements <- (length(nonempty.names) > 0)
  
  if (is.null(colClasses)) {
    args[['colClasses']] <- NA
  } else {
    if ("POSIXct" %in% colClasses) {
      indices <- which(colClasses == "POSIXct")
      colClasses[indices] <- "character"
    }
    args[['colClasses']] <- colClasses
  }
  
  dat <- do.call(utils::read.table, args)
  dat <- parse.dates(dat, ...)  
  return(dat)
}
