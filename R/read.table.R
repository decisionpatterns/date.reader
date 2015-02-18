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
#     if ("POSIXlt" %in% colClasses) {
#       indices <- which(colClasses == "POSIXlt")
#       colClasses[indices] <- "character"
#     }
    args[['colClasses']] <- colClasses
  }
  
  dat <- do.call(utils::read.table, args)
  colClasses <- colClasses.old

  nams <- names(dat)
  cls.index <- 0
  as.is <- args[['as.is']]
  typ <- typeof(as.is)
  as.is.logical <- (typ == "logical")
  as.is.index <- 0

  for (col.idx in seq_len(ncol(dat))) {
    name <-  nams[[col.idx]]
    if (as.is.index == length(as.is)) {
        as.is.index <- 0 #recycle
    }
    as.is.index <- as.is.index + 1
    if (is.null(as.is)) {
        as.is.current <- FALSE
    } else if (as.is.logical) {
      as.is.current <- as.is[[as.is.index]]
      if (is.null(as.is.current) || is.na(as.is.current)) {
        as.is.current <- FALSE
      }
    } else {
        common <- intersect(as.is, c(name, col.idx))
        as.is.current <- (length(common) > 0)
    }
    if (as.is.current) {
        next
    }    

    if (cls.index == length(colClasses)) {
        cls.index <- 0
    }
    cls.index <- cls.index+1

    if (is.null(colClasses)) {
        cls <- NA
    } else if (named.colClass.elements) {
        cls <- colClasses[[name]]
        if (is.null(cls)) {
            cls <- NA
        }
    } else {
        cls <- colClasses[[cls.index]]
        if (is.null(cls)) {
            cls <- NA
        }
    }
      
    if (! is.na(cls)) {
      if (cls != "POSIXct")
        next # assume that utils::read.table did that right thing
    }

    x <- dat[, col.idx]
        
    orders <- which.orders(x)
    tz <- options::get_option(date.reader$tz, 'UTC')
    if (!is.na(orders)) {
      x <- lubridate::parse_date_time(x, orders, tz=tz)
      if (! is.na(cls) ) {
        if (cls == "POSIXlt") {
          x <- as.POSIXlt(x)
        }
      }
      dat[, col.idx] <- x
      next
    }
  }
  
  return(dat)
}
