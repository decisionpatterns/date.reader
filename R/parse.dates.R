#' parse.dates
#' 
#' @param ... additional arguments passed to \code{utils::read.table}
#' @param x data; the table to fix
#'
#' @return data frame or data table
#' 
#' If a character column can be interpreted as dates, then translate
#' values into POSIXct objects.
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples
#'   name <- c("bob", "fred", "sally")
#'   birthday <- c("01/22/1993", "02/25/1980", "03/31/1970")
#'   birthday <- as.factor(birthday)
#'   table <- data.frame(name,birthday)
#'   table1 <- data.table(table)
#'   table.new <- parse.dates(table)
#'   table.new1 <- parse.dates(table1)
#' @export

parse.dates <- function( 
  x
  , ... 
) 
  UseMethod('parse.dates')

#' @rdname parse.dates
#' @method parse.dates data.frame
#' @export

parse.dates.data.frame <- function(x, ...) {
  args <- list(...)  
  colClasses <- args[['colClasses']]
  colClasses <- .compute.classes(colClasses, x)
  for (i in seq_len(ncol(x))) {
    cls <- colClasses[[i]]
    force <- FALSE
    if (is.na(cls)) {
      force <- FALSE
    } else if (cls == "POSIXct") {
      force <- TRUE
    } else {
      next
    }
    col <- x[, i]
    orders <- which.orders(col, force=force)
    tz <- options::get_option(date.reader$tz, 'UTC')
    if (!is.na(orders)) {
      col <- lubridate::parse_date_time(col, orders, tz=tz)
      x[, i] <- col
      next
    }
  }
  return(x)
}

#' @rdname parse.dates
#' @method parse.dates default
#' @export

parse.dates.default <- function(x, ...) {
  x <- as.character(x)
  parse.dates(x, ...)
}

#' @rdname parse.dates
#' @method parse.dates character
#' @export
parse.dates.character <- function(x, ...) {
  args <- list(...)  
  colClasses <- args[['colClasses']]
  if (is.null(colClasses)) {
    colClasses <- NA
  }
  colClasses <- colClasses[[1]]
  tz <- args[["tz"]]
  if (is.na(colClasses)) {
    force = FALSE
  } else if (colClasses == "POSIXct") {
    force = TRUE
  } else {
    return(NA)
  }
  
  # parsing parameters
  tz <- args[["tz"]]
  if (is.null(tz)) 
    tz <- get_option( date.reader$tz, "UTC" )  
  orders <- args[["orders"]]
  if (is.null(orders)) 
    orders <- get_option( date.reader$orders, all.orders )
  autostart <- args[["autostart"]]
  if (is.null(autostart))
    autostart <- get_option( date.reader$autostart, 30 )
  nErrors <- args[["nErrors"]]
  if (is.null(nErrors))
    nErrors   = get_option( date.reader$nErrors, 0 )

  orders <- which.orders(
    x
    , orders = orders
    , nErrors = nErrors
    , autostart = autostart
    , force=force)
  
  if(is.na(orders)) {
    return(NA)
  }
    
  lubridate::parse_date_time(x, orders, tz=tz)
}

#' @rdname parse.dates
#' @method parse.dates data.table
#' @export
parse.dates.data.table <- function(x, ...) {
  args <- list(...)  
  colClasses <- args[['colClasses']]
  colClasses <- .compute.classes(colClasses, x)
  
  parse.dates.aux <- function(col, cls) {
    return(parse.dates(col, colClasses = cls))
  }
  
  new.cols <- mapply(parse.dates.aux, x, colClasses)
  affected <- function(col) {
    ! all(is.na(col))
  }
  
  indices <- sapply(new.cols, affected)
  indices <- which(indices)

  new.cols <- new.cols[indices]
  x[, indices := new.cols, with=FALSE]
  return(x)
}