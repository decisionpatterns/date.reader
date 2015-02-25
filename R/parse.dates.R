#' parse.dates
#'
#' parse columns into 
#' 
#' @param x object; the object to parse dates  
#' @param ... additional arguments passed to \code{utils::read.table}
#'
#' @return data frame or data table
#' 
#' \code{parse.dates} is an S3 generic function for parsing dates from an 
#' existing object. For parsing dates while reading see 
#' \code{\link[date.reader]{read.table}}.  
#' 
#' If a character or factor column can be interpreted as dates, it is 
#' coerces into POSIXct object
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples
#'   name     <- c("bob", "fred", "sally")
#'   birthday.character <- c("01/22/1993", "02/25/1980", "03/31/1970") 
#'   birthday.factor <- as.factor(birthday.character)
#'   
#'   df  <- data.frame( name, birthday.character, birthday.factor, stringsAsFactors=FALSE )
#'   res <- parse.dates(df)
#'   class( res$birthday.character )                # "POSIXct" "POSIXt" 
#'   class( res$birthday.factor )                  
#'   
#'   dt <- setDT( df )
#'   parse.dates(dt)                     
#'   class( dt$birthday.character )                # "POSIXct" "POSIXt" 
#'   class( dt$birthday.factor )
#'   
#' @export

parse.dates <- function( x, ... ) UseMethod('parse.dates')


#' @rdname parse.dates
#' @method parse.dates data.frame
#' @export

parse.dates.data.frame <- function( x, ... ) {
  
  args <- list(...)  
  colClasses <- args[['colClasses']]
  colClasses <- .compute.classes( colClasses, x )
  
  for ( i in seq_len(ncol(x)) ) {
    cls <- colClasses[[i]]
    col <- parse.dates( x[[i]], colClasses = cls )
    if ( all(! is.na(col)) ) x[[i]] <- col
  }
  return(x)
  
}


#' @rdname parse.dates
#' @method parse.dates default
#' @export

parse.dates.default <- function( x, ... ) {
  x <- as.character(x)
  parse.dates( x, ... )
}


#' @rdname parse.dates
#' @method parse.dates character
#' @export

parse.dates.character <- function( x, ... ) {
  
  args <- list(...)  
  colClasses <- args[['colClasses']]
  
  if (is.null(colClasses)) colClasses <- NA
  
  
  colClasses <- colClasses[[1]]
  tz <- args[["tz"]]
  if (is.null(tz)) 
    tz <- options::get_option(date.reader$tz, 'UTC')
  
  orders <- args[["orders"]]
  if (is.null(orders)) 
    orders <- options::get_option(date.reader$orders, all.orders)
  
  nErrors <- args[["nErrors"]]
  if( is.null(nErrors) ) 
    nErrors <- options::get_option(date.reader$nErrors, 0)
  
  autostart <- args[["autostart"]]
  if( is.null(autostart) ) 
    autostart <- options::get_option(date.reader$autostart, 30)
  
  
  if( is.na(colClasses) ) 
    force <- FALSE else 
    if (colClasses == "POSIXct") force <- TRUE else 
    return(NA)
  
  
  orders <- which.orders(
    x
    , force=force
    , orders=orders
    , autostart=autostart
    , nErrors=nErrors
  )
  
  if( is.na(orders) ) return(NA)
  
  lubridate::parse_date_time( x, orders, tz=tz )
  
}


#' @rdname parse.dates
#' @method parse.dates data.table
#' @import data.table
#' @export

parse.dates.data.table <- function( x, ... ) {
  
  args <- list(...)  
  colClasses <- args[['colClasses']]
  colClasses <- .compute.classes( colClasses, x )
  
  # parse.dates.aux <- function(col, cls) parse.dates(col, colClasses = cls)

  new.cols <- 
    mapply( 
        function(col, cls) parse.dates( col, colClasses = cls )
      , x
      , colClasses 
    )

  indices <- sapply( new.cols,  function(col) ! all( is.na(col) ) )
  indices <- which(indices)

  new.cols <- new.cols[indices]
  x[ , indices := new.cols, with=FALSE ]
  return(x)
  
}
