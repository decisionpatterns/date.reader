#' date.reader
#' 
#' Recognizes and coerces dates when converting or reading data by overloading
#' the \code{type.convert} and \code{read.table} methods.
#' 
#' @details
#'
#' The \code{date.reader} package is comprised of several exported functions and
#' a few global options that control the behavior of those functions.
#'    
#' @section Exported Functions: 
#' 
#' \code{type.convert}: Overrides base::type.convert to automatically 
#' converts character vectors to POSIXct objects if they are parsable.
#' For anything other than a character vector, it works like base::type.convert.
#' 
#' \code{as.POSIXct.character}: A function that converts character vectors to 
#' \code{POSIXct} object.
#' 
#' \code{which.format}: A function that takes a vector of character vectors 
#' representing dates and returns the deduced format ("mdy", etc.)
#' 
#' \code{can.posix}: Returns TRUE if given a vector of character vectors that
#' can be parsed as dates.
#' 
#' \code{read.table}: Overrides \code{base::read.table}; converts a column of 
#' date strings into a column of POSIXct objects, if they can be parsed. 
#' (Note: assumes all strings have a consistent format). For all columns that 
#' cannot be parsed as dates, this works like base::read.table.
#'
#'  
#' @section Options:
#' 
#' This functions in this package allow for a number of options that affect 
#' how the date.parsing works. (Note: this package relies on the 
#' \code{options} package. Each of these options is within the 
#' \code{date.reader} option).
#' 
#' \code{tz}: the default time zone to use for parsing dates. The default for 
#' functions is to use "UTC". If \code{tz} is the empty string, the 
#' local time zone is used.
#'     
#' \code{nErrors}: The number of allowed errors in parsing strings in a vector 
#' before giving up. The default 
#' 
#' \code{autostart}: The number of strings to consider before deciding on the 
#' date format (initially 30)
#' 
#' @name date.reader
#' 
#' @examples 
#'   # -tk
#'   
#' @import lubridate options   
#' @docType package

NULL

date.reader = NULL  # This is needed for the options package