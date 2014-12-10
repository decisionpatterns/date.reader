#' date.reader
#' 
#' Recognizes and coerces dates when converting or reading data by overloading
#' the \code{type.convert} and \code{read.table} methods.
#' 
#' @details
#' The package contains the following exported methods:
#' 
#' \code{type.convert}: Overrides base::type.convert to automatically 
#' converts character vectors to POSIXct objects, if they are parsable.
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
#' \strong{OPTIONS}
#' This package introduces a number of options that affect how the code works:
#' 
#' \code{tz}: the default time zone to use for parsing dates (initially set to 
#' "UTC").  Note: if tz is the empty string, that represents the local time zone.
#'     
#' \code{nErrors}: The number of errors in parsing strings in a vector before 
#' giving up (initially set to 5)
#' 
#' \code{autostart}: The number of strings to consider before deciding on the 
#' date format (initially 30)
#' 
#' 
#' @name date.reader
#' 
#' @examples 
#'   # -tk
#'   
#' @docType package

NULL
