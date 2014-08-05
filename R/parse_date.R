MONTH_WORDS <- c(
  "january", "jan",
  "february", "feb",
  "march", "mar",
  "april", "apr",
  "may",
  "june", "jun",
  "july", "jul",
  "august", "aug",
  "september", "sep",
  "october", "oct",
  "november", "nov",
  "december", "dec")

DAY_WORDS <- c(
  "sunday", "sun", "su",
  "monday", "mon", "mo",
  "tuesday", "tue", "tu",
  "wednesday", "wed", "we",
  "thursday", "thu", "th",
  "friday", "fri", "fr",
  "saturday", "sat", "sa")

MISC_SYNTAX <- c(
  ",", "/", "-", "at", ":", "am", "a.m.", "pm", "p.m.")

DIGIT_REG <- "\\d+(th)?"

strip_recognized_strings <- function(txt) {
  for (month in MONTH_WORDS) {
    txt <- gsub(month, " ", txt, ignore.case=TRUE)
  }
  for (day in DAY_WORDS) {
    txt <- gsub(day, " ", txt, ignore.case=TRUE)
  }
  for (syn in MISC_SYNTAX) {
    txt <- gsub(syn, " ", txt, ignore.case=TRUE)
  }
  txt <- gsub(DIGIT_REG, " ", txt, ignore.case=TRUE)
  txt <- gsub(" ", "", txt)
  return(txt)
}

#' contains_months
#' 
#' @examples
#'   dts <- c( '20140210', '19791118', '19720329' ) 
#'   contains_months(dts)

contains_months <- function(txt) {
  retval <- rep(FALSE, length(txt))
  for (month in MONTH_WORDS) {
    value <- grepl(month, txt, ignore.case=TRUE)
    retval <- retval | value
  }
  return(retval)
}

can_date <- function(txt) {
  txt <- strip_recognized_strings(txt)
  value <- as.posix(txt)
  return(! is.na(value))
}


#' get_formats
#' 
#' Probably should be 
#' 
#' @return 
#'   Vector of functions for coercing character to POSIXct. 
#' 
#' @param 

get_formats <- function(txt) {
  
  if( contains_months(txt) ) {
    FORMATS <- c(
      dmy_hms,
      dmy_hm,
      dmy_h,
      dmy,
      mdy_hms,
      mdy_hm,
      mdy_h,
      mdy,
      ymd_hms,
      ymd_hm,
      ymd_h,
      ymd)
  }
  else {
    FORMATS <- c(
      mdy_hms,
      mdy_hm,
      mdy_h,
      mdy,
      dmy_hms,
      dmy_hm,
      dmy_h,
      dmy,
      ymd_hms,
      ymd_hm,
      ymd_h,
      ymd)
  }
  return(FORMATS)
}


#' 
#' 

date.format <- function(x) { 

  



}


#' as.POSIXlt.character
#' 
#' @param x character; value to convert to a POSIX date
#'
#' 
#' @seealso 
#'   \code{\link[base]{as.POSIX}}
#'   
#' @examples 
#'   # -tk
#'  dts <- c( '20140210', '19791118', '19720329' ) 
#'  dts <- rep( '20140210', 100 )
#'  dts <- c( '14-02-10', '79-11-18' )
#'  as.posix( dts )  
#'    
#' @note should be names as.POSIXct.character 
#'     
#' @export

as.posix <- function(x) {
  
  retval <- NA
  formats <- get_formats(x)
  
  for ( f in formats ) {
    value = f( x, quiet=TRUE )
    if( all( ! is.na(value) ) ) break 
  }
  
  return(retval)
}

