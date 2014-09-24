#' apply.lubridate.fun
#' 
#' Applies a lubridate function
#' 
#' @param format character; the name of the date format (case insensitive)
#' @param txt character; the text to convert to a date
#' @param tz character; the time zone
#'
#' @return 
#' A POSIXct object, or NA
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples 
#'   \dontrun{ 
#'     apply.lubridate.fun("mdy", "01/23/2017", "UTC")
#'   } 
#'     
#' @note Internal function that is not exported
#' @include Constants.R

apply.lubridate.fun <- function(format, txt, tz=getOption("date.reader")$tz ) {
  
  retval <- suppressWarnings(
    switch( format,
           ymd_hms.numeric = lubridate::ymd_hms(txt, tz=tz),
           ymd_hm.numeric = lubridate::ymd_hm(txt, tz=tz),
           ymd_h.numeric  = lubridate::ymd_h(txt, tz=tz),
           ymd.numeric  = lubridate::ymd(txt, tz=tz),
           dmy_hms = lubridate::dmy_hms(txt, tz=tz),
           dmy_hm = lubridate::dmy_hm(txt, tz=tz),
           dmy_h = lubridate::dmy_h(txt, tz=tz),
           dmy = lubridate::dmy(txt, tz=tz),
           mdy_hms = lubridate::mdy_hms(txt, tz=tz),
           mdy_hm = lubridate::mdy_hm(txt, tz=tz),
           mdy_h = lubridate::mdy_h(txt, tz=tz),
           mdy = lubridate::mdy(txt, tz=tz),
           ymd_hms = lubridate::ymd_hms(txt, tz=tz),
           ymd_hm = lubridate::ymd_hm(txt, tz=tz),
           ymd_h = lubridate::ymd_h(txt, tz=tz),
           ymd = lubridate::ymd(txt, tz=tz),
           ymd_hms.alt = lubridate::ymd_hms(txt, tz=tz),
           ymd_hm.alt = lubridate::ymd_hm(txt, tz=tz),
           ymd_h.alt = lubridate::ymd_h(txt, tz=tz),
           ymd.alt = lubridate::ymd(txt, tz=tz)))
  
  return(retval)
  
}
