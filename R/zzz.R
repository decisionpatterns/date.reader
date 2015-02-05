.onAttach <- function( libname, pkgname ) {

  packageStartupMessage( 
    pkgname ,
    "-" ,
    utils::packageVersion(pkgname, libname),
    " - Copyright \u00a9 ", substr(Sys.Date(),1,4),
    " Decision Patterns" ,
    domain = NA
  )

  
  opts <- list( 
      tz = "UTC"      # the default time zone to use for parsing dates
                      # Note: if tz is the empty string, that represents the local time zone.
    , nErrors = 0     # The number of errors before giving up
    , autostart = 30  # The number of strings to consider before deciding on the date format
  )
  
  options( date.reader = opts )
  
  options( date.reader.tz = "UTC")
  options( date.reader.nErrors = 0 )
  options( date.reader.autostart = 30 )
  
}
