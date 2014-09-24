.onAttach <- function( libname, pkgname ) {

  packageStartupMessage( 
    pkgname ,
    "-" ,
    utils::packageVersion(pkgname, libname),
    " by Decision Patterns" ,
    domain = NA
  )
  
  opts <- list( 
      tz = "UTC"
    , nErrors = 5     # ????
    , nTrials = 10    # ????
  )
  
  options( date.reader = opts )
  
}
