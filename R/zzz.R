.onAttach <- function( libname, pkgname ) {

  packageStartupMessage( 
    pkgname ,
    "-" ,
    utils::packageVersion(pkgname, libname),
    " by Decision Patterns" ,
    domain = NA
  )
  
  options( date.reader.tz="UTC" )    
  options( date.reader.nErrors=5 )   # ???
  options( date.reader.nTrials=10 )  # ??? 

}
