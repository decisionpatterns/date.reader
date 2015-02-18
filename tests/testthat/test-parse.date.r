library(lubridate)
library(testthat)
library(date.reader)

context('.parse.date.strict')

  expect_is( 
      .parse.date.strict( "January 11, 2014", "mdy" ) 
    , c("POSIXct", "POSIXt")
  )
  
  
  expect_is( 
      .parse.date.strict( "January 11, 2014", "MDY" )
    , c("POSIXct", "POSIXt")
  )
  
  
  expect_is( 
      .parse.date.strict( "2014/02/16", "YMD" ) 
    , c("POSIXct", "POSIXt")
  )
  

  expect_is( 
      .parse.date.strict( "2014-08-05", "YMD" )
    , c("POSIXct", "POSIXt")
  )
  

  expect_is( 
      .parse.date.strict( c("2014-08-05","2014-08-06" ), "YMD" ) 
    , c("POSIXct", "POSIXt")
  )
  

  expect_is( 
      .parse.date.strict( "20140805", "ymd") 
    , c("POSIXct", "POSIXt")
  )
  
  
  expect_is( 
      .parse.date.strict( "14/08/05", "ymd") 
    , c("POSIXct", "POSIXt")
  )
  

  expect_is( 
    .parse.date.strict( "2/2/15", "dmy" )
  , c("POSIXct", "POSIXt")
  )
