library(testthat)
library(lubridate)


context( "test-as.POSIX.R")

dts.1 <- c( '20140210', '19791118', '19720329' ) 
dts.2 <- c('January 31, 2011', 'March 23, 1957', 'October 26, 1929')
dts.3 <- c("10/22/1977 12:33 pm", "9/1/1997 10:33 am", "12/25/1942 12:00 am")
dts.4 <- c("10/12/25", "14/11/13", "09/03/29")

z1 <- as.POSIXct(dts.1)
z2 <- as.POSIXct(dts.2)
z3 <- as.POSIXct(dts.3)
z4 <- as.POSIXct(dts.4)

expect_is( z1, 'POSIXct')
expect_is( z2, 'POSIXct')
expect_is( z3, 'POSIXct')
expect_is( z4, 'POSIXct')


w1 <- ymd(dts.1)
w2 <- mdy(dts.2)
w3 <- mdy_hm(dts.3,tz = "UTC")
w4 <- ymd(dts.4)

expect_equivalent( z1, w1 )
expect_equivalent( z2, w2 )
expect_equivalent( z3, w3 )
expect_equivalent( z4, w4 )
