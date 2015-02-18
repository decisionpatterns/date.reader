library(testthat)
library(lubridate)

context( "can.POSIX.R")

dts <- list()
parses <- list()

  dts[["ex1"]]  <- rep("10/10/10", 20)
  parses[["ex1"]] <- TRUE
  
  dts[["ex2"]]  <- c(rep("10/10/10", 20), "13/10/10")
  parses[["ex2"]] <- TRUE
  
  dts[["ex3"]]  <- c(rep("10/10/10", 20), "gibberish")
  parses[["ex3"]] <- TRUE

  
  dts[["ex4"]]  <- c(rep("10/10/10", 20), rep("gibberish", 6))
  parses[["ex4"]] <- FALSE # Too many errors
  
  dts[["ex5"]]  <- c( '20140210', '19791118', '19720329' )
  parses[["ex5"]] <- TRUE
  
  dts[["ex6"]]  <- c('January 31, 2011', 'March 23, 1957', 'October 26, 1929')
  parses[["ex6"]] <- TRUE
  
  dts[["ex7"]]  <- c("10/22/1977 12:33 pm", "9/1/1997 10:33 am", "12/25/1942 12:00 am")
  parses[["ex7"]] <- TRUE
  
  dts[["ex8"]]  <- c("10/12/25", "14/11/13", "09/03/29")
  parses[["ex8"]] <- TRUE

options( date.reader = list(nErrors = 2) )

for (name in names(dts)) {
  dt <- dts[[name]]
  
  prs <- parses[[name]]
  if (is.null(prs)) {
    prs <- FALSE
  }
  
  expect_equivalent(prs, can.posix(dt))

}
