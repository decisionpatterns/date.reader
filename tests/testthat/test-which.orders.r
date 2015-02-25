library(testthat)
library(lubridate)

context( "which.orders.R")

.normalize_orders <- function(txt) {
  if (is.na(txt)) return(NA)
  txt <- gsub("_", "", txt)
  txt <- tolower(txt)
}

dts <- list()
ords <- list()
warn <- list()
insufficient.examples <- list()

dts[["ex1"]]  <- rep("10/10/10", 20)
ords[["ex1"]] <- "mdy"

dts[["ex2"]]  <- c(rep("10/10/10", 20), "13/10/10")
ords[["ex2"]] <- "dmy"

dts[["ex3"]]  <- c(rep("10/10/10", 20), "gibberish")
ords[["ex3"]] <- "mdy"
warn[["ex3"]] <- TRUE

dts[["ex4"]]  <- c(rep("10/10/10", 20), rep("gibberish", 6))
ords[["ex4"]] <- "mdy" # shouldn't parse--too many errors
warn[["ex4"]] <- TRUE
insufficient.examples[["ex4"]] <- TRUE

dts[["ex5"]]  <- c( '20140210', '19791118', '19720329' )
ords[["ex5"]] <- "ymd"
insufficient.examples[["ex5"]] <- TRUE # for all-digit case, need more examples

dts[["ex6"]]  <- c('January 31, 2011', 'March 23, 1957', 'October 26, 1929')
ords[["ex6"]] <- "mdy"

dts[["ex7"]]  <- c("10/22/1977 12:33 pm", "9/1/1997 10:33 am", "12/25/1942 12:00 am")
ords[["ex7"]] <- "mdy_hm"

dts[["ex8"]]  <- c("10/12/25", "14/11/13", "09/03/29")
ords[["ex8"]] <- "dmy"

dts[["ex9"]] <- c("20151227", "2015/12/27", "32/12/27")
ords[["ex9"]] <- NA # should not parse; mixes two-digit and 4-digint years

dts[["ex10"]] <- c("01/11/14", "January 11, 2014", "01/11/2014")
ords[["ex10"]] <- NA # shouldn't parse--different formats

dts[["ex11"]] <- c("20151227", "2015/12/27", "2033 January 27")
ords[["ex11"]] <- "ymd" # mixed formats, but harmless

dts[["ex12"]] <- "20131223"
ords[["ex12"]] <- "ymd"
insufficient.examples[["ex12"]] <- TRUE

dts[["ex13"]] <- "2013122312"
ords[["ex13"]] <- "ymd_h"
insufficient.examples[["ex13"]] <- TRUE

dts[["ex14"]] <- "201312231222"
ords[["ex14"]] <- "ymd_hm"
insufficient.examples[["ex14"]] <- TRUE

dts[["ex15"]] <- "20131223122234"
ords[["ex15"]] <- "ymd_hms"
insufficient.examples[["ex15"]] <- TRUE

dts[["ex16"]] <- "20131223 12 am"
ords[["ex16"]] <- "ymd_h"

dts[["ex17"]] <- "20131223 12:22 am"
ords[["ex17"]] <- "ymd_hm"

dts[["ex18"]] <- "20131223 12:22:34 am"
ords[["ex18"]] <- "ymd_hms"

dts[["ex19"]] <- "13/10/10"
ords[["ex19"]] <- "dmy"
#standard[["ex19"]] <- "2010/10/13"

dts[["ex20"]] <- "32/10/10"
ords[["ex20"]] <- "ymd"
#standard[["ex20"]] <- "2032/10/10"


dts[["ex21"]] <- "January 12, 2015"
ords[["ex21"]] <- "mdy"
#standard[["ex21"]] <- "2015/01/12"


dts[["ex22"]] <- "12th of January, 2015"
ords[["ex22"]] <- "dmy"
##standard[["ex22"]] <- "2015/01/12"

dts[["ex23"]] <- "10/10/10"
ords[["ex23"]] <- "mdy"
#standard[["ex23"]] <- "2010/10/10"

options( date.reader = list(nErrors = 2) )

for (name in names(dts)) {
  #cat("example: ", name, "\n")
  dt <- dts[[name]]
  
  wrn <- warn[[name]]
  if (is.null(wrn)) {
    wrn <- FALSE
  }
  
  if (wrn == FALSE) {
    expect_that(result <- as.POSIXct(dt), not(gives_warning()))
  } else {
    expect_warning(result <- as.POSIXct(dt))
  }
  
  ord <- ords[[name]]
  
  ord1 <- which.orders(dt)
  
  ord2 <- which.orders(dt, force=TRUE) # ignore autostart
  
  if (! is.na(ord2)) {
    suppressWarnings(
      result2 <- lubridate::parse_date_time(dt, ord2, tz="UTC")
    )
  } else {
    result2 <- rep(NA, length(dt))
  }
  
  #cat("example:", name, "orders:", ord, "guessed:", ord1, "\n")
  # cat("dt:", dt, "\n")
  # browser()
  expect_equivalent(
    .normalize_orders(ord)
    , .normalize_orders(ord2)
  )
  
  expect_equivalent( result, result2 )
  
  if (! is.na(ord1)) {
    expect_equivalent(
      .normalize_orders(ord)
      , .normalize_orders(ord1)
    )
  }
  
  if (! is.na(ord2)) {
    expect_is( result, 'POSIXct')
    expect_false(is.null(result))
  }
}
