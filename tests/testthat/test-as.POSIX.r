library(testthat)
library(lubridate)

context( "as.POSIX.R")

.normalize_orders <- function(txt) {
  if (is.na(txt)) return(NA)
  txt <- gsub("_", "", txt)
  txt <- tolower(txt)
}

dts <- list()
ords <- list()
warn <- list()

  dts[["ex1"]]  <- rep("10/10/10", 20)
  ords[["ex1"]] <- "mdy"
  
  dts[["ex2"]]  <- c(rep("10/10/10", 20), "13/10/10")
  ords[["ex2"]] <- "dmy"
  
  dts[["ex3"]]  <- c(rep("10/10/10", 20), "gibberish")
  ords[["ex3"]] <- "mdy"
  warn[["ex3"]] <- TRUE
  
  dts[["ex4"]]  <- c(rep("10/10/10", 20), rep("gibberish", 6))
  ords[["ex4"]] <- NA # shouldn't parse--too many errors
  warn[["ex4"]] <- TRUE
  
  dts[["ex5"]]  <- c( '20140210', '19791118', '19720329' )
  ords[["ex5"]] <- "ymd"
  
  dts[["ex6"]]  <- c('January 31, 2011', 'March 23, 1957', 'October 26, 1929')
  ords[["ex6"]] <- "mdy"
  
  dts[["ex7"]]  <- c("10/22/1977 12:33 pm", "9/1/1997 10:33 am", "12/25/1942 12:00 am")
  ords[["ex7"]] <- "mdy_hm"
  
  dts[["ex8"]]  <- c("10/12/25", "14/11/13", "09/03/29")
  ords[["ex8"]] <- "dmy"


options( date.reader = list(nErrors = 2) )

for (name in names(dts)) {
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

  if (! is.na(ord)) {
  suppressWarnings(
    result2 <- lubridate::parse_date_time(dt, ord, tz="UTC")
  )
  } else {
    result2 <- NA
  }
  
  # cat("example:", name, "orders:", ord, "guessed:", ord1, "\n")
  # cat("dt:", dt, "\n")
  # browser()
  expect_equivalent(
      .normalize_orders(ord)
    , .normalize_orders(ord1)
  )
  
  if (! is.na(ord)) {
    expect_equivalent(
        .normalize_orders(ord)
      , .normalize_orders(ord2)
    )
  }
  if (! is.na(ord2)) {
    expect_is( result, 'POSIXct')
    expect_false(is.null(result))
  }
}
