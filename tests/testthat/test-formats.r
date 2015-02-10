library(testthat)
library(lubridate)

context( "test-formats.R")

.normalize_orders <- function(txt) {
  if (is.na(txt)) return(NA)
  txt <- gsub("_", "", txt)
  txt <- tolower(txt)
}

to.standard <- list()
to.standard[["ymd"]] = "ymd"
to.standard[["ymd_h"]] = "ymd_h"
to.standard[["ymd_hm"]] = "ymd_hm"
to.standard[["ymd_hms"]] = "ymd_hms"
to.standard[["dmy"]] = "ymd"
to.standard[["dmy_h"]] = "ymd_h"
to.standard[["dmy_hm"]] = "ymd_hm"
to.standard[["dmy_hms"]] = "ymd_hms"
to.standard[["mdy"]] = "ymd"
to.standard[["mdy_h"]] = "ymd_h"
to.standard[["mdy_hm"]] = "ymd_hm"
to.standard[["mdy_hms"]] = "ymd_hms"
dts <- list()
expected <- list()
standard <- list()

dts[["ex1"]] <- "20131223"
expected[["ex1"]] <- "ymd"
standard[["ex1"]] <- "2013/12/23"

dts[["ex2"]] <- "2013122312"
expected[["ex2"]] <- "ymd_h"
standard[["ex2"]] <- "2013/12/23 12"

dts[["ex3"]] <- "201312231222"
expected[["ex3"]] <- "ymd_hm"
standard[["ex3"]] <- "2013/12/23 12:22"

dts[["ex4"]] <- "20131223122234"
expected[["ex4"]] <- "ymd_hms"
standard[["ex4"]] <- "2013/12/23 12:22:34"

dts[["ex5"]] <- "20131223 12 am"
expected[["ex5"]] <- "ymd_h"
standard[["ex5"]] <- "2013/12/23 12 am"

dts[["ex6"]] <- "20131223 12:22 am"
expected[["ex6"]] <- "ymd_hm"
standard[["ex6"]] <- "2013/12/23 12:22 am"

dts[["ex7"]] <- "20131223 12:22:34 am"
expected[["ex7"]] <- "ymd_hms"
standard[["ex7"]] <- "2013/12/23 12:22:34 am"

dts[["ex7"]] <- "10/10/10"
expected[["ex7"]] <- "mdy"
standard[["ex7"]] <- "2010/10/10"

dts[["ex8"]] <- "13/10/10"
expected[["ex8"]] <- "dmy"
standard[["ex8"]] <- "2010/10/13"

dts[["ex9"]] <- "32/10/10"
expected[["ex9"]] <- "ymd"
standard[["ex9"]] <- "2032/10/10"


dts[["ex10"]] <- "January 12, 2015"
expected[["ex10"]] <- "mdy"
standard[["ex10"]] <- "2015/01/12"


dts[["ex11"]] <- "12th of January, 2015"
expected[["ex11"]] <- "dmy"
standard[["ex11"]] <- "2015/01/12"

for (name in names(dts)) {
  dt <- dts[[name]]
  exp <- expected[[name]]
  std <- standard[[name]]
  ord.std <- to.standard[[exp]]
  guess <- which.orders(dt)
  z1 <- .parse.date.strict(dt, exp)
  z2 <- lubridate::parse_date_time(std, ord.std)
  expect_equivalent(
    .normalize_orders(exp)
    , .normalize_orders(guess))
  expect_equivalent(z1, z2)
  cat("example:", name, "dt:", dt, "expected:", exp, "guessed:", guess, "parsed:", z1, "standard:", std, "parsed std:", z2, "\n")
}

