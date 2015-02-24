library(lubridate)
library(testthat)
library(date.reader)

context('read.table')

# CREATE TABLE
name <- c("fred", "bob", "sally")
birthday <- c("January 12, 1954", "February 1, 1990", "March 31, 1980")
weight <- c("170", "154", "124")
table <- data.frame(name, birthday, weight)


# TESTS:

## Specified classes
table.new <- parse.dates(table, colClasses=c("factor", "POSIXct", "character"))
dates <- table.new[[2]]

expect_equivalent( dates[[1]], mdy("01/12/1954") )
expect_equivalent( dates[[2]], mdy("02/01/1990") )
expect_equivalent( dates[[3]], mdy("03/31/1980") )


## Unspecified classes  
table.new <- parse.dates( table )
dates <- table.new[[2]]

expect_equivalent( dates[[1]], mdy("01/12/1954") )
expect_equivalent( dates[[2]], mdy("02/01/1990") )
expect_equivalent( dates[[3]], mdy("03/31/1980") )

library(data.table)
table1 <- data.table(table)
parse.dates(table1, colClasses=c("factor", "POSIXct", "character"))
# modifies its argument
dates <- table1[[2]]

expect_equivalent( dates[[1]], mdy("01/12/1954") )
expect_equivalent( dates[[2]], mdy("02/01/1990") )
expect_equivalent( dates[[3]], mdy("03/31/1980") )

## Unspecified classes
table1 <- data.table(table)
parse.dates(table1)
dates <- table1[[2]]

expect_equivalent( dates[[1]], mdy("01/12/1954") )
expect_equivalent( dates[[2]], mdy("02/01/1990") )
expect_equivalent( dates[[3]], mdy("03/31/1980") )

