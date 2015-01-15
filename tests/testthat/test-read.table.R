library(lubridate)
library(testthat)
library(date.reader)

context('read.table')

file = tempfile()

name <- c("fred", "bob", "sally")
birthday <- c("January 12, 1954", "February 1, 1990", "March 31, 1980")
weight <- c("170", "154", "124")
table <- data.frame(name, birthday, weight)

file = tempfile()
write.table(table, file=file, quote=FALSE, sep="\t", row.names=FALSE)


table.new <- read.table( file, header=TRUE, sep="\t", colClasses=c("factor", "POSIXct", "character"))
dates <- table.new[[2]]
expect_equivalent( dates[[1]], mdy("01/12/1954") )
expect_equivalent( dates[[2]], mdy("02/01/1990") )
expect_equivalent( dates[[3]], mdy("03/31/1980") )


table.new <- read.table( file, header=TRUE, sep="\t" )
dates <- table.new[[2]]
expect_equivalent( dates[[1]], mdy("01/12/1954") )
expect_equivalent( dates[[2]], mdy("02/01/1990") )
expect_equivalent( dates[[3]], mdy("03/31/1980") )


# CLEAN-UP
file.remove( file )
