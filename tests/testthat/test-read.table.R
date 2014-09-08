library(lubridate)
library(testthat)
library(date.reader)

name <- c("fred", "bob", "sally")
birthday <- c("January 12, 1954", "February 1, 1990", "March 31, 1980")
weight <- c("170", "154", "124")
table <- data.frame(name, birthday, weight)
conn <- textConnection("txt", 'w')
write.table(table, file=conn, quote=FALSE, sep="\t", row.names=FALSE)
# now, txt holds the table

conn <- textConnection(txt)
#table.new <- read.table(conn, header=TRUE, sep="\t")
table.new <- read.table(conn, header=TRUE, sep="\t", colClasses=c("factor", "POSIXct", "character"))
dates <- table.new[[2]]
expect_equivalent(dates[[1]], mdy("01/12/1954"))
expect_equivalent(dates[[2]], mdy("02/01/1990"))
expect_equivalent(dates[[3]], mdy("03/31/1980"))

close(conn)
conn <- textConnection(txt)
table.new <- read.table(conn, header=TRUE, sep="\t")
dates <- table.new[[2]]
expect_equivalent(dates[[1]], mdy("01/12/1954"))
expect_equivalent(dates[[2]], mdy("02/01/1990"))
expect_equivalent(dates[[3]], mdy("03/31/1980"))

print("done with test-read.table.R")