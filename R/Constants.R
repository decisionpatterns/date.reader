#' Constants used internally to date.reader packaged
#' 
#' These are the constants used internally by the \code{date.reader} package. 
#' They are not exported.
#' 
#' @name date.regex
#' @rdname Constants


#' @rdname Constants
all.regex.names <- function() {
  return( names(kFormats) )
}

#' @rdname Constants
kMonthWords <- c(
    "january", "jan", "february", "feb",
    "march", "mar", "april", "april", "may", "june", "jun",
    "july", "jul", "august", "aug", "september", "sep",
    "october", "oct", "november", "nov", "december", "dec")

#' @rdname Constants
kDayWords <- c(
    "sunday", "sun", "su", "monday", "mon", "mo", "tuesday", 
    "tue", "tu", "wednesday", "wed", "we", "thursday", "thu", "th",
    "friday", "fri", "fr", "saturday", "sat", "sa")

#' @rdname Constants
kOptionalDayName <- paste(
    c("(((", paste(kDayWords, collapse=")|("), "))\\s*(,)?\\s+)?"), collapse="")

#' @rdname Constants
kDay <- "(the\\s+)?[0-3]?\\d((rd)|(th)|(st))?"

#' @rdname Constants
kSeparator1 <- "\\s*((,)|( ))\\s*"

#' @rdname Constants
kSeparator2 <- "\\s*( of )?\\s*"

#' @rdname Constants
kMonthName <- paste(
    c("((", paste(kMonthWords, collapse=")|("), "))"), collapse="")

#' @rdname Constants
kSeparator3 <- "\\s*(,)?\\s*"

#' @rdname Constants
kSeparator4 <- "\\s*(( )|(-)|(/))\\s*"

#' @rdname Constants
kYear <- "((\\d{2})|(\\d{4}))"

#' @rdname Constants
kYear4 <- "(\\d{4})"

#' @rdname Constants
kTimeDigit <- "((\\d)?\\d)"

#' @rdname Constants
kAt <- "((at )?(@\\s*)?)"

#' @rdname Constants
kAmPm <- "(\\s*((am)|(a\\.m\\.)|(pm)|(p\\.m\\.)))?"

#' @rdname Constants
kSeparator6 <- "\\s*((:)|( ))\\s*"

#' @rdname Constants
kH <- paste(
  c(
    kAt,
    kTimeDigit,
    kAmPm
  ), collapse=""
)

#' @rdname Constants
kHM <- paste(
  c(
    kAt,
    kTimeDigit,
    kSeparator6,
    kTimeDigit,
    kAmPm
  ), collapse=""
)

#' @rdname Constants
kHMS <- paste(
  c(
    kAt,
    kTimeDigit,
    kSeparator6,
    kTimeDigit,
    kSeparator6,
    kTimeDigit,
    kAmPm
  ),
  collapse=""
)

#' @rdname Constants
kMonthNum <- "((0?[1-9])|(1[0-2]))"

#' @rdname Constants
kMonth <- paste(
  c( "(", kMonthName, "|", kMonthNum, ")"), collapse=""
)

#' @rdname Constants
kMonth1 <- paste(
  c("(", kMonthName, "\\s*", ")"
  ), collapse=""
)

#' @rdname Constants
kMonth2 <- paste(
  c("(", kMonthNum, kSeparator4, ")"
  ), collapse=""
)

#' @rdname Constants
kMonth3 <- paste(
  c("(", kMonth1, "|", kMonth2, ")"), collapse=""
)

#' @rdname Constants
kDMY <- paste(
    c(kOptionalDayName, kDay, kSeparator2, kMonthName, 
        kSeparator3, kYear), collapse="")

#' @rdname Constants
kSeparator5 <- "\\s*((,)|( )|(-)|(/))\\s*"

#' @rdname Constants
kMDY <- paste(
    c(
    kOptionalDayName, 
    kMonth3,
    kDay,
    kSeparator5,
    kYear), collapse="")

#' @rdname Constants
kYMD.alt <- paste(
  c(
    kYear,
    kSeparator4,
    kMonth3,
    kDay
	), collapse=""
)

#' @rdname Constants
kYMD_H.alt <- paste(
  c( kYMD.alt,
     kSeparator1,
     kH 
  ), collapse=""
)

#' @rdname Constants
kYMD_HM.alt <- paste(
  c( kYMD.alt,
     kSeparator1,
     kHM
  ), collapse=""
)

#' @rdname Constants
kYMD_HMS.alt <- paste(
  c( kYMD.alt,
     kSeparator1,
     kHMS
  ), collapse=""
)
            
#' @rdname Constants
kMonth2 <- paste(
  c("(", kMonthNum, kSeparator4, ")"
  ), collapse=""
)

#' @rdname Constants
kMonth4 <- paste(
  c("(", "\\s*", kMonthName, "\\s*", ")"
  ), collapse=""
)

#' @rdname Constants
kMonth5 <- paste(
  c("(", kSeparator5, kMonthNum, kSeparator5, ")"
  ), collapse=""
)

#' @rdname Constants
kMonth6 <- paste(
  c("(", kMonth4, "|", kMonth5, ")"), collapse=""
)

#' @rdname Constants
kYMD <- paste(
    c(
        kYear4,
        kMonth6,
        kDay
    ), collapse="")

#' @rdname Constants
kDMY_H <- paste(
    c( kDMY,
        kSeparator1,
        kH 
    ), collapse=""
)

#' @rdname Constants
kDMY_HM <- paste(
    c( kDMY,
        kSeparator1,
        kHM
    ), collapse=""
)

#' @rdname Constants
kDMY_HMS <- paste(
    c( kDMY,
        kSeparator1,
        kHMS
    ), collapse=""
)

#' @rdname Constants
kMDY_H <- paste(
    c( kMDY,
        kSeparator1,
        kH 
    ), collapse=""
)

#' @rdname Constants
kMDY_HM <- paste(
    c( kMDY,
        kSeparator1,
        kHM
    ), collapse=""
)

#' @rdname Constants
kMDY_HMS <- paste(
    c( kMDY,
        kSeparator1,
        kHMS
    ), collapse=""
)

#' @rdname Constants
kYMD_H <- paste(
    c( kYMD,
        kSeparator1,
        kH 
    ), collapse=""
)

#' @rdname Constants
kYMD_HM <- paste(
    c( kYMD,
        kSeparator1,
        kHM
    ), collapse=""
)

#' @rdname Constants
kYMD_HMS <- paste(
    c( kYMD,
        kSeparator1,
        kHMS
    ), collapse=""
)

#' @rdname Constants
kYYYYMMDD <- "\\d{8}"

#' @rdname Constants
kYYYYMMDD_HH <- "\\d{10}"

#' @rdname Constants
kYYYYMMDD_HHMM <- "\\d{12}"

#' @rdname Constants
kYYYYMMDD_HHMMSS <- "\\d{14}"

#' @rdname Constants
kFormats <- list(
    ymd_hms.numeric=kYYYYMMDD_HHMMSS,
    ymd_hm.numeric=kYYYYMMDD_HHMM,
    ymd_h.numeric=kYYYYMMDD_HH,
    ymd.numeric=kYYYYMMDD,
    dmy_hms=kDMY_HMS,
    dmy_hm=kDMY_HM,
    dmy_h=kDMY_H,
    dmy=kDMY,
    mdy_hms=kMDY_HMS,
    mdy_hm=kMDY_HM,
    mdy_h=kMDY_H,
    mdy=kMDY,
    ymd_hms=kYMD_HMS,
    ymd_hm=kYMD_HM,
    ymd_h=kYMD_H,
    ymd=kYMD,
    ymd_hms.alt=kYMD_HMS.alt,
    ymd_hm.alt=kYMD_HM.alt,
    ymd_h.alt=kYMD_H.alt,
    ymd.alt=kYMD.alt
)



