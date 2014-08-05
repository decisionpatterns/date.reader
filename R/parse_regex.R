library(lubridate)

kTrials <- 10
kErrors <- 5
kSuccesses <- 5

kMonthWords <- c(
    "january", "jan", "february", "feb",
    "march", "mar", "april", "april", "may", "june", "jun",
    "july", "jul", "august", "aug", "september", "sep",
    "october", "oct", "november", "nov", "december", "dec")

kDayWords <- c(
    "sunday", "sun", "su", "monday", "mon", "mo", "tuesday", 
    "tue", "tu", "wednesday", "wed", "we", "thursday", "thu", "th",
    "friday", "fri", "fr", "saturday", "sat", "sa")

kOptionalDayName <- paste(
    c("(((", paste(kDayWords, collapse=")|("), "))\\s*(,)?\\s+)?"), collapse="")

kDay <- "(the\\s+)?[0-3]?\\d((rd)|(th)|(st))?"

kSeparator2 <- "\\s*( of )?\\s*"

kMonthName <- paste(
    c("((", paste(kMonthWords, collapse=")|("), "))"), collapse="")

kSeparator3 <- "\\s*(,)?\\s*"

kSeparator4 <- "\\s*(( )|(-)|(/))\\s*"

kYear <- "((\\d{2})|(\\d{4}))"

kYear4 <- "(\\d{4})"

kMonthNum <- "((0?[1-9])|(1[0-2]))"

kMonth <- paste(
c("(", kMonthName, "|", kMonthNum, ")"), collapse="")

kMonth1 <- paste(
  c("(", kMonthName, "\\s*", ")"
  ), collapse=""
)

kMonth2 <- paste(
  c("(", kMonthNum, kSeparator4, ")"
  ), collapse=""
)

kMonth3 <- paste(
  c("(", kMonth1, "|", kMonth2, ")"), collapse=""
)

kDMY <- paste(
    c(kOptionalDayName, kDay, kSeparator2, kMonthName, 
        kSeparator3, kYear), collapse="")

kSeparator5 <- "\\s*((,)|( )|(-)|(/))\\s*"

kMDY <- paste(
    c(
    kOptionalDayName, 
    kMonth3,
    kDay,
    kSeparator5,
    kYear), collapse="")

kMonth4 <- paste(
  c("(", "\\s*", kMonthName, "\\s*", ")"
  ), collapse=""
)

kMonth5 <- paste(
  c("(", kSeparator5, kMonthNum, kSeparator5, ")"
  ), collapse=""
)

kMonth6 <- paste(
  c("(", kMonth4, "|", kMonth5, ")"), collapse=""
)

kYMD <- paste(
    c(
        kYear4,
        kMonth6,
        kDay
    ), collapse="")

kTimeDigit <- "((\\d)?\\d)"

kAt <- "((at )?(@\\s*)?)"

kAmPm <- "(\\s*((am)|(a\\.m\\.)|(pm)|(p\\.m\\.)))?"

kSeparator6 <- "\\s*((:)|( ))\\s*"

kH <- paste(
    c(
        kAt,
        kTimeDigit,
        kAmPm
    ), collapse=""
)

kHM <- paste(
    c(
        kAt,
        kTimeDigit,
        kSeparator6,
        kTimeDigit,
        kAmPm
    ), collapse=""
)

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

kSeparator1 <- "\\s*((,)|( ))\\s*"

kDMY_H <- paste(
    c( kDMY,
        kSeparator1,
        kH 
    ), collapse=""
)

kDMY_HM <- paste(
    c( kDMY,
        kSeparator1,
        kHM
    ), collapse=""
)

kDMY_HMS <- paste(
    c( kDMY,
        kSeparator1,
        kHMS
    ), collapse=""
)

kMDY_H <- paste(
    c( kMDY,
        kSeparator1,
        kH 
    ), collapse=""
)

kMDY_HM <- paste(
    c( kMDY,
        kSeparator1,
        kHM
    ), collapse=""
)

kMDY_HMS <- paste(
    c( kMDY,
        kSeparator1,
        kHMS
    ), collapse=""
)

kYMD_H <- paste(
    c( kYMD,
        kSeparator1,
        kH 
    ), collapse=""
)

kYMD_HM <- paste(
    c( kYMD,
        kSeparator1,
        kHM
    ), collapse=""
)

kYMD_HMS <- paste(
    c( kYMD,
        kSeparator1,
        kHMS
    ), collapse=""
)

kYYYYMMDD <- "\\d{8}"
kYYYYMMDD_HH <- "\\d{10}}"
kYYYYMMDD_HHMM <- "\\d{12}}"
kYYYYMMDD_HHMMSS <- "\\d{14}"

kFormats <- c(
    kYYYYMMDD_HHMMSS,
    kYYYYMMDD_HHMM,
    kYYYYMMDD_HH,
    kYYYYMMDD,
    kDMY_HMS,
    kDMY_HM,
    kDMY_H,
    kDMY,
    kMDY_HMS,
    kMDY_HM,
    kMDY_H,
    kMDY,
    kYMD_HMS,
    kYMD_HM,
    kYMD_H,
    kYMD
)

kLubridateFormats <- c(
    ymd_hms,
    ymd_hm,
    ymd_h,
    ymd,
    dmy_hms,
    dmy_hm,
    dmy_h,
    dmy,
    mdy_hms,
    mdy_hm,
    mdy_h,
    mdy,
    ymd_hms,
    ymd_hm,
    ymd_h,
    ymd
)

kFormatNames <- c(
    "YYYYMMDD",
    "YYYYMMDD_H",
    "YYYYMMDD_HM",
    "YYYYMMDD_HMS",
    "DMY_HMS",
    "DMY_HM",
    "DMY_H",
    "DMY",
    "MDY_HMS",
    "MDY_HM",
    "MDY_H",
    "MDY",
    "YMD_HMS",
    "YMD_HM",
    "YMD_H",
    "YMD"
)

lookup.format <- function(name) {
    w <- match(name, kFormatNames)
    if (is.na(w)) {
        return(NA)
    }
    return(kFormats[[w]])
}

lookup.lubridate.fun <- function(name) {
    w <- match(name, kFormatNames)
    if (is.na(w)) {
        return(NA)
    }
    return(kLubridateFormats[[w]])
}
