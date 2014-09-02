

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

kYMD.alt <- paste(
  c(
    kYear,
    kSeparator4,
    kMonth3,
    kDay), collapse="")

kYMD_H.alt <- paste(
  c( kYMD.alt,
     kSeparator1,
     kH 
  ), collapse=""
)

kYMD_HM.alt <- paste(
  c( kYMD.alt,
     kSeparator1,
     kHM
  ), collapse=""
)

kYMD_HMS.alt <- paste(
  c( kYMD.alt,
     kSeparator1,
     kHMS
  ), collapse=""
)
            
kMonth2 <- paste(
  c("(", kMonthNum, kSeparator4, ")"
  ), collapse=""
)

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
kYYYYMMDD_HH <- "\\d{10}"
kYYYYMMDD_HHMM <- "\\d{12}"
kYYYYMMDD_HHMMSS <- "\\d{14}"

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

kLubridateFormats <- list(
  ymd_hms.numeric=lubridate::ymd_hms,
  ymd_hm.numeric=lubridate::ymd_hm,
  ymd_h.numeric=lubridate::ymd_h,
  ymd.numeric=lubridate::ymd,
  dmy_hms=lubridate::dmy_hms,
  dmy_hm=lubridate::dmy_hm,
  dmy_h=lubridate::dmy_h,
  dmy=lubridate::dmy,
  mdy_hms=lubridate::mdy_hms,
  mdy_hm=lubridate::mdy_hm,
  mdy_h=lubridate::mdy_h,
  mdy=lubridate::mdy,
  ymd_hms=lubridate::ymd_hms,
  ymd_hm=ymd_hm,
  ymd_h=lubridate::ymd_h,
  ymd=lubridate::ymd,
  ymd_hms.alt=lubridate::ymd_hms,
  ymd_hm.alt=lubridate::ymd_hm,
  ymd_h.alt=lubridate::ymd_h,
  ymd.alt=lubridate::ymd
)

lookup.regex <- function(name) {
    name <- tolower(name)
    retval <- kFormats[[name]]
    if (is.null(retval)) {
      return(NA)
    }
    return(retval)
}

lookup.lubridate.fun <- function(name) {
    name <- tolower(name)
    retval <- kLubridateFormats[[name]]
    if (is.null(retval)) {
      return(NA)
    }
    return(retval)
}
