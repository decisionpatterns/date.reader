#' read.table
#' 
#' @param ... additional arguments passed to \code{utils::read.table}
#' @param file the file or connection to read the table from
#'
#' @return data frame. 
#' 
#' If a character column can be interpreted as dates, then it translates the 
#' value into POSIXct objects.  Otherwise, it acts as utils::read.table
#'
#' @seealso 
#'   \code{\link[base]{as.POSIXct}}
#'   
#' @examples
#'   name <- c("bob", "fred", "sally")
#'   birthday <- c("01/22/1993", "02/25/1980", "03/31/1970")
#'   birthday <- as.factor(birthday)
#'   table <- data.frame(name,birthday)
#'   
#'   conn <- textConnection("read.table.txt", 'w')
#'   write.table(table, file=conn, quote=FALSE, sep="\t", row.names=FALSE)
#'   
#'   conn <- textConnection(read.table.txt)
#'   table.new <- read.table(conn, header=TRUE, sep="\t")
#'      
#' @export

read.table <- function(file, ...) {
  args <- list(file, ...)

  colClasses.old <- args[['colClasses']]
  colClasses <- colClasses.old
  
  # two cases: colClasses has named elements
  nams <- names(colClasses)
  nonempty.names <- setdiff(nams, "")
  named.colClass.elements <- (length(nonempty.names) > 0)
  
  if (is.null(colClasses)) {
    args[['colClasses']] <- NA
  } else {
    if ("POSIXct" %in% colClasses) {
      indices <- which(colClasses == "POSIXct")
      colClasses[indices] <- "character"
    }
#     if ("POSIXlt" %in% colClasses) {
#       indices <- which(colClasses == "POSIXlt")
#       colClasses[indices] <- "character"
#     }
    args[['colClasses']] <- colClasses
  }
  
  dat <- do.call(utils::read.table, args)
  colClasses <- colClasses.old

  nams <- names(dat)
  cls.index <- 0
  as.is <- args[['as.is']]
  typ <- typeof(as.is)
  as.is.logical <- (typ == "logical")
  as.is.index <- 0

  for (col.idx in seq_len(ncol(dat))) {
    name <-  nams[[col.idx]]
    if (as.is.index == length(as.is)) {
        as.is.index <- 0 #recycle
    }
    as.is.index <- as.is.index + 1
    if (is.null(as.is)) {
        as.is.current <- FALSE
    } else if (as.is.logical) {
      as.is.current <- as.is[[as.is.index]]
      if (is.null(as.is.current) || is.na(as.is.current)) {
        as.is.current <- FALSE
      }
    } else {
        common <- intersect(as.is, c(name, col.idx))
        as.is.current <- (length(common) > 0)
    }
    if (as.is.current) {
        next
    }    

    if (cls.index == length(colClasses)) {
        cls.index <- 0
    }
    cls.index <- cls.index+1

    if (is.null(colClasses)) {
        cls <- NA
    } else if (named.colClass.elements) {
        cls <- colClasses[[name]]
        if (is.null(cls)) {
            cls <- NA
        }
    } else {
        cls <- colClasses[[cls.index]]
        if (is.null(cls)) {
            cls <- NA
        }
    }
      
    if (! is.na(cls)) {
      if (cls != "POSIXct")
        next # assume that utils::read.table did that right thing
    }

    x <- dat[, col.idx]
    #nErrors <- getOption("date.reader.nErrors") 
    #autostart <- getOption("date.reader.autostart")
        
    orders <- which.orders(x)
    tz <- option.tools::getOption("date.reader.tz")
    if (!is.na(orders)) {
      x <- lubridate::parse_date_time(x, orders, tz=tz)
      if (! is.na(cls) ) {
        if (cls == "POSIXlt") {
          x <- as.POSIXlt(x)
        }
      }
      dat[, col.idx] <- x
      next
    }
  }
  
  return(dat)
}

# my.read.table <- function (file, header = FALSE, sep = "", quote = "\"'", dec = ".", 
#           numerals = c("allow.loss", "warn.loss", "no.loss"), row.names, 
#           col.names, as.is = !stringsAsFactors, na.strings = "NA", 
#           colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, 
#           fill = !blank.lines.skip, strip.white = FALSE, blank.lines.skip = TRUE, 
#           comment.char = "#", allowEscapes = FALSE, flush = FALSE, 
#           stringsAsFactors = default.stringsAsFactors(), fileEncoding = "", 
#           encoding = "unknown", text, skipNul = FALSE) 
# {
#   if (missing(file) && !missing(text)) {
#     file <- textConnection(text, encoding = "UTF-8")
#     encoding <- "UTF-8"
#     on.exit(close(file))
#   }
#   if (is.character(file)) {
#     file <- if (nzchar(fileEncoding)) 
#       file(file, "rt", encoding = fileEncoding)
#     else file(file, "rt")
#     on.exit(close(file))
#   }
#   if (!inherits(file, "connection")) 
#     stop("'file' must be a character string or connection")
#   if (!isOpen(file, "rt")) {
#     open(file, "rt")
#     on.exit(close(file))
#   }
#   pbEncoding <- if (encoding %in% c("", "bytes", "UTF-8")) 
#     encoding
#   else "bytes"
#   numerals <- match.arg(numerals)
#   if (skip > 0L) 
#     readLines(file, skip)
#   nlines <- n0lines <- if (nrows < 0L) 
#     5
#   else min(5L, (header + nrows))
#   lines <- .External(utils:::C_readtablehead, file, nlines, comment.char, 
#                      blank.lines.skip, quote, sep, skipNul)
#   if (encoding %in% c("UTF-8", "latin1")) 
#     Encoding(lines) <- encoding
#   nlines <- length(lines)
#   if (!nlines) {
#     if (missing(col.names)) 
#       stop("no lines available in input")
#     rlabp <- FALSE
#     cols <- length(col.names)
#   }
#   else {
#     if (all(!nzchar(lines))) 
#       stop("empty beginning of file")
#     if (nlines < n0lines && file == 0L) {
#       pushBack(c(lines, lines, ""), file, encoding = pbEncoding)
#       on.exit((clearPushBack(stdin())))
#     }
#     else pushBack(c(lines, lines), file, encoding = pbEncoding)
#     first <- scan(file, what = "", sep = sep, quote = quote, 
#                   nlines = 1, quiet = TRUE, skip = 0, strip.white = TRUE, 
#                   blank.lines.skip = blank.lines.skip, comment.char = comment.char, 
#                   allowEscapes = allowEscapes, encoding = encoding, 
#                   skipNul = skipNul)
#     col1 <- if (missing(col.names)) 
#       length(first)
#     else length(col.names)
#     col <- numeric(nlines - 1L)
#     if (nlines > 1L) 
#       for (i in seq_along(col)) col[i] <- length(scan(file, 
#                                                       what = "", sep = sep, quote = quote, nlines = 1, 
#                                                       quiet = TRUE, skip = 0, strip.white = strip.white, 
#                                                       blank.lines.skip = blank.lines.skip, comment.char = comment.char, 
#                                                       allowEscapes = allowEscapes, encoding = encoding, 
#                                                       skipNul = skipNul))
#     cols <- max(col1, col)
#     rlabp <- (cols - col1) == 1L
#     if (rlabp && missing(header)) 
#       header <- TRUE
#     if (!header) 
#       rlabp <- FALSE
#     if (header) {
#       .External(utils:::C_readtablehead, file, 1L, comment.char, 
#                 blank.lines.skip, quote, sep, skipNul)
#       if (missing(col.names)) 
#         col.names <- first
#       else if (length(first) != length(col.names)) 
#         warning("header and 'col.names' are of different lengths")
#     }
#     else if (missing(col.names)) 
#       col.names <- paste0("V", 1L:cols)
#     if (length(col.names) + rlabp < cols) 
#       stop("more columns than column names")
#     if (fill && length(col.names) > cols) 
#       cols <- length(col.names)
#     if (!fill && cols > 0L && length(col.names) > cols) 
#       stop("more column names than columns")
#     if (cols == 0L) 
#       stop("first five rows are empty: giving up")
#   }
#   if (check.names) 
#     col.names <- make.names(col.names, unique = TRUE)
#   if (rlabp) 
#     col.names <- c("row.names", col.names)
#   nmColClasses <- names(colClasses)
#   if (length(colClasses) < cols) 
#     if (is.null(nmColClasses)) {
#       colClasses <- rep_len(colClasses, cols)
#     }
#   else {
#     tmp <- rep_len(NA_character_, cols)
#     names(tmp) <- col.names
#     i <- match(nmColClasses, col.names, 0L)
#     if (any(i <= 0L)) 
#       warning("not all columns named in 'colClasses' exist")
#     tmp[i[i > 0L]] <- colClasses
#     colClasses <- tmp
#   }
#   what <- rep.int(list(""), cols)
#   names(what) <- col.names
#   colClasses[colClasses %in% c("real", "double")] <- "numeric"
#   known <- colClasses %in% c("logical", "integer", "numeric", 
#                              "complex", "character", "raw")
#   what[known] <- sapply(colClasses[known], do.call, list(0))
#   what[colClasses %in% "NULL"] <- list(NULL)
#   keep <- !sapply(what, is.null)
#   data <- scan(file = file, what = what, sep = sep, quote = quote, 
#                dec = dec, nmax = nrows, skip = 0, na.strings = na.strings, 
#                quiet = TRUE, fill = fill, strip.white = strip.white, 
#                blank.lines.skip = blank.lines.skip, multi.line = FALSE, 
#                comment.char = comment.char, allowEscapes = allowEscapes, 
#                flush = flush, encoding = encoding, skipNul = skipNul)
#   nlines <- length(data[[which.max(keep)]])
#   if (cols != length(data)) {
#     warning("cols = ", cols, " != length(data) = ", length(data), 
#             domain = NA)
#     cols <- length(data)
#   }
#   if (is.logical(as.is)) {
#     as.is <- rep_len(as.is, cols)
#   }
#   else if (is.numeric(as.is)) {
#     if (any(as.is < 1 | as.is > cols)) 
#       stop("invalid numeric 'as.is' expression")
#     i <- rep.int(FALSE, cols)
#     i[as.is] <- TRUE
#     as.is <- i
#   }
#   else if (is.character(as.is)) {
#     i <- match(as.is, col.names, 0L)
#     if (any(i <= 0L)) 
#       warning("not all columns named in 'as.is' exist")
#     i <- i[i > 0L]
#     as.is <- rep.int(FALSE, cols)
#     as.is[i] <- TRUE
#   }
#   else if (length(as.is) != cols) 
#     stop(gettextf("'as.is' has the wrong length %d  != cols = %d", 
#                   length(as.is), cols), domain = NA)
#   do <- keep & !known
#   if (rlabp) 
#     do[1L] <- FALSE
#   for (i in (1L:cols)[do]) {
#     data[[i]] <- if (is.na(colClasses[i])) 
#       type.convert(data[[i]], as.is = as.is[i], dec = dec, 
#                    numerals = numerals, na.strings = character(0L))
#     else if (colClasses[i] == "factor") 
#       as.factor(data[[i]])
#     else if (colClasses[i] == "Date") 
#       as.Date(data[[i]])
#     else if (colClasses[i] == "POSIXct") 
#       as.POSIXct(data[[i]])
#     else methods::as(data[[i]], colClasses[i])
#   }
#   compactRN <- TRUE
#   if (missing(row.names)) {
#     if (rlabp) {
#       row.names <- data[[1L]]
#       data <- data[-1L]
#       keep <- keep[-1L]
#       compactRN <- FALSE
#     }
#     else row.names <- .set_row_names(as.integer(nlines))
#   }
#   else if (is.null(row.names)) {
#     row.names <- .set_row_names(as.integer(nlines))
#   }
#   else if (is.character(row.names)) {
#     compactRN <- FALSE
#     if (length(row.names) == 1L) {
#       rowvar <- (1L:cols)[match(col.names, row.names, 0L) == 
#                             1L]
#       row.names <- data[[rowvar]]
#       data <- data[-rowvar]
#       keep <- keep[-rowvar]
#     }
#   }
#   else if (is.numeric(row.names) && length(row.names) == 1L) {
#     compactRN <- FALSE
#     rlabp <- row.names
#     row.names <- data[[rlabp]]
#     data <- data[-rlabp]
#     keep <- keep[-rlabp]
#   }
#   else stop("invalid 'row.names' specification")
#   data <- data[keep]
#   if (is.object(row.names) || !(is.integer(row.names))) 
#     row.names <- as.character(row.names)
#   if (!compactRN) {
#     if (length(row.names) != nlines) 
#       stop("invalid 'row.names' length")
#     if (anyDuplicated(row.names)) 
#       stop("duplicate 'row.names' are not allowed")
#     if (anyNA(row.names)) 
#       stop("missing values in 'row.names' are not allowed")
#   }
#   class(data) <- "data.frame"
#   attr(data, "row.names") <- row.names
#   data
# }