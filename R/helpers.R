#' fast IDate function
#' @param x character string
#' @param format date/time parsing format
#' @export

fast.as.IDate <- function(x, format = "%Y-%m-%d", ...) data.table::as.IDate(lubridate::fast_strptime(x, format = format, ...))

#' fast ITime function
#' @param x character string
#' @param format date/time parsing format
#' @export
fast.as.ITime <- function(x, format = "%H:%M:%S", ...) data.table::as.ITime(lubridate::fast_strptime(x, format = format, ...))
