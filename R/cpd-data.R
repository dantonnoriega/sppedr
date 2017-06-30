#' get chicago public school data
#' @param data_dir path to data folder.
#' @param force force a full import.
#' @export

get_cpd_crime <- function(data_dir = file.path(get_main_dir(), "RawData/Chicago_Crime/"), import = FALSE) {

  # data_dir = "~/Dropbox/ra-work/spped/Data/"
  # cat(sprintf("DEV LINE: data_dir = %s", data_dir))
  if(import) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE, pattern = '^crime.*csv$', ignore.case = TRUE)

    cpd_crime <- data.table::fread(files)
    data.table::setnames(cpd_crime, names(cpd_crime), gsub('[[:space:]]+', '_', tolower(names(cpd_crime))))
    data.table::setnames(cpd_crime, c('latitude', 'longitude', 'x_coordinate', 'y_coordinate'), c('lat', 'lon', 'x', 'y'))

    # get dates and filter out dates prior to 2007
    cpd_crime <- cpd_crime %>%
      .[, `:=`(ymd = fast.as.IDate(date, format = "%m/%d/%Y %H:%M:%S %p"), hms = fast.as.ITime(date, format = "%m/%d/%Y %H:%M:%S %p"))] %>%
      .[ymd > as.IDate("2007-01-01")]

    data.table::setkey(cpd_crime, ymd, hms)

    # drop some vars and edit others
    cpd_crime[, c('x', 'y', 'location', 'updated_on', 'date') := NULL]
    cpd_crime[, arrest := 0 + (arrest == 'true')]
    cpd_crime[, domestic := 0 + (domestic == 'true')]

    # filter cpd_crime file by dates
    # (1) between 2007 and 2016 calendar year dates
    ## school year span
    sy <- cps_school_year$school_year
    st <- sy$start
    ed <- sy$end

    ## off days
    off <- cps_school_year$days_off$date

    # during school year
    invisible(mapply(function(.x, .y) cpd_crime[data.table::between(ymd, .x, .y), during_school_year := 1] %>% .[is.na(during_school_year), during_school_year := 0], .x = st, .y = ed, SIMPLIFY = FALSE))

    # days off
    cpd_crime[, day_off_school_year := 0 + and(ymd %in% off, during_school_year == 1)]

    # weekdays and day/evening
    cpd_crime[, `:=`(dow = data.table::wday(ymd), dow_chr = format(ymd, "%a"))]
    cpd_crime[, weekday := 0 + (dow %in% c(2:6))]

    cpd_crime[, hour := data.table::hour(hms)]
    cpd_crime[, `:=`(day_hours = 0 + (hour %in% c(6:18)))]

    # (2) merge crimes
    # merge cpd_crime_codes
    cpd_crime <- data.table:::merge.data.table(cpd_crime, crime_codes$fbi, by = 'fbi_code', all.x = TRUE)

    data.table::fwrite(cpd_crime, file.path(get_main_dir(), "Data", "cpd_crime.csv"), logicalAsInt = TRUE)

    cpd_crime <- cpd_crime %>%
      dplyr::mutate(year = as.integer(year)) %>%
      tibble::as_tibble()

    return(cpd_crime)

  } else {
    # check for data
    tiny <- data.table::fread(file.path(get_main_dir(), "Data", "cpd_crime.csv"), encoding="UTF-8", nrows = 5)
    nms  <- names(tiny)
    nc   <- ncol(tiny)
    int  <- c("arrest", "domestic", "beat", "district", "ward", "community_area", "year", "lat", "lon", "during_school_year", "day_off_school_year", "weekday", "dow", "hour", "day_hours") # integer cols
    indx <- which(nms %in% int)

    # set col classes
    cols_class <- rep('character', nc) %>%
      '[<-'(indx, 'integer')

    cpd_crime <- data.table::fread(file.path(get_main_dir(), "Data", "cpd_crime.csv"), encoding="UTF-8", colClasses = cols_class, na.strings = c("", "NA")) %>%
      .[, `:=`(ymd = fast.as.IDate(ymd), hms = fast.as.ITime(hms, format = "%H:%M:%S"))] %>%
      '['() %>%
      tibble::as_tibble()

    return(cpd_crime)

  }

}
