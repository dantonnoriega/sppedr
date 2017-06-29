#' get chicago public school data
#' @param data_dir path to data folder.
#' @param force force a full import.
#' @export

get_cpd_crime_data <- function(data_dir = file.path(get_main_dir(), "RawData/Chicago_Crime/"), import = FALSE) {

  # data_dir = "~/Dropbox/ra-work/spped/Data/"
  # cat(sprintf("DEV LINE: data_dir = %s", data_dir))
  if(import) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE, pattern = '^crime.*csv$', ignore.case = TRUE)

    csv <- readr::read_csv(files, trim_ws = TRUE) %>%
      setNames(tolower(gsub('[[:space:]]+', '_', names(.)))) %>%
      dplyr::rename(lat = latitude, lon = longitude, x = x_coordinate, y = y_coordinate) %>%
      dplyr::mutate(date = strptime(date, format = "%m/%d/%Y %H:%M:%S %p") %>% as.POSIXct(tz = "GMT")) %>%
      dplyr::mutate(ymd = as.Date(format(date, "%Y-%m-%d")), hms = format(date, "%T")) %>%
      dplyr::select(id, case_number, date, ymd, hms, dplyr::everything())

    # filter csv file by dates
    # (1) filter violent crimes
    # (2) between 2007 and 2016 school year dates
    ## school year span
    sy <- cps_school_year$school_year
    st <- sy$start
    ed <- sy$end

    ## off days
    off <- cps_school_year$days_off

    csv2 <- csv %>%
      dplyr::filter(dplyr::between(ymd, st[1], ed[nrow(sy)])) %>%
      purrr::map2_df(.x = st, .y = ed, .f = ~dplyr::filter(., dplyr::between(ymd, .x, .y)))

    devtools::use_data(cpd_crime, overwrite = TRUE)

    return(cpd_crime)

  } else {
    # check for data
    csv <- 'data/cpd_crime.rda'

    # if exists, load, otherwise, import using force
    if(file.exists('data/cpd_crime.rda')) {
      load('data/cpd_crime.rda')
      return(cpd_crime)
    } else {
      get_cpd_crime_data(data_dir, force = TRUE)
    }
  }

}
