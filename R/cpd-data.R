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

    csv <- readr::read_csv(files, trim_ws = TRUE, n_max = 1000) %>%
      setNames(tolower(gsub('[[:space:]]+', '_', names(.)))) %>%
      dplyr::rename(lat = latitude, lon = longitude, x = x_coordinate, y = y_coordinate) %>%
      dplyr::mutate(date = strptime(date, format = "%m/%d/%Y %H:%M:%S %p") %>% as.POSIXct(tz = "GMT"))

    # filter csv file by dates

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
