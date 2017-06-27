#' get chicago public school data
#' @import data.table
#' @importFrom magrittr %>%
#' @param data_dir path to data folder.
#' @param force force a full import.#' @export

get_cps_address_data <- function(data_dir = "~/Dropbox/ra-work/spped/Data/", force = FALSE) {

  # data_dir = "~/Dropbox/ra-work/spped/Data/"
  # cat(sprintf("DEV LINE: data_dir = %s", data_dir))
  if(force) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE)

    is_addr <- grepl('address', basename(files))

    dta <- haven::read_dta(files[is_addr]) %>%
      dplyr::mutate_if(is.character, trimws)

    addr <- dta %>%
      dplyr::mutate(zip5 = gsub('^([0-9]{5}) ([0-9]{4})?', '\\1', zip)) %>%
      dplyr::mutate(address_full = paste(address_clean, "Chicago, IL", zip5, sep = ", "))

    uni_addr <- addr$address_full %>%
      unique()

    geocoded <- uni_addr %>%
      purrr::map_df(ggmap::geocode, output = 'more', source = 'google')
      dplyr::bind_cols(address_full = uni_addr, .)

    na_addr <- geocoded %>%
      dplyr::filter(is.na(lon)) %>%
      dplyr::pull(address_full)

    geo_na <- na_addr %>%
      purrr::map_df(ggmap::geocode, output = 'more', source = 'google')

    geo_full <- geo_na %>%
      dplyr::bind_cols(address_full = na_addr, .) %>%
      dplyr::bind_rows(geocoded %>% dplyr::filter(!is.na(lon)), .) %>%
      dplyr::arrange(address_full) %>%
      dplyr::select(address_full, lon, lat, address, street_number, postal_code)

    cps_2008_2016 <- dplyr::left_join(addr, geo_full, by = 'address_full') %>%
      dplyr::select(schoolidr_c_d_t_s, schoolname, year, address, lat, lon, charter, ever_address_change, address_full)

    devtools::use_data(cps_address, overwrite = TRUE)

    return(cps_address)

  } else {
    # check for data
    rda <- 'data/cps_address.rda'

    # if exists, load, otherwise, import using force
    if(file.exists('data/cps_address.rda')) {
      load('data/cps_address.rda')
      return(cps_address)
    } else {
      get_cps_address_data(data_dir, force = TRUE)
    }
  }

}

#' get chicago public school data
#' @inheretParams
#' @export

get_cps_2008_2016 <- function(data_dir = "~/Dropbox/ra-work/spped/Data/", force = FALSE) {

  if(force) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE)

    # tag non address data
    indx <- !grepl('address', basename(files))

    # get school address data with lat lon
    addr <- get_cps_address_data(data_dir) %>%
      dplyr::select(schoolidr_c_d_t_s, year, address, lat, lon, address_full)

    cps_2008_2016 <- haven::read_dta(files[indx]) %>%
      dplyr::mutate_if(is.character, trimws) %>%
      dplyr::left_join(., addr, by = c('schoolidr_c_d_t_s', 'year'))

    devtools::use_data(cps_2008_2016, overwrite = TRUE, compress = 'bzip2')

    return(cps_2008_2016)

  } else {
    # check for data
    rda <- 'data/cps_2008_2016.rda'

    # if exists, load, otherwise, import using force
    if(file.exists('data/cps_2008_2016.rda')) {
      load('data/cps_2008_2016.rda')
      return(cps_2008_2016)
    } else {
      get_cps_2008_2016(data_dir, force = TRUE)
    }
  }

}

#' get chicago public school data
#' @inheretParams
#' @export

get_cps_personnel <- function(data_dir = "~/Dropbox/ra-work/spped/RawData/CPS_Personnel/cooked", force = FALSE) {

  if(force) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE)

    # txt and csv
    txt_files <- files[grepl('\\.txt', basename(files))]
    csv_files <- files[grepl('\\.csv', basename(files))]

    # parse txt
    txt_cols <- c('position_number', 'budget_category', 'unit_number', 'other')
    txt <- txt_files %>%
      purrr::map(.x = ., .f = readr::read_delim, delim = "|", col_names = txt_cols, trim_ws = TRUE)

    # parse csv
    csv_cols <- c("position_number","unit_number","unit_name","fte","annual_salary","fte_annual_salary","annual_benefit_cost","job_code","job_description","employee_name")
    csv <- csv_files %>%
      purrr::map(.x = ., .f = readr::read_csv, col_names = csv_cols, trim_ws = TRUE)

    return(cps_2008_2016)

  } else {
    # check for data
    rda <- 'data/cps_2008_2016.rda'

    # if exists, load, otherwise, import using force
    if(file.exists('data/cps_2008_2016.rda')) {
      load('data/cps_2008_2016.rda')
      return(cps_2008_2016)
    } else {
      get_cps_personnel(data_dir, force = TRUE)
    }
  }

}



