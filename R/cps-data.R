#' get chicago public school data
#' @import data.table
#' @importFrom magrittr %>%
#' @param data_dir path to data folder.
#' @param force force a full import.
#' @param replace replace existing package data with new import.
#' @export

get_cps_address <- function(data_dir = file.path(get_main_dir(), "Data"), force = FALSE, replace = FALSE) {

  # data_dir = "~/Dropbox/ra-work/spped/Data/"
  # cat(sprintf("DEV LINE: data_dir = %s", data_dir))
  if(force) {

    message("This will take a while to compute. Are you sure you want to regenerate `cps_address`?\n1: yes\n2: stop")
    answer <- readline(">>> ")
    if(answer != 1) stop('Import cancelled.')

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

    cps_address <- dplyr::left_join(addr, geo_full, by = 'address_full') %>%
      dplyr::select(schoolidr_c_d_t_s, schoolname, year, address, lat, lon, charter, ever_address_change, address_full) %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::rename(school_year = year)

    if(replace) devtools::use_data(cps_address, overwrite = TRUE)

    return(cps_address)

  } else {
    # check for data
    rda <- 'data/cps_address.rda'

    # if exists, load, otherwise, import using force
    if(file.exists('data/cps_address.rda')) {
      load('data/cps_address.rda')
      return(cps_address)
    } else {
      get_cps_address(data_dir, force = TRUE)
    }
  }

}

# #' get chicago public school data
# #' @inheritParams get_cps_address
# #' @export
# get_cps_addresses <- get_cps_address


#' get chicago public school data
#' @inheritParams get_cps_address
#' @export

get_cps_2008_2016 <- function(data_dir = file.path(get_main_dir(), "Data"), force = FALSE, replace = FALSE) {

  if(force) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE)

    # tag non address data
    indx <- !grepl('address', basename(files))

    # get school address data with lat lon
    addr <- get_cps_address(data_dir) %>%
      dplyr::select(schoolidr_c_d_t_s, year, address, lat, lon, address_full)

    cps_2008_2016 <- haven::read_dta(files[indx]) %>%
      dplyr::mutate_if(is.character, trimws) %>%
      dplyr::left_join(., addr, by = c('schoolidr_c_d_t_s', 'year')) %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::rename(school_year = year)

    if(replace) devtools::use_data(cps_2008_2016, overwrite = TRUE, compress = 'bzip2')

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

#' get chicago public school data. REQUIRES DATA CLEANED BY SCRIPT `bin/clean-cps-personnel.sh`.
#' @param data_dir path to data directory
#' @param force force a full import.
#' @param replace replace existing package data with new import.
#' @export

get_cps_security_personnel <- function(data_dir = file.path(get_main_dir(), "RawData/CPS_Personnel/cooked"), force = FALSE, replace = FALSE) {

  if(force) {

    stopifnot(dir.exists(data_dir))
    data_dir <- normalizePath(data_dir)
    files <- list.files(data_dir, full.names = TRUE)

    # txt and csv
    txt_files <- files[grepl('\\.txt', basename(files))] %>%
      .[!grepl('2014', .)]
    txt_2014 <- files[grepl('\\.txt', basename(files))] %>%
      .[grepl('2014', .)]
    csv_files <- files[grepl('\\.csv', basename(files))]

    # get data years
    txt_yrs <- stringr::str_extract(txt_files, '20[01][123890]')
    csv_yrs <- stringr::str_extract(csv_files, '20[0-9]{2}')

    # parse txt
    txt_cols <- c('position_number', 'budget_category', 'unit_number', 'other')
    txt <- txt_files %>%
      purrr::map(.x = ., .f = readr::read_delim, delim = "|", col_names = txt_cols, col_type = paste(rep('c', length(txt_cols)), collapse = ""), trim_ws = TRUE) %>% # import everything as character
      purrr::map(.x = ., .f = ~dplyr::mutate_if(.x, is.character, gsub, pattern = '[[:space:]]+', replacement = ' ')) %>% # replace any space pattern with single space
      purrr::map(.x = ., .f = ~dplyr::mutate_if(.x, is.character, tolower))

    # 2014 text
    cols14 <- c("position_number","unit_number", "unit_name", "other")
    txt14 <- txt_2014 %>%
      readr::read_delim(delim = "|", trim_ws = TRUE, col_names = cols14, col_type = paste(rep('c', length(cols14)), collapse = "")) %>%
      dplyr::mutate_if(is.character, gsub, pattern = '[[:space:]]+', replacement = ' ') %>% # replace any space pattern with single space
      dplyr::mutate_if(is.character, tolower)

    # parse csv
    csv_cols2017 <- c("position_number","unit_number","unit_name","fte", "clsindc", "annual_salary","fte_annual_salary","annual_benefit_cost","job_code","job_description","employee_name")
    csv_cols <- csv_cols2017[!csv_cols2017 %in% "clsindc"]

    # 2015 and 2016
    csv1 <- csv_files[csv_yrs %in% as.character(2015:2016)] %>%
      purrr::map(.x = ., .f = readr::read_csv, col_names = csv_cols, col_type = paste(rep('c', length(csv_cols)), collapse = ""), trim_ws = TRUE) %>%
      purrr::map(.x = ., .f = ~dplyr::mutate_if(.x, is.character, gsub, pattern = '[[:space:]]+', replacement = ' ')) %>%
      purrr::map(.x = ., .f = ~dplyr::mutate_if(.x, is.character, tolower))

    # 2017 year
    csv2 <- csv_files[!csv_yrs %in% as.character(2015:2016)] %>%
      purrr::map(.x = ., .f = readr::read_csv, col_names = csv_cols2017, col_type = paste(rep('c', length(csv_cols2017)), collapse = ""), trim_ws = TRUE) %>% # use csv_cols2017
      purrr::map(.x = ., .f = ~dplyr::mutate_if(.x, is.character, gsub, pattern = '[[:space:]]+', replacement = ' ')) %>%
      purrr::map(.x = ., .f = ~dplyr::mutate_if(.x, is.character, tolower)) %>%
      purrr::map(.x = ., .f = ~dplyr::select(.x, -clsindc)) # drop clsindc

    csv <- append(csv1, csv2)

    # add year variable to datasets so we can stack
    txt <- purrr::map2(.x = txt, .y = txt_yrs, .f = ~dplyr::mutate(.x, year = .y))
    csv <- purrr::map2(.x = csv, .y = csv_yrs, .f = ~dplyr::mutate(.x, year = .y))
    txt14 <- txt14 %>% dplyr::mutate(year = "2014") # year must be character to stack

    # stack
    txt <- dplyr::bind_rows(txt)
    csv <- dplyr::bind_rows(csv)

    # table of security positions
    table(csv[['job_description']]) %>% sort(TRUE)

    ## csv security extractions
    txt[['other']] %>%
      stringr::str_extract_all('(school)? security ([[:alnum:]]+)', simplify = FALSE) %>%
      unlist() %>%
      trimws() %>%
      table() %>%
      sort(TRUE)

    # hand compile set of security positions
    csv_sec <- c("cntrl office security officer", "flex team security officer", "school security officer", "senior school security officer", "security supervisor ii")

    txt_sec <- c("school security aide", "school security officer", "security off", "security supervisor")

    # filter and refine
    csv <- csv %>%
      dplyr::filter(job_description %in% csv_sec) %>%
      dplyr::filter(!is.na(job_description)) %>%
      dplyr::select(position_number, unit_number, job_description, year)

    v <- paste0('(', paste(txt_sec, collapse = '|'), ')') # pattern collapse
    txt <- txt %>%
      dplyr::mutate(job_description = stringr::str_extract(other, v)) %>%
      dplyr::filter(!is.na(job_description)) %>%
      dplyr::select(position_number, unit_number, job_description, year)

    v <- paste0('(', paste(csv_sec, collapse = '|'), ')') # pattern collapse
    txt14 <- txt14 %>%
      dplyr::mutate(job_description = stringr::str_extract(other, v)) %>%
      dplyr::filter(!is.na(job_description)) %>%
      dplyr::select(position_number, unit_number, job_description, year) %>%
      dplyr::mutate_all(as.character)

    # stack all and summarize
    cps_security_personnel <- dplyr::bind_rows(txt, txt14, csv) %>%
      dplyr::group_by(unit_number, job_description, year) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::arrange(year, unit_number, job_description) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::rename(school_year = year)

    if(replace) devtools::use_data(cps_security_personnel, overwrite = TRUE, compress = 'bzip2')

    return(cps_security_personnel)

  } else {
    # check for data
    rda <- 'data/cps_security_personnel.rda'

    # if exists, load, otherwise, import using force
    if(file.exists('data/cps_security_personnel.rda')) {
      load('data/cps_security_personnel.rda')
      return(cps_security_personnel)
    } else {
      get_cps_personnel(data_dir, force = TRUE)
    }
  }

}



