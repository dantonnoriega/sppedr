main_dir = get_main_dir()

if(.Platform$OS.type!="windows") {
  s <- sprintf('PWD=$(pwd) && cd bin && make cps-school-year SPPED_MAIN_DIR=%s && cd $PWD', main_dir)
  system(s)
}

data_dir = file.path(main_dir, "RawData/Chicago_Crime/")

# non-attendance
files <- list.files(data_dir, full.names = TRUE, pattern = '^non-attendance.*psv$', ignore.case = TRUE)

psv <- purrr::map(files, readr::read_delim, trim_ws = TRUE, delim = "|")

# school year
sy <- psv[[1]] %>%
  t() %>%
  .[-1,] %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  setNames(c("sy", "start", "end")) %>%
  dplyr::mutate_at(.vars = dplyr::vars(start, end), as.Date, format = "%m-%d-%y") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(summer = dplyr::lead(start)) %>%
  dplyr::mutate(school_year = as.integer(lubridate::year(end)))

sy$summer[nrow(sy)] <- fast.as.Date('2017-09-04')

# off days
off <- psv[[2]] %>%
  setNames(tolower(names(.))) %>%
  dplyr::mutate(date = ISOdate(year, month, day, 0, 0, 0) %>% as.Date) %>%
  dplyr::select(sy, date, reason, dplyr::everything()) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(school_year = as.integer(gsub('.*(\\d{2})$', '20\\1', sy))) %>%
  dplyr::mutate(calendar_year = year)

cps_school_year <- list(school_year = sy, days_off = off)

devtools::use_data(cps_school_year, overwrite = TRUE)