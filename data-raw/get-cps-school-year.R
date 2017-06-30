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
  tibble::as_tibble()

# off days
off <- psv[[2]] %>%
  setNames(tolower(names(.))) %>%
  dplyr::mutate(date = ISOdate(year, month, day, 0, 0, 0) %>% as.Date) %>%
  dplyr::select(sy, date, reason, dplyr::everything()) %>%
  tibble::as_tibble()

cps_school_year <- list(school_year = sy, days_off = off)

devtools::use_data(cps_school_year, overwrite = TRUE)