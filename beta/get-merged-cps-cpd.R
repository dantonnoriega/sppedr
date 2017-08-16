# functions
get_miles <- function(x, y, yr) {

  y <- y %>%
    dplyr::filter(school_year == yr)

  v <- y %>%
    dplyr::select(lon, lat) %>% # order matters
    as.matrix()

  r <- 3959 # earth mean radius in miles
  d <- geosphere::distCosine(x, v, r = r)

  z <- dplyr::bind_cols(y, dist = d) %>%
    dplyr::mutate(mi_qtr = d <= 0.25, mi_half = d <= 0.5) %>%
    dplyr::group_by(crime_type, school_year, during_school_year, day_off_school_year, weekday, day_hours) %>%
    dplyr::summarize(mi_qtr = sum(mi_qtr), mi_half = sum(mi_half))

  return(z)

}

# get distance
get_crime_counts <- function(x, y, yr) {

  message(sprintf('computing year %s...', yr))

  CORES <- parallel::detectCores()

  u <- x %>%
    dplyr::filter(school_year == yr) %>%
    dplyr::select(lon, lat) %>%
    as.matrix() %>%
    apply(., 1, list) %>%
    lapply(., '[[', 1)

  z <- x %>%
    dplyr::filter(school_year == yr) %>%
    dplyr::select(-lat, -lon, -school_year) # drop lat lon, attach later

  counts <- parallel::mclapply(u, get_miles, y = y, yr = yr, mc.cores = CORES) %>%
    tibble::tibble(df = .)

    # bind values
  dist_df <- do.call(rbind, u) %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(., counts) %>%
    dplyr::bind_cols(z, .) %>%
    tidyr::unnest(df) %>%
    dplyr::mutate(school_year = as.integer(school_year))

  return(dist_df)

}

# load data

devtools::load_all("/Users/danton/GitHub/sppedr")
cpd_crime <- get_cpd_crime(force = TRUE, replace = TRUE)

# missing rate of lat/lon
pct <- sum(is.na(cpd_crime$lat))/nrow(cpd_crime) * 100
s <- sprintf("lat/lon missing rate = %g%%", pct)
message(s)

# remove rows without lat/long
cpd <- cpd_crime %>%
  dplyr::filter(!is.na(lat))

# assume 2016 is calendar year. school year goes into 2017 calendar, so we add these data
cps2017 <- cps_address %>%
  dplyr::filter(school_year == 2016) %>% # carry forward 2016 data to 2017
  dplyr::mutate(school_year = 2017)

cps <- dplyr::bind_rows(cps_address, cps2017) %>%
  dplyr::arrange(schoolidr_c_d_t_s, school_year)

# estimate crimes within distance
dats <- lapply(2008:2017, get_crime_counts, y = cpd, x = cps)
names(dats) <- 2008:2017

# add on security data
cps_security_personnel %>%
  dplyr::left_join(., cps_crosswalk, by = 'unit_number') %>%
  dplyr::mutate(job_description = gsub('school ', '', job_description)) %>%
  dplyr::mutate(job_description = gsub('[[:space:]]', '_', job_description)) %>%
  dplyr::filter(!is.na(schoolidr_c_d_t_s)) %>%
  tidyr::spread(job_description, n) %>%
  dplyr::mutate_if(is.integer, dplyr::funs(replace(., is.na(.), 0))) %>% # replace NAs with 0
  dplyr::select(-unit_number, -abbreviated_name) %>%
  dplyr::mutate(school_year = as.integer(school_year)) %>%
  I() -> personnel


full <- lapply(dats, function(x) dplyr::left_join(x, personnel, by = c('schoolidr_c_d_t_s', 'school_year')))

# export
outputs <- paste0("merged_cpd_cps_", 2008:2017, ".csv")
for(i in seq_along(full)) data.table::fwrite(full[[i]], file.path(get_main_dir(), "Data", outputs[i]), logicalAsInt = TRUE)




