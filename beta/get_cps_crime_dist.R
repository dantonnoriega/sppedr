# functions
get_mi <- function(x, y) {

  d <- geosphere::distCosine(x, y, r = r)
  mi_qtr  <- sum(d <= .25)
  mi_half <- sum(d <= .5)

  return(c(mi_qtr = mi_qtr, mi_half = mi_half))

}

# load data

devtools::load_all("/Users/danton/GitHub/sppedr")
cpd_crime <- get_cpd_crime(import = TRUE)

cpd2008 <- cpd_crime %>%
  dplyr::filter(year == 2008)

# unique lat lon
cps_lon_lat <- cps_address %>%
  dplyr::select(lon, lat) %>%
  dplyr::distinct() %>%
  as.matrix()

cpd_lon_lat <- cpd2008 %>%
  dplyr::select(lon, lat) %>%
  dplyr::filter(!is.na(lat)) %>%
  as.matrix()

r <- 3959

p1s <- apply(cps_lon_lat, 1, list) %>%
  lapply(., '[[', 1)

dist <- parallel::mclapply(p1s, get_mi, y = cpd_lon_lat, mc.cores = 3L) %>%
  do.call(rbind, .)

