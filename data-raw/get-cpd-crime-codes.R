# build up crime data

url <- 'http://gis.chicagopolice.org/clearmap_crime_sums/crime_types.html'

# get each row of table data (tr)
tr <- xml2::read_html(url) %>%
  rvest::html_nodes(xpath = '/html/body/table//tr')

# get the sub elements. these tables are HORRIBLY formatted. have to attack left and right columns separately.
tds <- purrr::map(tr, rvest::html_children) %>%
  purrr::map(rvest::html_text)

hdr <- tds[[2]] # keep heading
tds <- tds[3:length(tds)] # condense

# left is easier. do first.
lrow <- sapply(tds, '[', 1) # left row
lrow <- lapply(lrow, gsub, pattern = '[ ]?\r\n[\t]+', replacement = '|') %>%
  lapply(. %>% gsub('(\\([[:digit:]|[:alpha:]]+\\))', '\\1|', .)) %>%
  lapply(. %>% gsub('[\\|]+', '|', .)) %>% # remove repeat commas
  lapply(. %>% gsub('\\|$', '', .)) %>% # remove EOL commas
  lapply(. %>% gsub('[[:space:]]+', ' ', .)) %>% # ensure all spaces are just space
  lapply(. %>% strsplit('\\|') %>% .[[1]] %>% trimws())

# right row little harder.
rrow <- lapply(tds, '[', 2) # right row
rrow <- lapply(rrow, gsub, pattern = '[ ]?\r\n[\t]+', replacement = '|') %>%
  lapply(. %>% gsub('(\\(see below\\)|Codes)', '\\1|', ., ignore.case = TRUE)) %>% # add separator to lines ending with "(see below)" or "Codes"
  lapply(. %>% gsub('(\\([[:digit:]+[:alpha:]]*\\))[ ]*(\\(Index\\))?', '\\1|', ., ignore.case = TRUE)) %>%
  lapply(. %>% gsub('[\\|]+', '|', .)) %>% # remove repeat commas
  lapply(. %>% gsub('\\|$', '', .)) %>% # remove EOL commas
  lapply(. %>% gsub('[[:space:]]+', ' ', .)) %>% # ensure all spaces are just space
  lapply(. %>% strsplit('\\|') %>% .[[1]] %>% trimws()) %>%
  lapply(. %>% .[!grepl('(see below)|(Reporting Codes)', .)]) # exclude these rows

# get first value of lrow to use as groupings
lgrp <- lapply(lrow, '[', 1)
zip <- mapply(cbind, lgrp, rrow, SIMPLIFY = FALSE) %>% # zip together
  do.call(rbind, .) %>% # stack
  dplyr::as_tibble(.) %>%
  dplyr::mutate(iucr = stringr::str_extract(V2, '^[\\d]+[[:upper:]]?\\b')) %>%
  dplyr::filter(nchar(V2) > 0) %>%
  setNames(c('category', 'description', 'iucr')) %>% # new header
  dplyr::mutate(hit = gsub('.*[ ]\\(([[:alnum:]]+)\\)$', '1', category)) %>% # tag if matched
  dplyr::select(-hit) %>%
  dplyr::mutate_all(toupper)

# split the dataframe into crime index and iucr
crime <- zip %>%
  dplyr::filter(is.na(iucr)) %>%
  dplyr::slice(2:nrow(.)) %>%
  dplyr::select(category, description) %>%
  dplyr::rename(meta = category, category = description) %>% # rename for joining
  dplyr::filter(!meta %in% c("INDEX CRIME")) # remove redundant

cpd_crime_codes <- zip %>%
  dplyr::filter(!is.na(iucr)) %>%
  dplyr::filter(!category %in% "PUBLIC VIOLENCE") %>%
  dplyr::left_join(crime, by = 'category')

# split up crime category and NIBRS code
cpd_crime_codes <- cpd_crime_codes %>%
  dplyr::mutate(fbi_code = gsub('^(.*)[[:space:]]\\(([[:alnum:]]+)\\)', '\\2', category)) %>%
  dplyr::mutate(category = gsub('^(.*)[[:space:]]\\(([[:alnum:]]+)\\)', '\\1', category)) %>%
  setNames(c('crime_category', 'iucr_description', 'iucr', 'crime_type', 'fbi_code'))

iucr <- cpd_crime_codes %>%
  dplyr::select(dplyr::starts_with('iucr'), dplyr::starts_with('crime')) %>%
  dplyr::distinct()

fbi <- cpd_crime_codes %>%
  dplyr::select(fbi_code, crime_type) %>%
  dplyr::distinct()

cpd_crime_codes <- list(iucr = iucr, fbi = fbi)

devtools::use_data(cpd_crime_codes, overwrite = TRUE)


# TODO
# - [ ] categorize into violent, non-violent etc
