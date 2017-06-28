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
  dplyr::mutate(ucr = stringr::str_extract(V2, '^[\\d]+[[:upper:]]?\\b')) %>%
  dplyr::filter(nchar(V2) > 0) %>%
  setNames(c('category', 'description', 'ucr')) %>% # new header
  dplyr::mutate(prefix = gsub('.*[ ]\\(([[:alnum:]]+)\\)$', '\\1', category)) %>%
  dplyr::mutate(hit = gsub('.*[ ]\\(([[:alnum:]]+)\\)$', '1', category)) %>% # tag if matched
  dplyr::mutate(prefix = dplyr::if_else(hit == '1', prefix, NA_character_)) %>% # clean up
  dplyr::select(-hit) %>%
  dplyr::mutate_all(tolower)

# split the dataframe into crime index and ucr
crime <- zip %>%
  dplyr::filter(is.na(prefix)) %>%
  dplyr::slice(2:nrow(.)) %>%
  dplyr::select(category, description) %>%
  dplyr::rename(meta = category, category = description) %>% # rename for joining
  dplyr::filter(!meta %in% c("index crime", "public violence")) # remove redundant

crime_codes <- zip %>%
  dplyr::filter(!is.na(prefix)) %>%
  dplyr::left_join(crime, by = 'category')

devtools::use_data(crime_codes, overwrite = TRUE)


# TODO
# - [ ] categorize into violent, non-violent etc
