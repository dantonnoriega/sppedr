#' get chicago public school data
#' @import data.table
#' @importFrom magrittr %>%
#' @export

get_cps_data <- function(source_dir = "~/Dropbox/ra-work/spped/Data/") {

  source_dir = "~/Dropbox/ra-work/spped/Data/"
  cat(sprintf("DEV LINE: source_dir = %s", source_dir))
  stopifnot(dir.exists(source_dir))
  source_dir <- normalizePath(source_dir)
  files <- list.files(source_dir, full.names = TRUE)
  dta <- lapply(files, haven::read_dta) %>%
    purrr::map(~dplyr::mutate_if(.x, is.character, trimws))

}
