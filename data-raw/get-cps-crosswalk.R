# run makefile to convert cps-crosswalk data to raw text
main_dir <- get_main_dir()

# run if not windows (os x/linux only)
if(.Platform$OS.type!="windows") {
  s <- sprintf('PWD=$(pwd) && cd bin && make cps-crosswalk SPPED_MAIN_DIR=%s && cd $PWD', main_dir)
  system(s)
}

file_path = file.path(main_dir, "RawData/Crosswalks/Unit_SchoolID_Crosswalk.psv")

cps_crosswalk <- readr::read_delim(file_path, delim = "|", col_types = 'cccc') %>%
  setNames(tolower(gsub('[[:space:]]+', '_', names(.))))

devtools::use_data(cps_crosswalk, overwrite = TRUE)