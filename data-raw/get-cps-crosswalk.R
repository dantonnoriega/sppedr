
# run makefile to convert cps-crosswalk data to raw text
system('PWD=$(pwd) && cd bin && make cps-crosswalk && cd $PWD')

file_path = "/Users/danton/Dropbox/ra-work/spped/RawData/Crosswalks/Unit_SchoolID_Crosswalk.psv"

cps_crosswalk <- readr::read_delim(file_path, delim = "|", col_types = 'cccc') %>%
  setNames(tolower(gsub('[[:space:]]+', '_', names(.))))

devtools::use_data(cps_crosswalk, overwrite = TRUE)