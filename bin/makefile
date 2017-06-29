RAW_DATA=$(SPPED_MAIN_DIR)/RawData
CURRENT=$(pwd)
CROSSWALK=/Users/danton/Dropbox/ra-work/spped/RawData/Crosswalks/Unit_SchoolID_Crosswalk.xlsx
CROSSWALK_BASE=$(basename $(CROSSWALK))

SY=/Users/danton/Dropbox/ra-work/spped/RawData/Chicago_Crime/non-attendance-days.xlsx
SY_BASE=$(basename $(SY))

all: cps-personnel cps-crosswalk cps-school-year

cps-personnel:
	sh ./clean-cps-personnel.sh "$(RAW_DATA)/CPS_Personnel/" \;
	cd $(CURRENT)


cps-crosswalk:
	xlsx2csv -s 1 -d '|' $(CROSSWALK) $(CROSSWALK_BASE).psv

cps-school-year:
	xlsx2csv -s 1 -d '|' $(SY) $(SY_BASE)-1.psv
	xlsx2csv -s 2 -d '|' $(SY) $(SY_BASE)-2.psv