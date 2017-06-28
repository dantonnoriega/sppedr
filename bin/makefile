RAW_DATA=/Users/danton/Dropbox/ra-work/spped/RawData
CURRENT=$(pwd)

all: cps-personnel cps-crosswalk

cps-personnel:
	sh ./clean-cps-personnel.sh "$(RAW_DATA)/CPS_Personnel/" \;
	cd $(CURRENT)

CROSSWALK=/Users/danton/Dropbox/ra-work/spped/RawData/Crosswalks/Unit_SchoolID_Crosswalk.xlsx
BASE=$(basename $(CROSSWALK))

cps-crosswalk:
	xlsx2csv -s 1 -d '|' $(CROSSWALK) $(BASE).psv