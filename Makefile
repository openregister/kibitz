.PHONY: all clean mrproper all-scm all-json all-pdf

### All Pickers

# To add a new picker, add its name here and a rule for "out/<its name>.scm"
# in the "Bespoke Pickers" section.

ALL_PICKERS=location-demo


### Global Rules

ALL_SCM =$(addprefix out/,${ALL_PICKERS:=.scm})
ALL_JSON=$(ALL_SCM:.scm=.json)
ALL_PDF =$(ALL_SCM:.scm=.pdf)

all: $(ALL_SCM) $(ALL_JSON) $(ALL_PDF)
	@echo "\n\n\nDONE!\n=====\n"
	du -h out.* out/*

all-scm:  $(ALL_SCM)

all-json: $(ALL_JSON)

all-pdf:  $(ALL_PDF)

clean:
	rm -f out.scm out.json out.pdf
	rm -f $(ALL_SCM) $(ALL_JSON) $(ALL_PDF)
	rmdir out/

mrproper: clean
	rm -f *~ data/*~


out/:
	mkdir out/

%.json: %.scm
	csi -s picker-input-to-json.scm < $< > $@

%.pdf:  %.scm
	csi -s visualise-picker-input.scm < $< > $@


### Bespoke Pickers

## Location Pickers

LOCATION_REGISTERS=data/country.rsf data/territory.rsf data/uk.rsf
LOCATION_COMMON_CONFIG=data/Location\ picker\ data\ -\ Data.csv

out/location-demo.scm: out/ $(LOCATION_COMMON_CONFIG) $(LOCATION_REGISTERS)
	csi -s create-location-picker-demo.scm data/Location\ picker\ data\ -\ Data.csv > $@

