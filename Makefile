.PHONY: all clean mrproper

all:
	csi -s create-picker-input.scm data/Country\ picker\ data\ -\ Data.20170118.1.csv > out.scm
	csi -s visualise-picker-input.scm < out.scm
	csi -s picker-input-to-json.scm < out.scm > out.json
	du -hs out.*

clean:
	rm -f out.scm out.json out.pdf

mrproper: clean
	rm -f *~ data/*~

