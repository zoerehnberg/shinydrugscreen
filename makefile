.PHONY: builddocker build rerun download process package clean dockeronly archive

builddocker: build installpackage dockeronly 

build: download process package

rerun: clean process package

download:
	cd data && R -e 'rmarkdown::render("download.Rmd")'

process:
	echo "Processing CCLE data..."
	cd analysis && R -e 'rmarkdown::render("process_CCLE.Rmd")'
	echo "Processing GDSC1 data..."
	cd analysis && R -e 'rmarkdown::render("process_GDSC1.Rmd")'
	echo "Processing GDSC2 data..."
	cd analysis && R -e 'rmarkdown::render("process_GDSC2.Rmd")'
	echo "Matching GDSC and CCLE data..."
	cd analysis && R -e 'rmarkdown::render("process_maps.Rmd")'
	echo "Creating fast subsetting methods..."
	cd analysis && R -e 'rmarkdown::render("process_fastSubset.Rmd")'

package:
	cp data/processed/* package/shinyDrugScreen/data/
	R CMD build package/shinyDrugScreen
	rm package/shinyDrugScreen/data/*  # to save space

installpackage:
	R CMD INSTALL shinyDrugScreen*.tar.gz

clean:
	echo "Deleting all processed data and output..."
	rm -f data/processed/*
	rm -f analysis/*.html
	rm -f archive.tar.bz2
	rm -f shinyDrugScreen*.tar.gz
	R -e 'remove.packages("shinyDrugScreen")'

dockeronly:
	# To save space:
	# Remove package file
	rm -f shinyDrugScreen*.tar.gz

archive:
	rm -f archive.tar.bz2
	tar --exclude='.git' -cjf /tmp/archive.tar.bz2 . && mv /tmp/archive.tar.bz2 .
