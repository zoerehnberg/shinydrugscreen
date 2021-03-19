.PHONY: build download process package clean dockerapp archive

build: download process

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
	rm package/shinyDrugScreen/data/*

clean:
	echo "Deleting all processed data and output..."
	rm -f data/processed
	rm -f analysis/*.html
	rm -f archive.tar.bz2
	rm -f shinyDrugScreen*.tar.gz
	R -e 'remove.packages("shinyDrugScreen")'

dockerapp: package
	R CMD INSTALL shinyDrugScreen*.tar.gz
	rm -rf analysis data package .git* shinyDrugScreen*.tar.gz makefile

archive:
	rm -f archive.tar.bz2
	tar --exclude='.git' -cjf /tmp/archive.tar.bz2 . && mv /tmp/archive.tar.bz2 .
