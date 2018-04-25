all:  build  install check biocCheck

docs:
	R -e "devtools::document()"
vig:
	R -e "devtools::build_vignettes()"

build:
	(cd ..; R CMD build --no-build-vignettes BrowserViz)

install:
	(cd ..; R CMD INSTALL BrowserViz)

check:
	(cd ..; R CMD check `ls -t BrowserViz_* | head -1`)

biocCheck:
	(cd ..; R CMD BiocCheck `ls -t BrowserViz_* | head -1`)
