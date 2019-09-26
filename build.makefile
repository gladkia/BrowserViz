quick:  browserCode roxy install

all:  browserCode roxy build  install check

browserCode:
	(cd inst/browserCode; make assemble)

roxy:
	R -e "devtools::document()"

vig:
	R -e "devtools::build_vignettes()"

build:
	(cd ..; R CMD build --no-build-vignettes BrowserViz)

install:
	(cd ..; R CMD INSTALL BrowserViz)

check:
	(cd ..; R CMD check --no-manual --no-build-vignettes --ignore-vignettes `ls -t BrowserViz_* | head -1`)

test:
	for x in inst/unitTests/test_*.R; do echo ============== $$x; R -f $$x; done

site:
	R -e "devtools::build_site()"
