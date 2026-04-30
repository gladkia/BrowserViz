default:
	@echo targets: quick [browserCode roxy install], vig, build, test, site

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

check-podman:
	podman run --rm -v $$(pwd):/pkg -w /pkg browserviz-test-env /bin/bash -c "Rscript -e 'remotes::install_local(dependencies=FALSE)' && Rscript inst/unitTests/test_BrowserViz.R && Rscript -e \"gDRstyle::checkPackage('BrowserViz', repoDir='.', skip_lint=TRUE, skip_tests=TRUE)\""

build-test-env:
	podman build -t browserviz-test-env -f Containerfile .
