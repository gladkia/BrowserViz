build: webpack assemble buildPackage installPackage

webpack:
	npm install webpack
	npm update
	webpack

assemble:
	(cd ./dist; m4 -P bvdemo.html-template | sed s/\`//g > bvdemo.html)

buildPackage:
	(cd ../../..; R CMD build --no-build-vignettes BrowserViz)

installPackage:
	(cd ../../..; R CMD INSTALL `ls -at BrowserViz_* | head -1`)
