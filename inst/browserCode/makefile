build: assemble

webpack:
	npm install webpack
	npm update
	webpack

assemble:
	(cd ./src; m4 -P bvDemoApp.pre > ../dist/bvDemoApp.html)
	ls -l ./dist/bvDemoApp.html
#	(cd ./dist; m4 -P bvdemo.html-template | sed s/\`//g > bvdemo.html)

buildPackage:
	(cd ../../..; R CMD build --no-build-vignettes BrowserViz)

installPackage:
	(cd ../../..; R CMD INSTALL `ls -at BrowserViz_* | head -1`)
