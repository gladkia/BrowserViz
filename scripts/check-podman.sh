#!/bin/bash
set -e

Rscript -e 'remotes::install_local(dependencies=FALSE)'
Rscript inst/unitTests/test_BrowserViz.R
Rscript -e "gDRstyle::checkPackage('BrowserViz', repoDir='.', skip_lint=TRUE, skip_tests=TRUE)"
