% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BrowserViz-class.R
\name{getBrowserWindowTitle,BrowserViz-method}
\alias{getBrowserWindowTitle,BrowserViz-method}
\alias{getBrowserWindowTitle}
\title{Supply the name of a function to call, identified by its key}
\usage{
\S4method{getBrowserWindowTitle}{BrowserViz}(obj)
}
\arguments{
\item{obj}{An object of class BrowserViz}
}
\description{
Supply the name of a function to call, identified by its key
}
\examples{
library(BrowserViz)
browserVizBrowserFile <- system.file(package="BrowserViz", "browserCode", "dist", "bvDemoApp.html")
if(BrowserViz::webBrowserAvailableForTesting()){
  bvApp <- BrowserViz(browserFile=browserVizBrowserFile, quiet=TRUE)
  getBrowserWindowTitle(bvApp)
  closeWebSocket(bvApp)
  }

}
