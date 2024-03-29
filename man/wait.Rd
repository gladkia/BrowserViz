% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BrowserViz-class.R
\name{wait,BrowserViz-method}
\alias{wait,BrowserViz-method}
\alias{wait}
\title{Pause for the specified number of milliseconds}
\usage{
\S4method{wait}{BrowserViz}(obj, msecs)
}
\arguments{
\item{obj}{An object of class BrowserViz}

\item{msecs}{Numeric}
}
\description{
Pause for the specified number of milliseconds
}
\examples{
library(BrowserViz)
browserVizBrowserFile <- system.file(package="BrowserViz", "browserCode", "dist", "bvDemoApp.html")
if(BrowserViz::webBrowserAvailableForTesting()){
  bvApp <- BrowserViz(browserFile=browserVizBrowserFile, quiet=TRUE)
  wait(bvApp, 100)
  closeWebSocket(bvApp)
  }

}
