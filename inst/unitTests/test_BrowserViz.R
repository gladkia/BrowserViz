library(RUnit)
library(BrowserViz)
#--------------------------------------------------------------------------------
# specify an html/javascript file which gives this particular BrowserViz app its
# personality, assembled by npm and webpackfrom this package's companion
# file "github:paul-shannon/browservizjs" and these two
#
#  inst/browserCode/src/bvdemo.js
#  inst/browserCode/dist/bvdemo.html-template
#
# the tests below exercise the base browserviz messages
#    ready
#    getBrowserInfo
#    getWindowTitle
#    setWindowTitle
#    getWindowSize
#    roundTripTest
#
#  and adds one new one, to show how new capability is added
#
browserVizBrowserFile <- system.file(package="BrowserViz", "browserCode", "dist", "bvDemoApp.html")
PORT_RANGE <- 12111:12120   # usually defaults, but used here for more specific testing
#--------------------------------------------------------------------------------
# two sets of tests, useful in different contexts
#   daily routine build tests: construct just one instance, and therefore just
#         one browser window (tab), upon which multiple tests are run.
#   deeper tests:  construct multiple instances, establishing that sockets
#         can be opened and closed, that when PORT_RANGE is exhausted the
#         app handles that gracefully
#--------------------------------------------------------------------------------
if(interactive()){
   if(!exists("bvApp")){
      bvApp <- BrowserViz(browserFile=browserVizBrowserFile, quiet=TRUE)
      #checkTrue(ready(bvApp))
      }
   } # if interactive
#--------------------------------------------------------------------------------
runTests <- function()
{
   test_basic()
}
#--------------------------------------------------------------------------------
test_basic <- function()
{
  checkGetBrowserInfo();
  checkWindowTitle();
  checkGetWindowSize();
  checkRoundTrips()

} # test_basic
#--------------------------------------------------------------------------------
deeperTests <- function()
{
   checkConstructor()
   checkMultipleOpenCloseOnSamePort();
   checkRunOutOfPorts(); #  temporarily disabled

} # deeperTests
#--------------------------------------------------------------------------------
checkGetBrowserInfo <- function()
{
   print("--- checkGetBrowserInfo")
   if(interactive()){
      userAgent <- getBrowserInfo(bvApp)
      checkEquals(typeof(userAgent), "character")
      checkTrue(nchar(userAgent) > 5);  # 120 on chrome 40.0.2214.115 (27 feb 2015)
      }

} # checkGetBrowserInfo
#--------------------------------------------------------------------------------
checkGetBrowserInfo <- function()
{
   print("--- checkGetBrowserInfo")
   if(interactive()){
      userAgent <- getBrowserInfo(bvApp)
      checkEquals(typeof(userAgent), "character")
      checkTrue(nchar(userAgent) > 5);  # 120 on chrome 40.0.2214.115 (27 feb 2015)
      }

} # checkGetBrowserInfo
#--------------------------------------------------------------------------------
checkWindowTitle <- function()
{
   print("--- checkWindowTitle")
   if(interactive()){
      checkTrue(ready(bvApp))
      setBrowserWindowTitle(bvApp, "new title");
      checkEquals(getBrowserWindowTitle(bvApp), "new title")
      }

} # checkWindowTitle
#--------------------------------------------------------------------------------
checkGetWindowSize <- function()
{
   print("--- checkGetWindowSize")

   if(interactive()){
      checkTrue(ready(bvApp))
      x <- getBrowserWindowSize(bvApp)
      checkEquals(sort(names(x)), c("height", "width"))
      checkTrue(all(as.integer(x) > 0))
      }

} # checkGetWindowSize
#--------------------------------------------------------------------------------
checkRoundTrips <- function(quiet=TRUE)
{
   print("--- check_roundTrips")
   if(interactive()){
      #checkTrue(ready(bvApp))

      setBrowserWindowTitle(bvApp, "bv round trip tests")

      data <- 99
      json.returned <- roundTripTest(bvApp, data)
      data.returned <- fromJSON(json.returned)
      message(sprintf("    %5d bytes exchanged", nchar(json.returned)))
      checkEquals(data, data.returned)
      html <- sprintf("<h3> successful round trip of json-encoded data, length %d</h3>", nchar(json.returned))
      displayHTMLInDiv(bvApp, html, "bvDemoDiv")
      Sys.sleep(1)

      data <- list(lowercase=letters, uppercase=LETTERS)
      json.returned <- roundTripTest(bvApp, data)
      data.returned <- fromJSON(json.returned)
      message(sprintf("    %5d bytes exchanged", nchar(json.returned)))
      checkEquals(data, data.returned)
      html <- sprintf("<h3> successful round trip of json-encoded data, length %d</h3>", nchar(json.returned))
      displayHTMLInDiv(bvApp, html, "bvDemoDiv")
      Sys.sleep(1)

      data <- matrix(1:100, nrow=10)
      json.returned <- roundTripTest(bvApp, data)
      data.returned <- fromJSON(json.returned)
      message(sprintf("    %5d bytes exchanged", nchar(json.returned)))
      checkEquals(data, data.returned)
      html <- sprintf("<h3> successful round trip of json-encoded data, length %d</h3>", nchar(json.returned))
      displayHTMLInDiv(bvApp, html, "bvDemoDiv")
      Sys.sleep(1)

      data <- matrix(1:10000, nrow=10)
      json.returned <- roundTripTest(bvApp, data)
      data.returned <- fromJSON(json.returned)
      message(sprintf("    %5d bytes exchanged", nchar(json.returned)))
      checkEquals(data, data.returned)
      html <- sprintf("<h3> successful round trip of json-encoded data, length %d</h3>", nchar(json.returned))
      displayHTMLInDiv(bvApp, html, "bvDemoDiv")
      Sys.sleep(1)
      } # if interactive

} # checkRoundTrips
#--------------------------------------------------------------------------------
checkConstructor <- function()
{
   print("--- checkConstructor")
   if(interactive()){
      app <- BrowserViz(portRange=PORT_RANGE, browserFile=browserVizBrowserFile, quiet=TRUE)
      checkTrue(ready(app))
      checkTrue(port(app) %in% PORT_RANGE)
      closeWebSocket(app)
         # TODO (31 mar 2019): need better abstraction here
      checkTrue(!app@websocketConnection$server$isRunning())
      #checkTrue(!ready(app))
      }

   TRUE

} # checkConstructor
#--------------------------------------------------------------------------------
checkMultipleOpenCloseOnSamePort <- function()
{
   print("--- checkMultipleOpenCloseOnSamePort")

   if(interactive()){
      max <- 3
      for(i in 1:max){
         app <- BrowserViz(portRange=PORT_RANGE[1], browserFile=browserVizBrowserFile, quiet=TRUE)
         #checkTrue(ready(app))
         #printf("app instance #%d ready on port %d", i, port(app))
         checkEquals(port(app), PORT_RANGE[1])
         closeWebSocket(app)
         #checkTrue(!ready(app))
         #printf("app instance #%d closed", i)
         } # for i
      } # if interactive

   TRUE

} # checkMultipleOpenCloseOnSamePort
#--------------------------------------------------------------------------------
checkRunOutOfPorts <- function()
{
   print("--- checkRunOutOfPorts")

   if(interactive()){
      max <- 3
      portRange <- PORT_RANGE[1]:(PORT_RANGE[1]+1)
      apps <- lapply(rep("BrowserVizClass", max), new)

      boundToFail <- function(max, portRange){
         for(i in 1:max){
            app <- BrowserViz(portRange);
            apps[[i]] <- app
            #checkTrue(ready(app))
            setBrowserWindowTitle(app, sprintf("app %d", i))
            checkTrue(port(app) %in% portRange)
            } # for i
         } # boundToFail

      # should be able to open two connections.  the third should fail
      checkException(boundToFail(max, portRange), silent=TRUE)

      # now close any apps which we managed to open
      for(i in 1:length(apps)){
         if(length(port(apps[[i]])) > 0){
            app <- apps[[i]]
            if(ready(app)) {
               #printf("closing app instance on port %d", port(app))
               checkEquals(getBrowserWindowTitle(app), sprintf("app %d", i))
               closeWebSocket(app)
               checkTrue(!ready(app))
               } # if ready
            } # if port(app) has meaningful value
         } # for i
      } # if interactive

   TRUE

} # checkRunOutOfPorts
#--------------------------------------------------------------------------------
