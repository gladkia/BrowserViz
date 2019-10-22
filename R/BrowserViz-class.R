#' BrowserViz:  a base class providing simple, extensible message passing between
#' your R session and your web browser, for interactive data visualization.
#'
#' @docType package
#'
#' @name BrowserViz-class
#' @rdname BrowserViz-class
#' @aliases BrowserViz-class
#'
#' @import BiocGenerics
#' @import methods
#' @importFrom utils browseURL
#'
#' @import httpuv
#' @import jsonlite
#'
#' @exportClass BrowserViz
#'
#----------------------------------------------------------------------------------------------------
#' @description
#' Many of the best interactive graphics capabilities available today are written in Javascript and
#' run in a web browser.  BrowserViz makes these capabilities available in R, using websockets
#' for message passing back and forth between R and the browser. This class connects your R session to your web browser via websockets,
#' using the R httupv library, which in turn uses the Rook webserver.
#'
#' BrowserViz is a concrete base class, in that instances can be constructed and run - which we do for testing.
#' The primary use of this BrowserViz is to be subclassed: to facilitate the creation of new
#' browser-based, R-connected interactive graphics capabilities embodied in R packages, written by
#' programmers with some skill in both R and Javascript.  Two examples of this can
#' be found in these Bioconductor packages \url{https://bioconductor.org/packages/devel/bioc/html/igvR.html}
#' and \url{https://bioconductor.org/packages/devel/bioc/html/RCyjs.html}.
#' @seealso \code{\link{BrowserViz}}
#'
#----------------------------------------------------------------------------------------------------

#' An S4 class to create and manage a modest webserver for websocket message passing  between R and Javascript
#' running in your web browser
#'
#' @slot uri The http location at which this modest webserver runs
#' @slot port An integer port number for the http connection
#' @slot websocketConnection An environment managed by the httpuv library on our behalf
#' @slot quiet Logical varaible controlling verbosity during execution
#'
.BrowserViz <- setClass ("BrowserViz",
                         representation = representation (
                                               uri="character",
                                               port="numeric",
                                               websocketConnection="environment",
                                               quiet="logical"),
                         prototype = prototype (uri="http://localhost", 9000)
                         )

#----------------------------------------------------------------------------------------------------
setGeneric('wait',                    signature='obj', function(obj, msecs) standardGeneric('wait'))
setGeneric('port',                    signature='obj', function(obj) standardGeneric('port'))
setGeneric('ready',                   signature='obj', function(obj) standardGeneric('ready'))
setGeneric('getBrowserInfo',          signature='obj', function(obj) standardGeneric('getBrowserInfo'))
setGeneric('send',                    signature='obj', function(obj, msg) standardGeneric('send'))
setGeneric('browserResponseReady',    signature='obj', function(obj) standardGeneric('browserResponseReady'))
setGeneric('getBrowserResponse',      signature='obj', function(obj) standardGeneric('getBrowserResponse'))
setGeneric('closeWebSocket',          signature='obj', function(obj) standardGeneric('closeWebSocket'))
setGeneric('getBrowserWindowTitle',   signature='obj', function(obj) standardGeneric('getBrowserWindowTitle'))
setGeneric('setBrowserWindowTitle',   signature='obj', function(obj, newTitle) standardGeneric('setBrowserWindowTitle'))
setGeneric('roundTripTest',           signature='obj', function (obj, ...) standardGeneric('roundTripTest'))
setGeneric('getBrowserWindowSize',    signature='obj', function(obj) standardGeneric('getBrowserWindowSize'))
setGeneric('displayHTMLInDiv',        signature='obj', function(obj, htmlText, div.id) standardGeneric('displayHTMLInDiv'))
#----------------------------------------------------------------------------------------------------

# some global variables:
BrowserViz.state <- new.env(parent=emptyenv())
BrowserViz.state$onOpenCall <- 0

# this maps from incoming json commands to function calls
dispatchMap <- new.env(parent=emptyenv())

# status is global variable at file scope, invisible outside the package.
# it keeps track of web sockect connection state, and -- crucially --
# holds the result variable.  this solves the latency problem: when we make
# a request to the code running in the browser, the browser later (though
# often very quickly) sends a JSON message back to R.  If we are, for instance,
# asking for the current browser window title (see 'getBrowserWindowTitle' below), that
# result is sent to the  call back we have registered, "handleResponse")
# to make this seem like a synchronous call, the caller sits in a tight sleep loop,
# waiting until status$result is no longer NULL.  getBrowserWindowTitle will then
# parse that JSON response into an R variable.
# the checking of status$result, and its retrieval when ready (no longer null)
# is accomplished by exported methods browserResponseReady and getBrowserResponse,
# to be used by subclasses as well.

status <- new.env(parent=emptyenv())
status$result <- NULL
#----------------------------------------------------------------------------------------------------
setupMessageHandlers <- function()
{
   addRMessageHandler("handleResponse", "handleResponse")

} # setupMessageHandlers
#----------------------------------------------------------------------------------------------------
#' Constructor for BrowserViz
#'
#' @name BrowserViz constructor
#' @rdname BrowserViz
#'
#' @description
#'
#' This constructor function:
#'
#' \itemize{
#'   \item creates the BrowserViz object
#'   \item initializes the httpuv web server
#'   \item prepares that web server to additionally handle websocket traffic
#'   \item loads a "browserFile" - an html/javascript/css web page to communicate with in your web browser
#'   \item opens websocket communication between your R session and your browser
#'   \item installs an optional "httpQueryProcessingFunction" to handle http (non-websocket) requests.
#' }
#'
#'
#' @param portRange The constructor looks for a free websocket port in this range.  15000:15100 by default
#' @param title Used for the web browser window, "igvR" by default
#' @param browserFile The full path to the bundled html, js and libraries, and css which constitute the browser app
#' @param quiet A logical variable controlling verbosity during execution
#' @param httpQueryProcessingFunction a function, default NULL, provides subclasses with the
#'   opportunity to execute code on the http server created here.
#'
#' @return An object of the BrowserViZ class
#'
#' @export
#'
#'

BrowserViz = function(portRange=10000:10100, title="BrowserViz", browserFile, quiet=TRUE,
                      httpQueryProcessingFunction=NULL)
{
  host <- "localhost"

  if(!quiet){
     message(sprintf("BrowserViz constructor starting with html file '%s'", browserFile))
     message(sprintf(" html file exists? %s", file.exists(browserFile)))
     }

  stopifnot(file.exists(browserFile))

  wsCon <- new.env(parent=emptyenv())
  wsCon <- .setupWebSocketHandlers(wsCon, browserFile, quiet)
  result <- .startServerOnFirstAvailableLocalHostPort(portRange, wsCon)
  actualPort <- result$port
  wsCon$server <- result$server

  if(is.null(actualPort))
    stop(sprintf("no available ports in range %d:%d", min(portRange), max(portRange)))

  uri = sprintf("http://%s:%s", host, actualPort)


  if(!quiet){
     message(sprintf("summoning default browser to get %s", uri))
     }

  sleepTime <- 2
  Sys.sleep(sleepTime);
  browseURL(uri, browser=.getBrowser())

  if(!quiet)
     message(sprintf("starting daemonized server on port %s", actualPort))

  setupMessageHandlers()

  obj <- .BrowserViz(uri=uri, websocketConnection=wsCon, port=actualPort, quiet=quiet)

  BrowserViz.state[["httpQueryProcessingFunction"]] <- httpQueryProcessingFunction

  totalWait <- 0.0
  sleepTime <- 100

  while(!wsCon$open){
      wait(obj, sleepTime)
     totalWait <- totalWait + (sleepTime/1000)
     }

  message(sprintf("BrowserViz websocket ready after %6.2f seconds", totalWait));

  obj

} # BrowserViz: constructor
#----------------------------------------------------------------------------------------------------
.startServerOnFirstAvailableLocalHostPort <- function(portRange, wsCon)
{
   done <- FALSE

   port <- portRange[1]
   server <- NULL

   while(!done){
     if(port > max(portRange))
        done <- TRUE
     else{
        message(sprintf("attempting to open websocket connection on port %d", port))
        server <- tryCatch(startServer("127.0.0.1", port, wsCon),
                         error=function(m){sprintf("port not available: %d", port)})
        }
     if("WebServer" %in% class(server))  # will be character if the port is already claimed
        done <- TRUE
     else
        port <- port + 1;
     } # while

   actualPort <- server$getPort()

   list(server=server, port=actualPort)

} # .startServerOnFirstAvailableLocalHostPort
#----------------------------------------------------------------------------------------------------
#' Pause for the specified number of milliseconds
#'
#' @rdname wait
#' @aliases wait
#'
#' @param obj An object of class BrowserViz
#' @param msecs Numeric
#'
#' @export
#'

setMethod('wait', 'BrowserViz',

  function (obj, msecs) {
     service(msecs)  # an httpuv function
     }) # wait

#----------------------------------------------------------------------------------------------------
#' Display the core attributes of the BrowserViz object to stdout
#'
#' @rdname show
#' @aliases show
#'
#' @param object An object of class BrowserViz
#'
#'

setMethod('show', 'BrowserViz',

  function (object) {
     msg <- sprintf("BrowserViz object");
     cat(msg, '\n', sep='')
     msg <- sprintf("ready? %s", ready(object))
     cat(msg, '\n', sep='')
     msg <- sprintf("port: %d", port(object))
     cat(msg, '\n', sep='')
     }) # show

#----------------------------------------------------------------------------------------------------
#' Get the port number
#'
#' @rdname port
#' @aliases port
#'
#' @param obj An object of class BrowserViz
#'
#' @return the port number use in the websocket connection, a numeric value.
#'
#' @export
#'
setMethod('port', 'BrowserViz',

  function (obj) {
     obj@port
     })

#----------------------------------------------------------------------------------------------------
#' Close the websocket connection - between your R session and your web browser.
#'
#' @rdname closeWebSocket
#' @aliases closeWebSocket
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('closeWebSocket', 'BrowserViz',

  function (obj) {
     if(!obj@websocketConnection$open){
        warning("websocket server is not open, cannot close");
        return()
        }
     obj@websocketConnection$open <- FALSE
     obj@websocketConnection$server$stop()
     obj@websocketConnection$ws <- NULL
     obj@websocketConnection$ws <- -1

     invisible(obj)
     })

#----------------------------------------------------------------------------------------------------
#' Is the websocket connection to the browser ready for use?
#'
#' @rdname ready
#' @aliases ready
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('ready', 'BrowserViz',

  function (obj) {

     if(!is.environment(obj@websocketConnection)){
        message(sprintf("--- obj@websocketConnection not an environment"))
        return(FALSE)
        }

     if(!obj@websocketConnection$open){
       message(sprintf("--- obj@websocketConnection not open"))
       return(FALSE)
       }
   TRUE;
   })

#----------------------------------------------------------------------------------------------------
#' browserResponseReady
#'
#' @rdname browserResponseReady
#' @aliases browserResponseReady
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('browserResponseReady', 'BrowserViz',

  function (obj) {
     return(!is.null(status$result))
     })

#----------------------------------------------------------------------------------------------------
#' Retrieve the response sent by the browser
#'
#' @rdname getBrowserResponse
#' @aliases getBrowserResponse
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('getBrowserResponse', 'BrowserViz',

  function (obj) {
    if(!obj@quiet){
       message(sprintf("BrowserViz getBrowserResponse, length %d", length(status$result)))
       }
    x <- status$result
    return(x)
    })

#----------------------------------------------------------------------------------------------------
.setupWebSocketHandlers <- function(wsCon, browserFile, quiet)
{
   if(!quiet){
      message(sprintf("--- entering BrowserViz .setupWebSocketHandlers"));
      message(sprintf("    browserFile: %s (%s)", browserFile, file.exists(browserFile)));
      }

   wsCon$open <- FALSE
   wsCon$ws <- NULL
   wsCon$result <- NULL
     # process http requests
   wsCon$call = function(req) {
      qs <- req$QUERY_STRING
      if(nchar(qs) > 0){
         if(!quiet) print("--- bv$call, about to call dynamically assigned queryProcessor");
         fields <- ls(req)
         #for(field in fields){
            #printf("---- request field: %s", field)
            #print(req[[field]]);
         #   }
         queryProcessorFunction <- BrowserViz.state[["httpQueryProcessingFunction"]]
         if(!is.null(queryProcessorFunction)){
            queryResult <- queryProcessorFunction(qs)
            body <- queryResult$body
            contentType <- queryResult$contentType
            }
         else{
            body <- "no query processor registered"
            contentType <- "text/html"
            }
         return(list(status=200L, headers=list('Content-Type'=contentType), body=body))
         } # the request had a query string
      wsUrl = paste(sep='', '"', "ws://",
                   ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                   '"')
     list(
       status = 200L,
       headers = list('Content-Type' = 'text/html'),
       body = c(file=browserFile))
       }

      # called whenever a websocket connection is opened
   wsCon$onWSOpen = function(ws) {
      BrowserViz.state$onOpenCall <- BrowserViz.state$onOpenCall + 1
      if(!quiet)
         print("BrowserViz..setupWebSocketHandlers, wsCon$onWSOpen");
      wsCon$ws <- ws   # crucial assignment: this provides later calls to e.g.,  wsCon$ws$send
      ws$onMessage(function(binary, rawMessage) {
         if(!quiet) print("BrowserViz..setupWebSocketHandlers, onMessage ");
         message <- as.list(fromJSON(rawMessage))
         status$message <- message
         wsCon$lastMessage <- message
         if(!is(message, "list")){
            message("message: new websocket message is not a list");
            return;
            }
         if (! "cmd" %in% names(message)){
            message("error: new websocket message has no 'cmd' field");
            return;
            }
         cmd <- message$cmd
         if(!quiet) message(sprintf("BrowserViz dispatching on msg$cmd: %s", message$cmd));
         dispatchMessage(ws, message, quiet);
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   wsCon

} # .setupWebSocketHandlers
#--------------------------------------------------------------------------------
#' Supply the name of a function to call, identified by its key
#'
#' @rdname addRMessageHandler
#' @aliases addRMessageHandler
#'
#' @param key A character string
#' @param functionName A character string
#'
#' @export
#'
addRMessageHandler <- function(key, functionName)
{
   dispatchMap[[key]] <- functionName

} # addRMessagHandler
#---------------------------------------------------------------------------------------------------
#' Route the message coming in from the browser to the appropriate R function.
#'
#' @rdname dispatchMessage
#' @aliases dispatchMessage
#'
#' @param ws   a websocket connectin
#' @param msg  the JSON-encoded message from the browser
#' @param quiet logical TRUE or FALSE
#'
#'

dispatchMessage <- function(ws, msg, quiet)
{
   if(!msg$cmd %in% ls(dispatchMap)){
       message(sprintf("dispatchMessage error!  the incoming cmd '%s' is not recognized", msg$cmd))
       return()
       }

   function.name <- dispatchMap[[msg$cmd]]
   success <- TRUE

   if(is.null(function.name)){
       message(sprintf("dispatchMessage error!  cmd ('%s') not recognized", msg$cmd))
       success <- FALSE
       return()
       }

   tryCatch(func <- get(function.name), error=function(m) func <<- NULL)

   if(is.null(func)){
       message(sprintf("dispatchMessage error!  cmd ('%s') recognized but no corresponding function",
              msg$cmd))
       success <- FALSE
       }

   if(success){
      if(!quiet) message(sprintf("BrowserViz.dispatchMessage calling function '%s'", function.name));
      do.call(func, list(ws, msg))
      }

} # dispatchMessage
#---------------------------------------------------------------------------------------------------
#' Send the specified message to the browser
#'
#' @rdname send
#' @aliases send
#'
#' @param obj An object of class BrowserViz
#' @param msg A list with four fields: {cmd: "someCommand", status: "request",
#'                                      callback: "someFunction", payload: "someData"}
#' @export
#'
setMethod('send', 'BrowserViz',

    function(obj, msg) {
      msg.json <- toJSON(msg)
      obj@websocketConnection$ws$send(toJSON(msg))
      status$result <- NULL
      })

#--------------------------------------------------------------------------------
#' Retrieve basic attributes of the attached web browser.
#'
#' @rdname getBrowserInfo
#' @aliases getBrowserInfo
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('getBrowserInfo', 'BrowserViz',

  function (obj) {
     send(obj, list(cmd="getBrowserInfo", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     getBrowserResponse(obj);
     })

#--------------------------------------------------------------------------------
#' Send data to the browser, ensure that it is returned accurately.
#'
#' @rdname roundTripTest
#' @aliases roundTripTest
#'
#' @param obj An object of class BrowserViz
#' @param ... other arguments
#'
#' @export
#'
setMethod('roundTripTest', 'BrowserViz',

  function (obj, ...) {
     payload <- toJSON(...)
     send(obj, list(cmd="roundTripTest", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
#' Supply the name of a function to call, identified by its key
#'
#' @rdname getBrowserWindowTitle
#' @aliases getBrowserWindowTitle
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('getBrowserWindowTitle', 'BrowserViz',

  function (obj) {
     send(obj, list(cmd="getWindowTitle", callback="handleResponse", status="request", payload=""))
     #browser()
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
#' Supply the name of a function to call, identified by its key
#'
#' @rdname setBrowserWindowTitle
#' @aliases setBrowserWindowTitle
#'
#' @param obj An object of class BrowserViz
#' @param newTitle A character string
#'
#' @export
#'
setMethod('setBrowserWindowTitle', 'BrowserViz',

  function (obj, newTitle) {
     payload = list(title=newTitle)
     send(obj, list(cmd="setWindowTitle", callback="handleResponse", status="request",
                    payload=payload))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     invisible(getBrowserResponse(obj))
     })

#----------------------------------------------------------------------------------------------------
#' Supply the name of a function to call, identified by its key
#'
#' @rdname getBrowserWindowSize
#' @aliases getBrowserWindowSize
#'
#' @param obj An object of class BrowserViz
#'
#' @export
#'
setMethod('getBrowserWindowSize', 'BrowserViz',

  function (obj) {
     send(obj, list(cmd="getWindowSize", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     as.list(fromJSON(getBrowserResponse(obj)))
     })

#----------------------------------------------------------------------------------------------------
#' Ask the browser to display html markup in the specified div
#'
#' @rdname displayHTMLInDiv
#' @aliases displayHTMLInDiv
#'
#' @param obj An object of class BrowserViz
#' @param htmlText A character string with HTML markup
#' @param div.id  A character string
#'
#' @export
#'
setMethod('displayHTMLInDiv', 'BrowserViz',

  function (obj, htmlText, div.id) {
     payload = list(htmlText=htmlText, divID=div.id)
     send(obj, list(cmd="displayHTMLInDiv", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     #as.list(fromJSON(getBrowserResponse(obj)))
     })

#----------------------------------------------------------------------------------------------------
#' Is there a web browser available for testing?
#'
#' @description
#' This package's unit tests require a web browser to connect to.  our heuristic, though not bullet
#'  proof, is that one of three conditions must be met
#' Supply the name of a function to call, identified by its key
#'
#' @name webBrowserAvailableForTesting
#' @rdname webBrowserAvailableForTesting
#' @aliases webBrowserAvailableForTesting
#'
#' @return Logical TRUE or FALSE
#'
#' @export
#'
webBrowserAvailableForTesting <- function()
{
  authorsDevelopmentMachine <- grepl("hagfish", Sys.info()["nodename"])
  bioconductorBuildSystem.linux <- with(as.list(Sys.info()), sysname == "Linux")
  interactiveUse <- interactive()
  return(authorsDevelopmentMachine || bioconductorBuildSystem.linux || interactiveUse)

} # webBrowserAvailableForTesting
#----------------------------------------------------------------------------------------------------
#' handleResponse
#'
#' @rdname handleResponse
#' @aliases handleResponse
#'
#' @param ws websocket connectin
#' @param msg the JSON-encoded character string returned by the browser
#'
#' @return NULL
#'
#' @export
#'
handleResponse <- function(ws, msg)
{
   if(msg$status == "success"){
      #printf("-------- handleResponse, msg$payload: ")
      #print(msg$payload)
      status$result <- msg$payload
      #printf("         status$result: ")
      #print(status$result)
      }
   else{
     message(msg$payload)
     status$result <- NULL
     }

   NULL

} # handleResponse
#----------------------------------------------------------------------------------------------------
.getBrowser <- function()
{
  getOption("browser")
}
#----------------------------------------------------------------------------------------------------
#' Transform an R data structure into JSON
#'
#' @description
#'
#' The semantics of toJSON changed between RJSONIO and jsonlite: in the latter, scalars are
#' promoted to arrays of length 1.  rather than change our javascript code, and since such
#' promotion -- while sensible in the context of R -- strikes me as gratuitous, I follow
#' jeroen ooms suggestion, creating this wrapper
#'
#' @rdname toJSON
#'
#' @param ... Extra arguments passed to this function
#' @param auto_unbox Logical
#'
#' @return a character string with the JSON representation of the R object
#'
#' @export
#'
#' @examples
#'
#'  toJSON(data.frame(a=8:10, b=LETTERS[8:10], stringsAsFactors=FALSE))
#'
toJSON <- function(..., auto_unbox = TRUE)
{
  jsonlite::toJSON(..., auto_unbox = auto_unbox)
}
#----------------------------------------------------------------------------------------------------
#' Transform JSON string into a native R object
#'
#' @rdname fromJSON
#'
#' @param ... Extra arguments passed to this function
#'
#' @return a native R data structure
#'
#' @export
#'
#' @examples
#'
#'  fromJSON(toJSON(data.frame(a=8:10, b=LETTERS[8:10], stringsAsFactors=FALSE)))
#'
fromJSON <- function(...)
{
  jsonlite::fromJSON(...)
}
#----------------------------------------------------------------------------------------------------
