.getBrowser <- function()
{
  getOption("browser")
}
#----------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#----------------------------------------------------------------------------------------------------
BrowserViz.state <- new.env(parent=emptyenv())
BrowserViz.state$onOpenCall <- 0
#----------------------------------------------------------------------------------------------------
# the semanitcs of toJSON changed between RJSONIO and jsonlite: in the latter, scalars are
# promoted to arrays of length 1.  rather than change our javascript code, and since such
# promotion -- while sensible in the context of R -- strikes me as gratuitous, I follow
# jeroen ooms suggestion, creating this wrapper
toJSON <- function(..., auto_unbox = TRUE)
{
  jsonlite::toJSON(..., auto_unbox = auto_unbox)
}
#----------------------------------------------------------------------------------------------------
fromJSON <- function(...)
{
  jsonlite::fromJSON(...)
}
#----------------------------------------------------------------------------------------------------
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
.BrowserViz <- setClass ("BrowserVizClass",
                         representation = representation (
                                               uri="character",
                                               port="numeric",
                                               websocketConnection="environment",
                                               status="environment",
                                               quiet="logical"),
                         prototype = prototype (uri="http://localhost", 9000)
                         )

#----------------------------------------------------------------------------------------------------
setGeneric('wait',                    signature='obj', function(obj, msecs) standardGeneric('wait'))
setGeneric('show',                    signature='obj', function(obj) standardGeneric('show'))
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
setupMessageHandlers <- function()
{
   addRMessageHandler("handleResponse", "handleResponse")

} # setupMessageHandlers
#----------------------------------------------------------------------------------------------------
# constructor: asks your browser to display browserFile, which is presented
# by the minimal http server offered by httpuv, after which websocket messages are
# exchanged.  the default browserFile, viz.html, does not do much, but is copied and
# extended by BrowserViz subclassing applcations
# the optional sixth argument, httpQueryProcessingFunction, provides subclasses with the
# opportunity to execute code on the http server created here.  one case of this is
# the BrowserTable class, which uses http in an ajax-ish way to pass pages of a possibly
# very large data.frame to the browser for incremental display.
#
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
  result <- .startDaemonizedServerOnFirstAvailableLocalHostPort(portRange, wsCon)
  actualPort <- result$port
  wsCon$wsID <- result$wsID

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
.validWebSocketID <- function(candidate)
{
   if(length(grep("not available", candidate)) == 1)
      return (FALSE)

   return (TRUE)

} # .validWebSocketID
#----------------------------------------------------------------------------------------------------
.startDaemonizedServerOnFirstAvailableLocalHostPort <- function(portRange, wsCon)
{
   done <- FALSE

   port <- portRange[1]
   wsID <- NULL

   while(!done){
     if(port > max(portRange))
        done <- TRUE
     else{
        printf("attempting to open websocket connection on port %d", port)
        wsID <- tryCatch(startDaemonizedServer("127.0.0.1", port, wsCon),
                         error=function(m){sprintf("port not available: %d", port)})
        }
     if(.validWebSocketID(wsID))
        done <- TRUE
     else
        port <- port + 1;
     } # while

   actualPort <- NULL

   if(.validWebSocketID(wsID))
      actualPort <- port

   list(wsID=wsID, port=actualPort)

} # .startDaemonizedServerOnFirstAvailableLocalHostPort
#----------------------------------------------------------------------------------------------------
setMethod('wait', 'BrowserVizClass',

  function (obj, msecs) {
     service(msecs)  # an httpuv function
     }) # show

#----------------------------------------------------------------------------------------------------
setMethod('show', 'BrowserVizClass',

  function (obj) {
     msg <- sprintf("BrowserViz object");
     cat(msg, '\n', sep='')
     msg <- sprintf("ready? %s", ready(obj))
     cat(msg, '\n', sep='')
     msg <- sprintf("port: %d", port(obj))
     cat(msg, '\n', sep='')
     }) # show

#----------------------------------------------------------------------------------------------------
setMethod('port', 'BrowserVizClass',

  function (obj) {
     obj@port
     })

#----------------------------------------------------------------------------------------------------
setMethod('closeWebSocket', 'BrowserVizClass',

  function (obj) {
     if(!obj@websocketConnection$open){
        warning("websocket server is not open, cannot close");
        return()
        }
     obj@websocketConnection$open <- FALSE
     stopDaemonizedServer(obj@websocketConnection$wsID)
     obj@websocketConnection$ws <- NULL
     obj@websocketConnection$ws <- -1

     invisible(obj)
     })

#----------------------------------------------------------------------------------------------------
# test initial variable setup, then send an actual message, and await the reply
setMethod('ready', 'BrowserVizClass',

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
setMethod('browserResponseReady', 'BrowserVizClass',

  function (obj) {
     #printf("--- browserResponseReady, status$result:")
     #print(status$result)
     return(!is.null(status$result))
     })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserResponse', 'BrowserVizClass',

  function (obj) {
    if(!obj@quiet){
       message(sprintf("BrowserViz getBrowserResponse, length %d", length(status$result)))
       }
    x <- status$result
    #status$result <- "abc.0"
    #printf("status$result: %s", status$result)
    #status$result <- "abc.1"
    #printf("status$result: %s", status$result)
    #status$result <- "abc.2"
    #printf("status$result: %s", status$result)
    #status$result <- "abc.3"
    #printf("status$result: %s", status$result)
    #status$result <- "abc.4"
    #printf("status$result: %s", status$result)
    #status$result <- "abc.5"
    #printf("status$result: %s", status$result)
    #Sys.sleep(1)
    #printf("status$result: %s", status$result)
    return(x)
    })

#----------------------------------------------------------------------------------------------------
.setupWebSocketHandlers <- function(wsCon, browserFile, quiet)
{
   if(!quiet){
      printf("--- entering BrowserViz .setupWebSocketHandlers");
      printf("    browserFile: %s", browserFile);
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
         for(field in fields){
            #printf("---- request field: %s", field)
            #print(req[[field]]);
            }
         queryProcessorFunction <- BrowserViz.state[["httpQueryProcessingFunction"]]
         if(!is.null(queryProcessorFunction))
           body <- queryProcessorFunction(qs)
         else
           body <- "no query processor registered"
         return(list(status=200L, headers = list('Content-Type' = 'text/html'),
                     body=body))
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
      #if(BrowserViz.state$onOpenCall == 1) return()
      #printf("onWSOpen, connection: %s (%d)", ws$request$HTTP_CONNECTION, BrowserViz.state$onOpenCall)
      if(!quiet)
         print("BrowserViz..setupWebSocketHandlers, wsCon$onWSOpen");
      wsCon$ws <- ws   # this provides later access (eg wsCon$ws$send) to crucial functions
      ws$onMessage(function(binary, rawMessage) {
         if(!quiet) print("BrowserViz..setupWebSocketHandlers, onMessage ");
         message <- as.list(fromJSON(rawMessage))
         #printf("--- ws$onMessage")
         #print(message)
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
         if(!quiet) printf("BrowserViz dispatching on msg$cmd: %s", message$cmd);
         dispatchMessage(ws, message, quiet);
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   wsCon

} # .setupWebSocketHandlers
#--------------------------------------------------------------------------------
addRMessageHandler <- function(key, functionName)
{
   dispatchMap[[key]] <- functionName

} # addRMessageHandler
#---------------------------------------------------------------------------------------------------
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
      if(!quiet) printf("BrowserViz.dispatchMessage calling function '%s'", function.name);
      do.call(func, list(ws, msg))
      }

} # dispatchMessage
#---------------------------------------------------------------------------------------------------
setMethod('send', 'BrowserVizClass',

    function(obj, msg) {
      #printf("--- send 1")
      status$result <- NULL
      Sys.sleep(1)
      #printf("--- send 2")

      #printf("bv.send, nchar(str(msg)): %d", nchar(str(msg)));
      #printf("bv.send, nchar(msg$payload): %d", nchar(msg$payload))
      #printf("--- send 3")
      msg.json <- toJSON(msg)
      #printf("--- send 4")
      #printf("bv.send, nchar(msg.json): %d", nchar(str(msg.json)))
      #printf("--- send 5")
      #browser()
      obj@websocketConnection$ws$send(toJSON(msg))
      #printf("--- send 6")
      #printf("obj@websocketConnection$ws$send(toJSON(msg)) complete");
      })

#--------------------------------------------------------------------------------
setMethod('getBrowserInfo', 'BrowserVizClass',

  function (obj) {
     #printf("--- entering getBrowserInfo, status$result: ")
     #print(status$result)
     #status$result <- NULL
     #browser()
     send(obj, list(cmd="getBrowserInfo", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     getBrowserResponse(obj);
     })

#--------------------------------------------------------------------------------
setMethod('roundTripTest', 'BrowserVizClass',

  function (obj, ...) {
     payload <- toJSON(...)
     send(obj, list(cmd="roundTripTest", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('getBrowserWindowTitle', 'BrowserVizClass',

  function (obj) {
     send(obj, list(cmd="getWindowTitle", callback="handleResponse", status="request", payload=""))
     #browser()
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('setBrowserWindowTitle', 'BrowserVizClass',

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
setMethod('getBrowserWindowSize', 'BrowserVizClass',

  function (obj) {
     send(obj, list(cmd="getWindowSize", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     as.list(fromJSON(getBrowserResponse(obj)))
     })

#----------------------------------------------------------------------------------------------------
setMethod('displayHTMLInDiv', 'BrowserVizClass',

  function (obj, htmlText, div.id) {
     payload = list(htmlText=htmlText, divID=div.id)
     send(obj, list(cmd="displayHTMLInDiv", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        wait(obj, 100)
        }
     #as.list(fromJSON(getBrowserResponse(obj)))
     })

#----------------------------------------------------------------------------------------------------
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
     status$result <- NA
     }

   NULL

} # handleResponse
#----------------------------------------------------------------------------------------------------
#.processQuery <- function(queryString)
#{
#
#  list(status=200L, headers = list('Content-Type' = 'text/html'),
#       body="hello from bv.processQuery, dynamically assigned")
#
#} # .processQuery
##----------------------------------------------------------------------------------------------------
#queryProcessor <- .processQuery


