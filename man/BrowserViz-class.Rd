\name{BrowserVizClass}
\docType{class}

% the class
\alias{BrowserViz}
\alias{class:BrowserVizClass}
\alias{BrowserVizClass-class}
\alias{show,BrowserVizClass-method}


% class methods
\alias{setBrowserWindowTitle}
\alias{setBrowserWindowTitle,BrowserVizClass-method}
\alias{\S4method{setBrowserWindowTitle}{BrowserVizClass}}

\alias{getBrowserWindowSize}
\alias{getBrowserWindowSize,BrowserVizClass-method}
\alias{\S4method{getBrowserWindowSize}{BrowserVizClass}}

\alias{getBrowserWindowTitle}
\alias{getBrowserWindowTitle,BrowserVizClass-method}
\alias{\S4method{getBrowserWindowTitle}{BrowserVizClass}}

\alias{browserResponseReady}
\alias{browserResponseReady,BrowserVizClass-method}
\alias{\S4method{browserResponseReady}{BrowserVizClass}}

\alias{getBrowserResponse}
\alias{getBrowserResponse,BrowserVizClass-method}
\alias{\S4method{getBrowserResponse}{BrowserVizClass}}


\alias{port}
\alias{port,BrowserVizClass-method}
\alias{\S4method{port}{BrowserVizClass}}

\alias{closeWebSocket}
\alias{closeWebSocket,BrowserVizClass-method}
\alias{\S4method{closeWebSocket}{BrowserVizClass}}

\alias{getBrowserInfo}
\alias{getBrowserInfo,BrowserVizClass-method}
\alias{\S4method{getBrowserInfo}{BrowserVizClass}}

\alias{send}
\alias{send,BrowserVizClass-method}
\alias{\S4method{send}{BrowserVizClass}}

\alias{ready}
\alias{ready,BrowserVizClass-method}
\alias{\S4method{ready}{BrowserVizClass}}

\alias{toJSON}
\alias{addRMessageHandler}

\title{BrowserViz: a base class (for extension) and standalone example
  of R/Javascript interactive web browser visualization}

\description{
   A concrete base class for interactive R/javascript visualization
   tools.  Derived classes obtain socket setup, status and retrieval
   methods for free, obscuring many complicated details.
}

\usage{
% functions first, starting with the class constructor
BrowserViz(portRange, host="localhost", title="BrowserViz", quiet=TRUE,
browserFile=NA, httpQueryProcessingFunction=NULL)
toJSON(..., auto_unbox=TRUE)
addRMessageHandler(key, functionName)

% now class methods
\S4method{ready}{BrowserVizClass}(obj)
\S4method{send}{BrowserVizClass}(obj, msg)
\S4method{browserResponseReady}{BrowserVizClass}(obj)
\S4method{getBrowserResponse}{BrowserVizClass}(obj)
\S4method{getBrowserInfo}{BrowserVizClass}(obj)
\S4method{closeWebSocket}{BrowserVizClass}(obj)
\S4method{port}{BrowserVizClass}(obj)
\S4method{getBrowserWindowTitle}{BrowserVizClass}(obj)
\S4method{setBrowserWindowTitle}{BrowserVizClass}(obj, newTitle, proclaim=FALSE)
\S4method{getBrowserWindowSize}{BrowserVizClass}(obj)
}


\arguments{

  \item{obj}{The \code{BrowserVizClass} object returned by the
     class constructor.}

  \item{portRange}{One or more consecutive integers in the range
    1025-65535. A typical choice is \code{9000:9024}.  The BrowserViz
    class constructor will try these one at a time in succession until
    a free port is found and the  connection to your web browser is
    established.  If no open ports are found in the supplied range,
    an error is reported.}

  \item{host}{Nearly always left to its default value, "localhost" but
  included as a parameter supporting remote computers for future flexibility.}

  \item{title}{The constructor creates a new window (or a new tab,
  depending on how you web browser is configured). This title is
  displayed at the top of the window or tab.}

  \item{quiet}{Trace and tracking messages are written to the R console
  if this variable is set to FALSE.}

  \item{browserFile}{defaults to \code{NA}, which is interpreted as
  an instruction to use \code{viz.html} in the inst directory of this
  package.  Every subclass will have its own (possibly quite complex)
  browserFile, containing HTML, Javascript, CSS, and calls to powerful
  Javascript libraries (i.e., jQuery, d3, cytoscape.js).
  \code{viz.html} provides Javascript endpoints of, for instance, the
  \code{getBrowserWindowTitle} and \code{setBrowserWindowTitle} class methods
  described above.}

  \item{httpQueryProcessingFunction}{defaults to \code{NULL}.  When not
  \code{NULL} this function, supplied by the subclass, is called
  whenever an HTTP (as opposed to a websocket) request arrives.  Thus
  the BrowserViz subclass can transfer data to the web browser using a
  traditional HTTP GET when that is advantageous.}

  \item{msg}{A name list, with four required slots: "cmd", "status",
  "callback", "payload".  See below.}

  \item{newTitle}{A character string.}

  \item{proclaim}{Logical, default FALSE; if TRUE will add newTitle to the web page's
  body, providing vivid evidence of R controlling the browser.}

  \item{key}{A character string, the "cmd" field of the incoming
    four-field JSON command, used to dispatch on, so that the
    proper function is called.}

  \item{functionName}{A character string: the name of a function to
    which incoming web socket json commands can be dispatched.}

  \item{auto_unbox}{Logical, default TRUE; unboxing: do not coerce a
    scalar into a 1-element list, as the new (2015) jsonlite package
    prefers to do.}

  \item{\dots}{Arguments for our local, very slightly reddfined version of
  toJSON.}
}

\section{Methods}{
  In the code snippets below, \code{obj} is an instance of the BrowserVizClass.
  \describe{

    \item{}{
      \code{BrowserViz(portRange,  host="localhost", title="BrowserViz",
      quiet=TRUE, browserFile=NA, httpQueryProcessingFunction=NULL)}:
        Constructs a BrowserViz object.  Among the several actions
        included are:  your default webrowser browses to the uri of a minimal
        http server embedded in BrowserViz; the \code{browserFile} is returned
        to the browser; the websocket connection is initialized on both ends,
        and the lowest numbered port in \code{portRange}.  
        }      

    \item{}{
      \code{ready(obj)}:
      returns TRUE when the R/browser websocket connection is ready for use.
      }      

    \item{}{
      \code{port(obj)}: returns the actual port being used.
      }      

    \item{}{
      \code{getBrowserInfo(obj)}: returns a character string describing
      the browser to which we are connected, using the standard W3C DOM \emph{navigator.userAgent}.
      }      

    \item{}{
      \code{send(obj, msg)}:  sends a properly structured (having four fields: cmd,
      callback, status, payload) JSON message to the browser.
      }      

    \item{}{
      \code{browserResponseReady(obj)}:  returns TRUE when the
      asynchronous response to the last message has been received from
      the browser.
      }
    
    \item{}{
      \code{getBrowserResponse(obj)}:  returns the just-received
      JSON-encoded, four-field response to the latest message sent to
      the brower.

      }
    \item{}{
      \code{closeWebSocket(obj)}:  Close the websocket port now in use, making it
    available for reuse.
      }      

    \item{}{
      \code{getBrowserWindowTitle(obj)}:  Returns the title of the web page (or
      tab).
      }
    
    \item{}{
      \code{setBrowserWindowTitle(obj, newTitle, proclaim=FALSE)}:  Sets the
      title of the web page or tab to which we are currently connected.
      The "proclaim" argument is for demonstration purposes only, illustrating
      to new users that the web page can be interactively manipulated from R.
       }

    \item{}{
      \code{getBrowserWindowSize(object)}: in pixels.
      }
    
    \item{\dots}{Further arguments for \code{toJSON}, typically just the
    variable to be encoded.}

  }
}

\author{Paul Shannon}


\examples{
  library(BrowserViz)
    
  bv <- BrowserViz(4000:4024)

  ## make sure everything is ready to use
  while(!ready(bv)) Sys.sleep(0.1)

  port(bv)  

  ## illustrate a "low level" call.  This detail is usually hidden from
  ## the user, implemented and contained (in the case of this example)
  ## in a getBrowserWindowTitle(bv) method call.  This level of detail
  ## reveals what goes on behind the scenes.

  msg <- list(cmd="getWindowTitle", status="request", callback="handleResponse", payload="")
  send(bv, msg)
  while(!browserResponseReady(bv)) Sys.sleep(0.1)
  getBrowserResponse(bv)

  ## a simpler user-level approach:
  getBrowserWindowTitle(bv)

  ## set and get the windowTitle
  setBrowserWindowTitle(bv, "new title", proclaim=TRUE)
  getBrowserWindowTitle(bv)

  ## BrowserViz provides another information method which, like the others, will apply
  ## and maybe be of some use to derived classes 
  getBrowserWindowSize(bv)

  ## finally, you should close BrowserViz when you are done, returning
  ## the port for use by other applications.
  closeWebSocket(bv)
}


\keyword{classes}
\keyword{methods}

