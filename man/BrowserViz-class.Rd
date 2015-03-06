\name{BrowserVizClass}
\docType{class}

% the class
\alias{BrowserViz}
\alias{class:BrowserVizClass}
\alias{BrowserVizClass-class}
\alias{show,BrowserVizClass-method}


% class methods
\alias{setWindowTitle}
\alias{setWindowTitle,BrowserVizClass-method}
\alias{\S4method{setWindowTitle}{BrowserVizClass}}

\alias{getWindowSize}
\alias{getWindowSize,BrowserVizClass-method}
\alias{\S4method{getWindowSize}{BrowserVizClass}}

\alias{getWindowTitle}
\alias{getWindowTitle,BrowserVizClass-method}
\alias{\S4method{getWindowTitle}{BrowserVizClass}}

\alias{browserResponseReady}
\alias{browserResponseReady,BrowserVizClass-method}
\alias{\S4method{browserResponseReady}{BrowserVizClass}}

\alias{getBrowserResponse}
\alias{getBrowserResponse,BrowserVizClass-method}
\alias{\S4method{getBrowserResponse}{BrowserVizClass}}


\alias{port}
\alias{port,BrowserVizClass-method}
\alias{\S4method{port}{BrowserVizClass}}

\alias{close}
\alias{close,BrowserVizClass-method}
\alias{\S4method{close}{BrowserVizClass}}

\alias{getBrowserInfo}
\alias{getBrowserInfo,BrowserVizClass-method}
\alias{\S4method{getBrowserInfo}{BrowserVizClass}}

\alias{send}
\alias{send,BrowserVizClass-method}
\alias{\S4method{send}{BrowserVizClass}}

\alias{ready}
\alias{ready,BrowserVizClass-method}
\alias{\S4method{ready}{BrowserVizClass}}

\title{BrowserViz: a base class (for extension) and standalone example
  of R/Javascript interactive web browser visualization}

\description{
   A concrete base class for interactive R/javascript visualization
   tools.  Derived classes obtain socket setup, status and retrieval
   methods for free, obscuring many complicated details.
}

\usage{
% functions first, starting with the class constructor
BrowserViz(portRange, host="localhost", title="BrowserViz", quiet=TRUE, browserFile=NA)
% now class methods
\S4method{ready}{BrowserVizClass}(obj)
\S4method{send}{BrowserVizClass}(obj, msg)
\S4method{browserResponseReady}{BrowserVizClass}(obj)
\S4method{getBrowserResponse}{BrowserVizClass}(obj)
\S4method{getBrowserInfo}{BrowserVizClass}(obj)
\S4method{close}{BrowserVizClass}(obj)
\S4method{port}{BrowserVizClass}(obj)
\S4method{getWindowTitle}{BrowserVizClass}(obj)
\S4method{setWindowTitle}{BrowserVizClass}(obj, newTitle, proclaim=FALSE)
\S4method{getWindowSize}{BrowserVizClass}(obj)
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
  \code{getWindowTitle} and \code{setWindowTitle} class methods
  described above.}

  \item{msg}{A name list, with four required slots: "cmd", "status",
  "callback", "payload".  See below.}

  \item{newTitle}{A character string.}

  \item{proclaim}{Logical, default FALSE; if TRUE will add newTitle to the web page's
  body, providing vivid evidence of R controlling the browser.}


}

\section{Methods}{
  In the code snippets below, \code{obj} is an instance of the BrowserVizClass.
  \describe{

    \item{}{
      \code{BrowserViz(portRange,  host="localhost", title="BrowserViz", quiet=TRUE, browserFile=NA)}:
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
      \code{close(obj)}:  Close the websocket port now in use, making it
    available for reuse.
      }      

    \item{}{
      \code{getWindowTitle(obj)}:  Returns the title of the web page (or
      tab).
      }
    
    \item{}{
      \code{setWindowTitle(obj, newTitle, proclaim=FALSE)}:  Sets the
      title of the web page or tab to which we are currently connected.
      The "proclaim" argument is for demonstration purposes only, illustrating
      to new users that the web page can be interactively manipulated from R.
       }

    \item{}{
      \code{getWindowSize(object)}: in pixels.
      }
    
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
  ## in a getWindowTitle(bv) method call.  This level of detail
  ## reveals what goes on behind the scenes.

  msg <- list(cmd="getWindowTitle", status="request", callback="handleResponse", payload="")
  send(bv, msg)
  while(!browserResponseReady(bv)) Sys.sleep(0.1)
  getBrowserResponse(bv)

  ## a simpler user-level approach:
  getWindowTitle(bv)

  ## set and get the windowTitle
  setWindowTitle(bv, "new title", proclaim=TRUE)
  getWindowTitle(bv)

  ## BrowserViz provides another information method which, like the others, will apply
  ## and maybe be of some use to derived classes 
  getWindowSize(bv)

  ## finally, you should close BrowserViz when you are done, returning
  ## the port for use by other applications.
  close(bv)
}



\keyword{classes}
\keyword{methods}

