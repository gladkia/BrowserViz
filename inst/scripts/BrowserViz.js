//----------------------------------------------------------------------------------------------------
// These javascript functions and variables are arranged into a simple module so that
// implementation details are kept separate from the public API.
// common services and utility functions are provided here:
//
// -- socket creation and initialization
//    a websocket is created here which "points back" to the server which loads the web page
//    in which this script is contained.   this presumes, as is the case with the R httpuv
//    server used in the BrowserViz R base class, that the websocket server
//      a) begins life as an http server, serving up an initial web page (containing this script)
//      b) then promotes itself from the http:// protocol to the ws:// protocol, after which
//      c) it listens for incoming websocket JSON messages
//
// -- a registry and lookup up service ("dispatchOptions") which dispatches incoming
//    JSON messages to functions registered to handle them
//
// -- the means to register functions to be called when the web page (the one which includes the script)
//    is completely loaded and ready.
//
// -- the means to register functions to be called when the socket connection is open and fully
//    functioning.   for instance, you don't want to run any javascript functions which make
//    websocket requests on the server until the socket is ready
//
// -- a send function, hiding a few details of the socket.send function
//
// -- some very simple browser window operations
//    getBrowserInfo, getWindowTitle, setWindowTitle, getWindowSize
//
// 
//----------------------------------------------------------------------------------------------------
var BrowserViz = (function () {

  var onDocumentReadyFunctions = [];
  var name = "BrowserViz";
  var dispatchOptions = {};
  var socketConnectedFunctions = [];
  var socketURI = window.location.href.replace("http://", "ws://");

  var socket;

//----------------------------------------------------------------------------------------------------
function setupSocket(socket)
{
  try {
     socket.onopen = function() {
        console.log("=== BrowserViz.js, websocket connection now open.");
        for(var f=0; f < socketConnectedFunctions.length; f++){
           console.log("calling the next sockectConnectedFunction");
           socketConnectedFunctions[f]();
           } // for f
        } // socket.onopen

     socket.onmessage = function got_packet(msg) {
        var msg = JSON.parse(msg.data)
        console.log("=== BrowserViz.js, message received: " + msg.cmd);
        dispatchMessage(msg)
        } // socket.onmessage, got_packet

     socket.onclose = function(){
        console.log("socket closing");
        } // socket.onclose
     } // try
  catch(exception) {
    console.log("Error: " + exception);
    }
 
  return(socket);

} // setupSocket
//----------------------------------------------------------------------------------------------------
function addSocketConnectedFunction(func)
{
   socketConnectedFunctions.push(func)

} // addSocketConnectedFunction
//----------------------------------------------------------------------------------------------------
function getSocketConnectedFunctions()
{
   return(socketConnectedFunctions)

} // getSocketConnectedFunction
//----------------------------------------------------------------------------------------------------
function setupBasicMessageHandlers()
{
  addMessageHandler("ready", ready)   
  addMessageHandler("getBrowserInfo", getBrowserInfo)
  addMessageHandler("getWindowTitle", getWindowTitle)   
  addMessageHandler("setWindowTitle", setWindowTitle)   
  addMessageHandler("getWindowSize",  getWindowSize)

} // setupBasicMessageHandlers
//----------------------------------------------------------------------------------------------------
function addOnDocumentReadyFunction(func)
{
   console.log("== localhost addOnDocumentReadyFunction");
   console.log("   typeof(func): " + typeof(func));
   //console.log(func);

   onDocumentReadyFunctions.push(func)

   console.log("== after push, count: " + onDocumentReadyFunctions.length);
   console.log(func);
   //console.log("func, stored");
   //console.log(onDocumentReadyFunctions[0]);

} // addOnDocumentReadyFunction
//----------------------------------------------------------------------------------------------------
function getOnDocumentReadyFunctions()
{
   return(onDocumentReadyFunctions)

} // getOnDocumentReadyFunctions
//----------------------------------------------------------------------------------------------------
function runOnDocumentReadyFunctions()
{
  var funcs = getOnDocumentReadyFunctions()

  for (var f = 0; f < funcs.length; f++) {
     console.log("local BrowserViz, calling on ready function");
     funcs[f]();
     }

} // runOnDocumentReadyFunctions
//----------------------------------------------------------------------------------------------------
function initializeWebSocket()
{
   console.log("browserViz.js, initializeWebSocket, uri: " +
               socketURI);
   socket = new WebSocket(socketURI);
   socket = setupSocket(socket);

} // initializeWebSocket
//----------------------------------------------------------------------------------------------------
function getSocket()
{
  return(socket);

} // getSocket
//----------------------------------------------------------------------------------------------------
function addMessageHandler(cmd, func)
{
  if(cmd in dispatchOptions){
     dispatchOptions[cmd].push(func)
     }
  else{
     dispatchOptions[cmd] = [func]
     }
  
} // addMessageHandler
//----------------------------------------------------------------------------------------------------
function getRegisteredHandlers()
{
   return(Object.keys(dispatchOptions));
  
} // getRegisteredHandlers
//----------------------------------------------------------------------------------------------------
function dispatchMessage(msg)
{
   var cmd = msg.cmd;
   console.log("=== BrowserViz.js, dispatchMessage: " + cmd);
   var status = msg.status;

   if(Object.keys(dispatchOptions).indexOf(cmd) == -1){
      console.log("unrecognized socket request: " + msg.cmd);
      }
   else{
     var funcs = dispatchOptions[cmd];
      for(var i=0; i < funcs.length; i++){
         console.log("  dispatching for " + msg.cmd);
         funcs[i](msg); // dispatchOptions[msg.cmd](msg)
         } // for i
      }

}  // dispatchMessage
//----------------------------------------------------------------------------------------------------
function send(msg)
{
   console.log("=== BrowserViz send: " + msg.cmd);

   socket.send(JSON.stringify(msg));

}  // send
//----------------------------------------------------------------------------------------------------
function setTitle (newTitle)
{
  window.document.title = newTitle;

}  // setTitle
//----------------------------------------------------------------------------------------------------
function intersectionOfArrays(a, b)
{
   var result = a.filter(function(n) {console.log(n); return (b.indexOf(n) != -1)})
   return(result);

} // intersectionOfArrays
//----------------------------------------------------------------------------------------------------
function start()
{
  console.log("=== starting bv.start");   
  $(document).ready(runOnDocumentReadyFunctions);
  console.log("=== starting bv.start");   

}  // start
//----------------------------------------------------------------------------------------------------
function ready(msg)
{
   console.log("=== browserViz, running ready function");
   return_msg = {cmd: msg.callback, status: "success", callback: "", payload: "ready"};
   console.log("about to send...");
   console.log(return_msg);
   send(return_msg);

} // ready
//----------------------------------------------------------------------------------------------------
function getBrowserInfo(msg)
{
   send({cmd: msg.callback, status: "success", callback: "", payload: navigator.userAgent});

} // getBrowserInfo
//----------------------------------------------------------------------------------------------------
function getWindowTitle(msg)
{
   send({cmd: msg.callback, status: "success", callback: "",payload: window.document.title});

} // getWindowTitle
//----------------------------------------------------------------------------------------------------
function setWindowTitle(msg)
{
   console.log(msg)
   var payload = msg.payload;
   console.log(payload)
   var newTitle = payload.title;
   var proclaim = payload.proclaim;
   window.document.title = newTitle;

   if(proclaim == true){
      console.log("proclaim: " + proclaim +  "   title: " + newTitle);
      var html = " &nbsp; <h2 style='margin:50px;'>" + newTitle + " </h2>";
      document.getElementById("browserVizDiv").innerHTML = html;
      }

   send({cmd: msg.callback, status: "success", callback: "", payload: window.document.title});

} // setWindowTitle
//----------------------------------------------------------------------------------------------------
function getWindowSize(msg)
{
   var width = $(window).width()
   var height = $(window).height()
   return_msg = {cmd: msg.callback, status: "success", 
                 callback: "", payload: JSON.stringify({width:width, height: height})};
   send(return_msg);

} // getWindowSize
//----------------------------------------------------------------------------------------------------

  setupBasicMessageHandlers();
  initializeWebSocket();

  return({
    getName: function() {return(name)},
    addSocketConnectedFunction: addSocketConnectedFunction,
    getSocketConnectedFunctions: getSocketConnectedFunctions,
    addOnDocumentReadyFunction: addOnDocumentReadyFunction,
    getOnDocumentReadyFunctions: getOnDocumentReadyFunctions,
    getSocket: getSocket,
    addMessageHandler: addMessageHandler,
    getRegisteredHandlers: getRegisteredHandlers,
    dispatchMessage: dispatchMessage,
    intersectionOfArrays: intersectionOfArrays,
    send: send,
    setTitle: setTitle,
    start: start
    });

}); // BrowserViz
//----------------------------------------------------------------------------------------------------
