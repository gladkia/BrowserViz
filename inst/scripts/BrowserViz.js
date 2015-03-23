//----------------------------------------------------------------------------------------------------
// These javascript functions and variables are arranged into a simple module so that
// implementation details are kept private from the public API
// common services and utility functions are provided here
//----------------------------------------------------------------------------------------------------
var BrowserViz = (function () {

  var name = "BrowserViz";
  var dispatchOptions = {};
  var socketConnectedFunctions = [];
  var onDocumentReadyFunctions = [];
  var socketURI = window.location.href.replace("http://", "ws://");

  var socket;

//----------------------------------------------------------------------------------------------------
function setupSocket(socket)
{
  try {
     socket.onopen = function() {
        console.log("websocket connection now open");
        for(var f=0; f < socketConnectedFunctions.length; f++){
           console.log("calling the next sockectConnectedFunction");
           socketConnectedFunctions[f]();
           } // for f
        } // socked.onopen

     socket.onmessage = function got_packet(msg) {
        console.log("=== browserViz.js, socket.onmessage");
        var msg = JSON.parse(msg.data)
        console.log(msg);
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
function addOnDocumentReadyFunction(func)
{
   onDocumentReadyFunctions.push(func)

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
     console.log("calling on ready function");
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
function getRegisteredMessageNames()
{
   return(Object.keys(dispatchOptions));
  
} // getRegisteredMessageNames
//----------------------------------------------------------------------------------------------------
function dispatchMessage(msg)
{
   console.log("=== browserViz.js, dispatchMessage: ");
   console.log(msg);
   var cmd = msg.cmd;
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
   //var cmd = JSON.parse(msg).cmd;
   //var browserLocalCommand = Object.keys(dispatchOptions).indexOf(cmd) >= 0;

   //if(browserLocalCommand)
   //   dispatchMessage(JSON.parse(msg));
   //else

   console.log("=== browserViz: send, msg");
   console.log(msg);

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
function init()
{
  addMessageHandler("ready", ready)   
  addMessageHandler("getBrowserInfo", getBrowserInfo)
  addMessageHandler("getWindowTitle", getWindowTitle)   
  addMessageHandler("setWindowTitle", setWindowTitle)   
  addMessageHandler("getWindowSize",  getWindowSize)

  console.log("=== finishing bv.init");   

} // init
//----------------------------------------------------------------------------------------------------
function start()
{
  console.log("=== starting bv.start");   
  $(document).ready(runOnDocumentReadyFunctions);
  initializeWebSocket();
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

  return({
    getName: function() {return(name)},
    addSocketConnectedFunction: addSocketConnectedFunction,
    getSocketConnectedFunctions: getSocketConnectedFunctions,
    addOnDocumentReadyFunction: addOnDocumentReadyFunction,
    getOnDocumentReadyFunctions: getOnDocumentReadyFunctions,
    initializeWebSocket: initializeWebSocket,
    getSocket: getSocket,
    addMessageHandler: addMessageHandler,
    getRegisteredMessageNames: getRegisteredMessageNames,
    dispatchMessage: dispatchMessage,
    intersectionOfArrays: intersectionOfArrays,
    send: send,
    setTitle: setTitle,
    init: init,
    start: start
    });

}); // BrowserViz
//----------------------------------------------------------------------------------------------------
