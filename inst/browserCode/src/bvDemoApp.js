"use strict";
//----------------------------------------------------------------------------------------------------
var BrowserVizDemo = (function(hub){

  var hub = hub;

//----------------------------------------------------------------------------------------------------
function addMessageHandlers()
{
   var self = this;  // the context of the current object, IGV

   self.hub.addMessageHandler("ping",               respondToPing.bind(self));
   self.hub.addMessageHandler("displayText",        displayText.bind(self));
   //self.hub.addMessageHandler("returnPayLoadAsIs",  returnPayLoadAsIs.bind(self));

} // addMessageHandlers
//----------------------------------------------------------------------------------------------------
function respondToPing (msg)
{
   var self = this;
   var return_msg = {cmd: msg.callback, status: "success", callback: "", payload: "pong"};
   self.hub.send(return_msg);

} // respondToPing
//------------------------------------------------------------------------------------------------------------------------
function displayText(msg)
{
   var html = " &nbsp; <h2 style='margin:50px;'>" + msg.payload + " </h2>";
   //document.getElementById("bvDemoDiv").innerHTML = msg.payload
   document.getElementById("bvDemoDiv").innerHTML = msg.payload

   send({cmd: msg.callback, status: "success", callback: "", payload: window.document.title});

} // displayText
//----------------------------------------------------------------------------------------------------
function returnPayloadAsIs(msg)
{
   var s = "received json data structure of " + msg.payload.length + " characters";
   var html = " &nbsp; <h2 style='margin:50px;'>" + s + " </h2>";

       // this element is created in inst/browserCode/dist/bvdemo.html-template
   document.getElementById("bvDemoDiv").innerHTML = html;
   send({cmd: msg.callback, status: "success", callback: "", payload: msg.payload});

} // returnPayloadAsIs
//----------------------------------------------------------------------------------------------------
  return({

    signature: "BrowserVizDemo: 0.99.25",
    addMessageHandlers: addMessageHandlers,
    hub: hub,
    });

}); // BrowserVizDemo
//----------------------------------------------------------------------------------------------------
hub = BrowserViz
var demo = BrowserVizDemo(hub);
demo.addMessageHandlers()
hub.start();
window.demo = demo;
window.hub = hub;
