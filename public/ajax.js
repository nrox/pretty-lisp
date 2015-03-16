/**
pretty-LISP Editor (beta)
 
Copyright (c) 2012, Nuno Rocha.  All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     disclaimer in the documentation and/or other materials
     provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
 OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

/** dependencies 
  var REQUESTS_TAG
  function waitSignal
  function updateTitle
  function goToElement
  function roxEvent;

*/

/** single xml requests are send to this post uri */
var REQUEST_XML_URL = "/ExchangeXML"; 

var ALLOW_DEBUG = true; 
function alertError (msg) {
	if (ALLOW_DEBUG) {
		alert("js ERROR \n\n" + msg);
	}
}

function stringToXml(str) {
	return jQuery.parseXML(str);
}

function xmlToString(xmlOb) {
	var str;
	try {
		if (window.DOMParser) { //mozzila etc
			var serializer = new XMLSerializer();
			str = serializer.serializeToString(xmlOb);
		} else { //ms ie
			str = xmlOb.xml;
		}
	} catch (e) {
		alertError("xmlToString \n" + e);
	}
	return str;
}

/** 
 * Interpretation and execution of xml responses from the server.
 * Responses lead to execution of jquery, native and custom functions
 * xml responses are in the form
 * <response>
 *   <jquery selector='#id01' method='html'>
 *      <div> ... </div>
 *   </jquery>
 *   <dig>
 *      <jquery selector='.cl2' method='empty'>
 *      </jquery>
 *	<dig>
 *	  ...
 *	</dig>
 *   </dig>
 * </response>
 */

function evalXmlResponse (xmlDoc) {
	var xmlChilds;
	if (xmlDoc.nodeType == 9) {
		xmlChilds = xmlDoc.documentElement.childNodes;
	} else if (xmlDoc.nodeType == 1) {
		xmlChilds = xmlDoc.childNodes;
	} else {
		return;
	}
	try {
             for (var i = 0; i < xmlChilds.length; i++){
                 var respNode = xmlChilds[i];
                 if (respNode.nodeType==1) { //Process only element nodes (type 1)
                        var nodeName = respNode.nodeName;
			nodeName = nodeName.toLowerCase();
			var childNodes = respNode.childNodes;
			if (nodeName=="dig"){
				evalXmlResponse (respNode);
			} else if (nodeName=="jquery") {
				var sel = respNode.getAttribute('selector');
				var met = respNode.getAttribute('method');
				var par = "";
				for (var j = 0; j < childNodes.length; j++) {
                                        var childValue = xmlToString(childNodes[j]);
					if (childValue == "") {
						childValue = childNodes[j];
					}
					par += childValue;
                               	}
                                var JQUERY_0 = ["remove", "empty"];
				var JQUERY_1 = ["after", "before", "prepend", "html", "val", "replaceWith", "append"];
                                var JQUERY_2 = ["attr", "css"];
                                if ( JQUERY_0.indexOf(met) > -1 ) {
                                        $(sel)[met]();
				} else if ( JQUERY_1.indexOf(met) > -1 ) {
                                        $(sel)[met](par);
                                } else if ( JQUERY_2.indexOf(met) > -1 ) {
	                                var arg1 = respNode.getAttribute('arg1');
                                        $(sel)[met](arg1, par);
                                } else {
					alertError("jquery method not allowed: " + met);
                                }
			} else if (nodeName=="predefined") {
                                var met = respNode.getAttribute('method');
                                var par = [];
                                for (var j = 0; j < childNodes.length; j++) {
					if ( childNodes[j].nodeType == 1 ) {
                                		var childValue = childNodes[j].childNodes[0].nodeValue;
                                        	par.push(childValue);
					}
                                }
                                var CUSTOM_0 = {"updateTitle": updateTitle};
                                var CUSTOM_1 = {"alert": alert, "goToElement": goToElement, "showServerMessage": showServerMessage};
                                var CUSTOM_2 = {"roxEvent": roxEvent, "window.open": window.open};
                                if ( CUSTOM_0[met] !== undefined ) {
                                	CUSTOM_0[met]();
                                } else if (CUSTOM_1[met] !== undefined) {
                                	if (met == "alert") {
                                		alert(par[0]);
                                        } else {
                                       		CUSTOM_1[met](par[0]);
					}
                                } else if (CUSTOM_2[met] !== undefined) {
				       if (met == "window.open") {
				       		window.open(par[0], par[1]);
				       } else {
                                       		CUSTOM_2[met](par[0], par[1]);
				       }
                                } else {
                                       alertError("function not defined: " + met);
                                }
			} else {
                                alertError("undefined response group: <" + nodeName + ">");
                        }
                 } //end if nodeType == 1
            } //end for loop
	} catch (e) {
		alertError("while evaluating response:\n" + e);
	}
}

function httpXmlRequest(){
    var req;
    if (window.XMLHttpRequest) {
        req = new XMLHttpRequest();
    } else {
        req = new ActiveXObject("Microsoft.XMLHTTP");
    }
    req.onreadystatechange=function() {
        if (req.readyState==4) {
		if ( req.status==200) {
			evalXmlResponse(req.responseXML);
		} else {
			waitSignal(false);
			var showMessage = confirm ("Request to server failed: status " + req.status + "\n\n" + "Display server messages?");
			if (showMessage){
				sendRequestToServer("message", $("<dummy />").get(0), "irequest");
			}
		}
        } 
    }
    return req;

}

/*
var isChrome = navigator.userAgent.indexOf("hrome") > -1;
function browserCheck(alertWarning) {
	if (true) { 
		return true;
	}
	if (!isChrome){
		if (alertWarning==true) {
                	alert("The current version of pretty-LISP works properly \n\nonly with Google Chrome.");
		}
                return false;
        }
	return true;
}
*/

var reqObMemo;
var reqDataMemo;

function sendRequestToServer (fname, domElement, reqTag) {
	//remove old xml request from the dom (!?)
	if (reqTag===undefined) {
		reqTag = REQUESTS_TAG;
	}
        $(reqTag).remove();
        //builds the xml request with the event element, wich is sent also with the request
	var xmlStr = "<" + reqTag + ">";
	xmlStr += "<" + fname + ">";
	xmlStr += xmlToString(domElement);
	xmlStr += "</" + fname + ">";
	xmlStr += "</" + reqTag + ">";
	//send the request
        var req = httpXmlRequest();        
	req.open("POST", REQUEST_XML_URL, false);
        req.setRequestHeader("Content-type", "text/xml");
	//the request must be scheduled with timeout: need to update page with waitSignal before.
	reqObMemo = req;
	reqDataMemo = xmlStr;
        waitSignal(true);
        setTimeout("reqObMemo.send(reqDataMemo); waitSignal(false)", 0);
} 


function escapeMarkup(str) {
  str = str.replace(/&/g, "&amp;");
  str = str.replace(/>/g, "&gt;");
  str = str.replace(/</g, "&lt;");
  str = str.replace(/"/g, "&quot;");
  str = str.replace(/'/g, "&#039;");
  return str;
}

function showServerMessage(message) {
        message = "<p><pre>" + escapeMarkup(message) + "</pre></p>";
        var button = "<p><button style='width: 100%; height: 45px;' onclick='$(\"#bottomstatus\").empty();'>x<br />close</button></p>"
        $("#bottomstatus").html(button + message);
}

