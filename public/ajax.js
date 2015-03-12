/**
* Pretty-lisp
* Javascript interface functions
* Nuno Rox August 2011
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
				var JQUERY_1 = ["after", "before", "prepend", "html", "val", "replaceWith"];
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
                                var CUSTOM_1 = {"alert": alert, "goToElement": goToElement};
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
			alertError ("Request to server failed.");
		}
        } 
    }
    return req;
}

var reqObMemo;
var reqDataMemo;

function sendRequestToServer (fname, domElement) {
	//remove old xml request from the dom (!?)
        $("mprequest").remove();
        //builds the xml request with the event element, wich is sent also with the request
	var xmlStr = "<irequest>";
	xmlStr += "<" + fname + ">";
	xmlStr += xmlToString(domElement);
	xmlStr += "</" + fname + ">";
	xmlStr += "</irequest>";
	//send the request
        var req = httpXmlRequest();        
	req.open("POST", REQUEST_XML_URL, false);
        req.setRequestHeader("Content-type","text/xml");
	//the request must be scheduled with timeout: need to update page with waitSignal before.
	reqObMemo = req;
	reqDataMemo = xmlStr;
        waitSignal(true);
        setTimeout("reqObMemo.send(reqDataMemo); waitSignal(false)", 0);
} 
