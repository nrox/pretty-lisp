/**
* Pretty-lisp
* Javascript interface functions
* Nuno Rox August 2011
*/

var REQUEST_XML_URL = "/ExchangeXML"; 
var ALLOW_DEBUG = false; 

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
  alert("xmlToString\n" + e);
}
return str;
}

function safeEval(code) {
        var succeded = false;
        try {
                eval(code);
                succeded = true;
        } catch (err) {
		alert(err + "\n\nCode:\n\n" + code );
	}
}

function getValue(id) {
        var elem = document.getElementById(id);
        if (elem==null) return null;
        var name = elem.nodeName.toLowerCase();
        if (name.match("textarea")) {
                return elem.value;
        } else {
                return elem.innerHTML;
        }
}

function setValue(id, value) {
        var elem = document.getElementById(id);
        if (elem==null) return false;
	$("#"+id).empty();
        var name = elem.nodeName.toLowerCase();
        if (name.match("textarea")) {
                elem.value = value;
        } else {
                elem.innerHTML = value;
        }
        return true;
}


function evalXmlResponse (xmlDoc) {
        var x;
	try {
		x = xmlDoc.documentElement.childNodes;
	} catch (e) {
		try {
			x = xmlDoc.childNodes;
		} catch (e1) {
			alert("evalXmlResponse\n:" + e1);
		}
	}
        for (var i=0;i<x.length;i++){
                 if (x[i].nodeType==1) { //Process only element nodes (type 1)
                        var nodeName = x[i].nodeName;
			nodeName = nodeName.toLowerCase();
                        var nodeValue = "";
			if (x[i].childNodes.length > 0) {
				nodeValue = x[i].childNodes[0].nodeValue;
			}
			if (nodeName=="decodeandeval") {
                                nodeValue = decodeURIComponent(nodeValue);
                                safeEval(nodeValue);
                        } else if (nodeName=="remove") {
                                var id = x[i].getAttribute('setid');
                                if (id==null) {
                                        id = x[i].getAttribute('id');
                                }
                                $("#" + id).remove();
			} else if (nodeName=="eval") {
				//alert("really?"+nodeName);
                                safeEval(nodeValue);
			} else if (nodeName=="dig"){ //apply this function recursively for current node
				evalXmlResponse (x[i]);
			} else if (nodeName=="jquery") {
				var sel = x[i].getAttribute('selector');
				var met = x[i].getAttribute('method');
				var par = "";
				if (x[i].childNodes != null) {
					for (var j = 0; j < x[i].childNodes.length; j++) {
                                	        var childValue = xmlToString(x[i].childNodes[j]);
						if (childValue == "") {
							childValue = x[i].childNodes[j];
						}
						par += childValue;
                               		}
				}
				if (par!="") {
					par = par.replace(/\n/g, " ");
					par = par.replace(/\"/g,"\\\"");
					par = "\"" + par + "\"";
				}
				var expr = "$(\"" + sel + "\")." + met + "(" + par + ");";
				safeEval(expr);
                        } else if (nodeName=="setchildren" || nodeName=="replace") {
				var id = x[i].getAttribute('setid');
                                if (id==null) {
                                        id = x[i].getAttribute('id');
                                }
				nodeValue = "";
				for (var j = 0; j < x[i].childNodes.length; j++) {
                                        nodeValue += xmlToString(x[i].childNodes[j]);
                                }
				if (nodeName=="setchildren") {
					setValue(id, nodeValue);
				} else {
					$("#"+id).replaceWith(nodeValue);
				}
                        } else if (nodeName=="set") {
				for (var j = 0; j < x[i].childNodes.length; j++) {
					if (x[i].childNodes[j].nodeType==1) {
						nodeValue = xmlToString(x[i].childNodes[j]);
						break;
					}
				}
				var id = x[i].getAttribute('setid');
				if (id==null) {
					id = x[i].getAttribute('id');
				}
				var what = x[i].getAttribute('what');
				//alert(nodeValue);
				if (what!=null){
					var code = "document.getElementById('" + id + "')." + what + " = '" + nodeValue + "'";
					safeEval(code);
				} else {
					setValue(id, nodeValue);
					//alert(getValue(id));
				}
			} else if (nodeName=="alert") {
				alert(nodeValue);
			} else if (nodeName=="appendchildren") {
				try {
					var tgt = x[i].getAttribute("tgt");
					for (var j = 0; j < x[i].childNodes.length; j++){
						$("#"+tgt).append(xmlToString(x[i].childNodes[j]));
					}
				} catch (e) {
					alert("error in append child");
				}
			} else if (nodeName=="error") {
				alert(xmlToString(x[i]));
			} else {
                                alert(nodeName + "?");
                        }
                 }
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
        if (req.readyState==4 && req.status==200) {
		evalXmlResponse(req.responseXML);
		displayEpilogue();
        }
    }
    return req;
}

function sendXMLRequest(xmld) {
	var xmlStr = xmlToString(xmld);
	var req = httpXmlRequest();
	req.open("POST", REQUEST_XML_URL, true);
	req.setRequestHeader("Content-type","text/xml");
	req.send(xmlStr);
}

function sendMpRequest (fname, pkg, domElement) {
	var req = "<mprequest>";
	req += "<" + fname + " pkg='" + "pretty-lisp" + "'>";
	req += xmlToString(domElement);
	req += "</" + fname + ">";
	req += "</mprequest>";
	sendXMLRequest (stringToXml(req));
	$("mprequest").remove();
} 

$(document).ready(function(){
       displayEpilogue();
});

