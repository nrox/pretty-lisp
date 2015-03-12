/**
* pretty-LISP
  Javascript interface functions
* Nuno Rox January 2012
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
	req += "<" + fname + ">";
	req += xmlToString(domElement);
	req += "</" + fname + ">";
	req += "</mprequest>";
	sendXMLRequest (stringToXml(req));
	$("mprequest").remove();
} 

function spannedTextContent(textDOM) {
        var children = textDOM.childNodes;
        if (children.length==1) {
                return textDOM.textContent;
        }
        var txt = "";
        for (var i = 0; i < children.length; i++) {
                if (children[i].nodeType!=1) {
                        continue;
                }
                if (children[i].childNodes.length==0){
                        txt += "\n";
                } else {
                        txt += children[i].childNodes[0].nodeValue + "\n";
                }
        }
        return txt.trim();
}

function toggleClick(job){
   try {
        //restore normal classes values
        $("[bkclass]").each( function (index) {
                $(this).attr("class", $(this).attr("bkclass"));
        });
        $("[bkclass]").removeAttr("bkclass");
        var elType = job.attr("type");
        var isList = (elType == "list");
        var isComment = false;
        if (job.attr("class").match("comment")) {
                isComment = true;
        }
        var isString = false;
        if (job.attr("class").match("string")){
                isString = true;
        }
        job.attr("bkclass",job.attr("class"));
        if (isList==false){
           job.attr("class","atomclick");
           var parentid = job.attr("parent");
           var parent = $("[roxid='" + parentid + "']");
           parent.attr("bkclass", parent.attr("class"));
           parent.attr("class","atomparentclick");
        } else {
           job.attr("class","listclick");
           var id = job.attr("roxid");
           var listtype = $("[tgt='" + id + "']");
           if (listtype.get().length>0) {
                listtype.attr("bkclass", listtype.attr("class"));
                listtype.attr("class","listtypeclick");
           }
        }
        var txt = spannedTextContent(job.get(0)).replace("'","").replace("#","");
        if ( txt!="" && isList==false && isComment==false && isString==false ){
                $(":contains(" + txt + ")").each (function (index) {
                    var similar = $(this).get(0).textContent.replace("'","").replace("#","").replace(",","");
                    if (similar==txt){
                        if ( $(this).attr("roxid") != job.attr("roxid") ){
                                $(this).attr("bkclass", $(this).attr("class"));
                                $(this).attr("class", "highlight");
                        }
                    }
                });
        }
   } catch (e){
        alert("?" + e);
   }
}

function roxEvent(roxid, evtType){
        var job = $("[roxid='" + roxid + "']");
        if (job.get().length>0) {
                toggleClick(job);
        }
}



