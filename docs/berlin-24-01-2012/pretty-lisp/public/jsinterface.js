

/**
* Project MandelPie
* Javascript interface
* Nuno Rox August 2011
*/

var IDE_DIV = "idlispide"; 

function toggleSeparator(sepID) {
        $(".sepbutton").css("color","black");
	$(".sepbutton[onclick$=')']").each( function () { 
		var click = $(this).attr("onclick");
		if (click.indexOf("'" + sepID + "'")>-1) {
			$(this).css("color","#80967a");
		} 
	});
	$(".separator").css("display","none");
	$("#" + sepID).css("display","inline");	
}

function displayEpilogue() {
	$(".elementmenu").unbind ("click", elementmenuClick);
        $(".elementmenu").click (elementmenuClick);
        $("#fileslist").unbind ("click", openLispFile);
        $("#fileslist").click (openLispFile);
        $(".hintbutton").unbind ("click", hintProcess);
        $(".hintbutton").click (hintProcess);
}

function goToTgtElement(){
	var roxid = $("#tgtelement").attr("tgt0");
	goToElement(roxid);
}

function goToElement(roxid){
    try {
        var tgt = $("[roxid='" + roxid + "']");
        var offsetY = tgt.offset().top - 200;
	var offsetX = tgt.offset().left - 700;
        window.scroll(offsetX, offsetY);
    } catch (e) {
	alert("Trying to go to " + roxid + "\n\n" + e);
    }
}

var DBLCLICK_MEMO = (new Date()).getTime();
var DBLCLICK_TIME = 500;

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

/**returns the text content ox a svg text element, considering tspan*/

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

function roxEvent(roxid, evtType){
	if (evtType=="dblclick") {
		var tempo = (new Date()).getTime();
		if ((tempo-DBLCLICK_MEMO)>DBLCLICK_TIME) {
			DBLCLICK_MEMO = (new Date()).getTime();
			//return; //disabling this makes double click single click
		}
	}
	var job = $("[roxid='" + roxid + "']");
	if (job.get().length==0) {
		return;
	}
	if (job.get().length>1) {
		job.each ( function (index) {
			alert( xmlToString( $(this).get(0) ) +  "\n\n parent: " + xmlToString( $(this).parent().get(0) ) );
		});
	}
	if ((evtType=="dblclick")||(evtType=="lastclick")) {
		toggleClick(job);
		processClick(job);
	}
}

function fileOp(operation) {
        hydeMenu(null);
	$("#bottomstatus").empty();
        var newvalue = $("#fileslist").val();
        if (operation=="saveas") {
                var answer = prompt ("Save as:", $("#curfileid").html());
                if (answer!=null) {
                        newvalue = answer;
                } else {
                        return;
                }
        }
        if (operation=="new") {
                var answer = prompt ("New:", $("#fileslist").val());
                if (answer!=null) {
                        newvalue = answer;
                        $("#curfileid").html(answer);
                } else {
                        return;
                }
        }
        if (operation=="browse" || operation=="load") {
                newvalue =  $("#fileslist").val();
        }
        if (operation=="open") {
                $("#curfileid").html( $("#fileslist").val() );
                $("#tbhints").css("display","inline");
        }        
        var valor = $("#curfileid").html();
        updateTitle();
	var xml = jQuery.parseXML("<button operation='" + operation + "' value='" + valor + "' newvalue='" + newvalue + "'>" + operation + "</button>");
        sendMpRequest("fileoperation","rox-utils", xml);
}

var menuKeys = new Array();
menuKeys["0"] = "Cancel";
menuKeys["1"] = "Update";
menuKeys["2"] = "Execute";
//3
menuKeys["x"] = "Cut";
menuKeys["c"] = "Copy";
menuKeys["v"] = "Paste";
menuKeys["f"] = "Delete";
//7
menuKeys["a"] = "Before";
menuKeys["s"] = "Inside";
menuKeys["d"] = "After";
//10
menuKeys["q"] = "Free";
menuKeys["w"] = "Surround";
menuKeys["e"] = "Comment";
//13
menuKeys["t"] = "Transpose";
menuKeys["k"] = "Collapse";
menuKeys["l"] = "CollapseAll";
menuKeys["o"] = "Expand";
menuKeys["p"] = "ExpandAll";
//18
menuKeys["b"] = "Left";
menuKeys["h"] = "Up";
menuKeys["m"] = "Right";
menuKeys["n"] = "Down";

var AUX_KEY = 0;

function menuTextKeyPress(evt) {
	try {
                var menutgt = $(evt.srcElement).get(0).getAttribute("menutgt");
                if (menutgt == null) {
			return;
		}
		AUX_KEY++;
		var answer = null;
		if (evt.keyCode==27) { //esc
			answer = "0";
		}
		if (evt.ctrlKey && (evt.keyCode==13)) { //ctrl + enter
			answer = "1";
		}
		if (answer!=null) {
			var operation = menuKeys[answer].toLowerCase();
			var tgt = $("[roxid='" + menutgt + "']");
			copyPasteOperation(tgt, operation);
			//hydeMenu(null);
			return;
		}
                if(AUX_KEY==0) {
 			var k = String.fromCharCode(evt.keyCode);
                	k = k.toLowerCase();
                	if (evt.ctrlKey &&  k!="z" && k!="y" ) {
			} else {
                        	processClick(evt);
			}
			return;
                }
                if ((AUX_KEY==1) && (evt.keyCode==17) ){ //ctrl or ctrl+alt
                        AUX_KEY = -1;
			return;
                }
                if (evt.keyCode==17 && evt.altKey || evt.keyCode==18 && evt.ctrlKey || evt.ctrlKey && evt.altKey ){ //ctrl or ctrl+alt
                        AUX_KEY = -1;
                        return;
                }
	} catch (e){
		alert(e);
	}
}

function takeLastToHint(evt) {
	var txt = $("#menutext").val().split(" ");
	txt = txt[txt.length-1];
	txt = txt.replace("(","").trim();
	$("#hinttxt").val(txt);
}

function processClick (evt) {
	if (evt instanceof jQuery ) {
	        showClickMenu2(evt);
	} else {
        	var job = $(evt.srcElement);
        	var ob = job.get(0);
		var answer = null;
		var operation = null;
                var k = String.fromCharCode(evt.keyCode);
                k = k.toLowerCase();
		var nav = {37: "Left", 38: "Up", 39: "Right", 40: "Down"};
                if (k in menuKeys) {
                        answer = k;
		} else if ( k=="z" || k=="y" ) {
			var ur = "undo";
			if (k=="y") {
				ur="redo";
			}
			var xml = "<dummy operation='" + ur + "' value='" + $("#curfileid").html() + "' newvalue='" + $("#curfileid").html() + "'>" + ur + "</dummy>";
        		sendMpRequest("fileoperation","rox-utils", stringToXml(xml));
			hydeMenu(null);
			return;
		} else if (!(nav[evt.keyCode]==null)) {
			operation=nav[evt.keyCode];
		} else if (evt.keyCode!=0) {
			answer = null;
                } else {
			answer = ob.textContent;
			answer = jQuery.trim(answer);
			answer = answer.split(" ")[0];
		}
		if (answer!=null || operation!=null){
			operation = operation || menuKeys[answer].toLowerCase();		
			var menutgt = ob.getAttribute("menutgt");
			var tgt = $("[roxid='" + menutgt + "']");
                	copyPasteOperation(tgt, operation);
			var colexp = operation.substr(0,3);
			if (colexp=="exp" || colexp=="col"){
				hydeMenu(null);
			}
		}
	}
}

var MENUTEXT_CSS_DIMENSIONS = "";

function hydeMenu(evt) {
	if ( $("#menutext").css("width")!=undefined && $("#menutext").css("height")!=undefined) {
		MENUTEXT_CSS_DIMENSIONS = "width: " + $("#menutext").css("width") + ";" + "height: " + $("#menutext").css("height") + ";";
	}
        $("#tdmenu").empty();
        $("#menuarea").empty();
        $("#tgtelement").remove();
}

function showClickMenu2 (job) {
	AUX_KEY = 0;
        var question = "";
        for (menu in menuKeys) {
                question += "\n" + menu + " - " + menuKeys[menu];
        }
        var menustr = question;
        var ob = job.get(0);
        var roxid = ob.getAttribute("roxid");
        var lines = menustr.split("\n");
	var svg = "<table>";
        for (var i = 1; i < lines.length; i++) {
		switch (i) {
			case 4: case 8: case 11: case 14: case 19:
			svg += "<tr><td>..................</td></tr>";
		}
                svg += "<tr><td><b class='menuitem' menutgt='" + roxid + "' >" + lines[i] + "</b></td></tr>";
        }
	svg += "</table>";
	var txt = spannedTextContent(ob);
        var tp = job.attr("type");
        if (tp=="list" || tp=="listdescriptor") {
                txt = job.attr("listtype")
        }	
	//$("#bottomstatus").empty();
	$("#tdmenu").empty();
        $("#menuarea").html(svg);
	var atxt = "<textarea id='menutext'></textarea>";
	var hb = "<button class='hydemenubutton' onclick='hydeMenu(null);'>X<br/>close</button>";
	var bt = "<button id='tgtelement' onclick='goToTgtElement()'>⇈<br/>show</button>";
	$("#tdmenu").html("<table><tr><td>" + atxt +"</td><td>"+ bt + "</td><td>" + hb + "</td></tr></table>");
        $("#tgtelement").attr("tgt0", roxid);
        $("#menutext").keydown(menuTextKeyPress);
	$("#menutext").keyup(takeLastToHint);
	$("#menutext").attr("menutgt",roxid);
	$("#menutext").val(txt);
        $("#menutext").focus();
	$("#menutext").select();
	$("#tbhints").css("display","inline");
        $(".menuitem").click (processClick);
        if (tp=="list" || tp=="listdescriptor") {
                $("#menutext").attr("class","menutextlist");
        } else {
                $("#menutext").attr("class","menutextatom");		
	}
}

function txtToHint() {
	$("#hinttxt").val($("#menutext").val());
}

function hintProcess(evt) {
    var job = $(evt.srcElement);
    var jtxt = job.html();
    if (jtxt=="case") {
	var texto = $("#hinttxt").val();
	var buf = "";
	for (var i = 0; i < texto.length; i++) {
		var ch = texto.charAt(i);
		buf += ch.toUpperCase() == ch ? ch.toLowerCase() : ch.toUpperCase(); 
	}
	$("#hinttxt").val(buf);
    } else {
	$("#hintslist").empty();
	var file = $("#curfileid").html();
	job.attr("file",file);
	job.attr("operation", jtxt);
	job.attr("texto", $("#hinttxt").val());
	$("#curhint").html( jtxt);
	sendMpRequest("hint", "rox-utils", job.get(0));
   }
}

function copyPasteOperation (job, operation) {
	$("#bottomstatus").empty();
        var ob = job.get(0);
        job.attr("operation", operation);
	if (operation=="update") {
		editTextContent(job);
	}
        sendMpRequest("editelement", "rox-utils", ob);
}

function editTextContent (job) {
	var ob = job.get(0);
	var answer = $("#menutext").val();
	ob.innerHTML = answer;
	ob.textContent = answer;
}

function elementmenuClick(evt) {
}

function sepbuttonClick () {
	sendMpRequest("topsubmenu","rox-utils", jQuery.parseXML("<dummy/>"));
}

function openLispFile (evt) {
        var job = $(evt.srcElement);
	var valor = job.val();
	$("#filename").val(valor);
}

//function openHyperspec (job) {
//	var ob = job.get(0);
//	sendMpRequest("loadhyperspec","rox-utils", ob);
//}

function parseFileName(path) {
	var name = path;
	var pos1 = name.lastIndexOf("/");
	var pos2 = name.lastIndexOf("\\");
	var pos = Math.max(pos1, pos2);
	if (pos<0) {
		return path;
	} else {
		try {
			return name.substr(pos+1);
		} catch (e) {
			return name.substr(pos);
		}
	}
}

function updateTitle(){
	var path = $("#curfileid").html();
	var name = parseFileName(path);
	$("title").html(name);
}

function hintComplete(txt, focus) {
	var job = $("#hinttxt");
	if (focus==true) {
		job.val(txt);
		job.focus();	
		job.select();
	}
        var file = $("#curfileid").html();
        job.attr("file",file);
        job.attr("operation", "select");
        job.attr("texto", txt);
        sendMpRequest("hint", "rox-utils", job.get(0));
}

