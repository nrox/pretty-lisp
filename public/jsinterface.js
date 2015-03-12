
/**
* pretty-LISP
* Javascript Interface
* Copyright Nuno Rocha 2012
* LICENSE: Do whatever with this file.
*/

$(document).ready(function(){
	setHintButtons();
});

/** switch to another page. Currently just one page, the editor. */

function toggleSeparator(sepID) {
        $(".separator").css("display","none");
        $("#" + sepID).css("display","inline");
	var ob = $("<dummy/>").get(0);
        sendRequestToServer("topsubmenu", ob);
}

/** centers the window to see the element */

function goToElement(roxid){
	try {
		var off = $("[roxid='" + roxid + "']").offset();
		$('body').animate({ scrollTop: (off.top - 200), scrollLeft: (off.left-700) }, 200);
	} catch (e) {
		alertError("goToElement('" + roxid + "')\n" + e);
	}
}

/** trims and removes char as # : ' to hightlight similar texts */  

var TO_TRIM = {"#": 0, "\"": 0, ":": 0, "'": 0, ",": 0, "`": 0, " ": 0, ";": 0};

function formatForHighlighting(str) {
	if (str==undefined) {
		return null;
	}
	var replaced = true;
	while (replaced) {
		replaced = false;
		if (str.length==0) {
			return "";
		}
		if ( TO_TRIM[ str.charAt(0) ] != undefined ) {
			str = str.substring(1);
			replaced = true;
		}
                if (str.length==0) {
                        return "";
                }
                if ( TO_TRIM[ str.charAt( str.length - 1 ) ] != undefined ) {
                        str = str.substring(0, str.length - 1);
			replaced = true;
                }
	}	
	return str.toLowerCase();
}

/** highlight select element and the one with similar text */

function highlightSelected(roxid){
   try {
	var job =  $("[roxid='" + roxid + "']");
        //restore normal classes values
        $("[bkclass]").each( function (index) {
                $(this).attr("class", $(this).attr("bkclass"));
        });
        $("[bkclass]").removeAttr("bkclass");
        var elType = job.attr("type");
        var isList = elType == "list";
        var isComment = job.attr("class").indexOf("comment")>-1;
        var isString = job.attr("class").indexOf("string")>-1;
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
        var txt = formatForHighlighting( visibleText(job.get(0)) );
        if ( txt!="" && !isList && !isComment && !isString){
		//select all elements with similar text
		var jSimilar = $(":contains(" + txt + ")");
		jSimilar = jSimilar.add( $(":contains(" + txt.toUpperCase() + ")") );
                jSimilar.each (function (index) {
		    //if it is not of type atom, dont highlight
		    if ( $(this).is("[type='atom']") == false) {
			return;
		    }
                    var similar = formatForHighlighting ( visibleText( $(this).get(0) ) );
                    if (similar==txt){
                        if ( $(this).attr("roxid") != job.attr("roxid") ){
                                $(this).attr("bkclass", $(this).attr("class"));
                                $(this).attr("class", "highlight");
                        }
                    }
                });
        }
   } catch (e){
        alertError("highlightSelected " + roxid + "\n" + e);
   }
}

/**returns the text content of a svg text element, considering tspan*/

function visibleText(textDOM) {
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


/** this function is called on element click and to set focus on the element */

function roxEvent(roxid, evtType){
	var job = $("[roxid='" + roxid + "']");
	if (job.get().length==0) {
		alertError ("roxEvent: not found" + roxid);
		return;
	}
	if (job.get().length>1) {
		alertError("roxEvent: several " + roxid);
	}
	if ( (evtType=="dblclick") || (evtType=="lastclick") ) {
		highlightSelected(roxid);
		activateForEditing(roxid);
		
	}
}

/** file operations which must be confirmed */
var CONF = ["load", "saveAll", "closeAll", "openAll"];
function confirmFileOp (operation) {
	for (var o in CONF) {
		if (CONF[o] == operation) {
			return ( confirm(operation + " ? ") == true ) ;
		}
	}
	return true;
}

/** controls file operations */

function fileOp(operation) {
        hideMenu();
	$("#bottomstatus").empty();
        var newvalue = $("#fileslist").val();
        if (operation=="saveas") {
                var answer = prompt ("Save as:", $("#curfileid").html());
                if (answer!=null) {
                        newvalue = answer;
                } else {
                        return;
                }
        } else if (operation=="new") {
                var answer = prompt ("New:", $("#fileslist").val());
                if (answer!=null) {
                        newvalue = answer;
                        $("#curfileid").html(answer);
                } else {
                        return;
                }
        } else if (operation=="browse" || operation=="load") {
                newvalue =  $("#fileslist").val();
        } else if (operation=="open") {
                $("#curfileid").html( $("#fileslist").val() );
                $("#tbhints").css("display","inline");
        }      
	if ( confirmFileOp(operation) == false ) {
		return;
	}  
        var valor = $("#curfileid").html();
        updateTitle();
	var ob = $("<button operation='" + operation + "' value='" + valor + "' newvalue='" + newvalue + "'>" + operation + "</button>").get(0);
        sendRequestToServer("fileoperation", ob);
}

/** Keyboard chortcuts are activated first pressing ctrl, releasing it, and then pressing the operation key 
    the folowing functions help controling this 
*/

var AUX_KEY = 0;
function resetKeyControl() {
	AUX_KEY = 0;
}
function updateKeyControl() {
        AUX_KEY++;
}
function keyControlActivated() {
        return AUX_KEY==0;
}
function keyControlInactive() {
        return AUX_KEY==1;
}
function activateKeyControl() {
        AUX_KEY = -1;
}

/** keeps editing commands shortcuts and operations, displayed in the left menu */

var menuItems = {};

/** alternative shortucts */

var otherShortcuts = {13: "Update", 27: "Cancel", 37: "Left", 38: "Up", 39: "Right", 40: "Down"};
var otherShortcutsHelpText = {"Update": "C-RET", "Cancel": "ESC",  "Left": "C ←", "Up": "C ↑", "Right": "C →", "Down": "C ↓"};

/** populates menuItems */

function addAllMenuItems() {
     var curMenuItem = 0;
     var addMenuItem = function (itemKey, itemOperation) {
	  if (itemKey!==undefined && itemOperation!==undefined) {
		itemKey = (itemKey + "").toLowerCase();
	  	menuItems[curMenuItem++] = {key: itemKey, operation: itemOperation};
	  } else {
		menuItems[curMenuItem++] = null;
	  }
     }
     menuItems = {};
     addMenuItem("0", "Cancel");
     addMenuItem("1", "Update");
     addMenuItem("2", "Execute");
     addMenuItem();
     addMenuItem("x", "Cut");
     addMenuItem("c", "Copy");
     addMenuItem("v", "Paste");
     addMenuItem("f", "Delete");
     addMenuItem();
     addMenuItem("a", "Before");
     addMenuItem("s", "Inside");
     addMenuItem("d", "After");
     addMenuItem();
     addMenuItem("q", "Free");
     addMenuItem("w", "Surround");
     addMenuItem("e", "Comment");
     addMenuItem();
     addMenuItem("z", "Undo");
     addMenuItem("y", "Redo");
     addMenuItem();
     addMenuItem("t", "Transpose");
     addMenuItem("k", "Collapse");
     addMenuItem("l", "CollapseAll");
     addMenuItem("o", "Expand");
     addMenuItem("p", "ExpandAll");
     addMenuItem();
     addMenuItem("b", "Left");
     addMenuItem("h", "Up");
     addMenuItem("m", "Right");
     addMenuItem("n", "Down");
} 

addAllMenuItems();

/** constructs the html to be used as left menu */

function makeEditMenu (menutgt) {
	var count = 0;
	var buff = "<table>";
	while (true) {
		var item = menuItems[count];
		if (item===undefined){
			break;
		} else if (item===null) { //separator
			buff += "<tr><td>..................</td></tr>";
		} else {
			buff += "<tr><td><nobr><b class='menuitem' menutgt='" + menutgt + "' ";
			buff += "operation='" + item.operation + "' ";
			buff += "onclick='sendEditingCommand(\"" + menutgt + "\",\"" +  item.operation + "\");'";
			var other = otherShortcutsHelpText[item.operation];
			other = other != undefined ? " <span class='alterkey'>" + other + "</span>" : ""; 
			buff += ">" + item.key + " - " + item.operation + "</b>" + other + "</nobr></td></tr>";
		}
		count++;
	}
	buff += "</table>";
	return buff;;
}

/** returns the associated operation to the char key */

function keyOperation (itemKey) {
	itemKey = (itemKey + "").toLowerCase();
        var count = 0;
        while (true) {
                var item = menuItems[count];
                if (item===undefined){
                        return null;
		} else if (item===null) {
                } else if (item.key==itemKey) {
                        return item.operation;
                }
                count++;
        }
}

/** returns the associated key to the operation */

function operationKey (itemOperation) {
        var count = 0;
        while (true) {
                var item = menuItems[count];
                if (item===undefined){
                        return null;
                } else if (item===null) {
                } else if (item.operation==itemOperation) {
                        return item.key;
                }
                count++;
        }
}

/** with the keyboard event identify the associated operation and send the request, calling finnaly sendEditingCommand, if the operation was identified */

function processShortcut(evt) {
        try {
                updateKeyControl();
		var keyCode = evt.keyCode;
		var ctrReleased = keyControlActivated();
		var ctrAlsoPressed = evt.ctrlKey;
		var ctrPressedNow = ( keyCode==17 );
                var altAlsoPressed = evt.altKey ;
		var altPressedNow = ( keyCode==18 ); 
                var operation = otherShortcuts[keyCode];
		var sendOperation = false;
		if (operation != undefined ) {
	                if ( operation=="Update" && !ctrAlsoPressed) {
        	                return; //to update with enter key, ctrl key must be also pressed
	                } else if ( !ctrReleased && ( operation=="Down" || operation=="Up" || operation=="Left" || operation=="Right" ) ) {
				return; //to use cursor keys to navigate the ctrl key must be first pressed, tthen released 
			} else {
				//using ESC to Cancel needs no additional key 
			}
			sendOperation = true; 
		}
                if(ctrReleased && !ctrAlsoPressed && !sendOperation) {
                        var keyChar = String.fromCharCode(keyCode);
                        operation = keyOperation(keyChar);
			if (operation == undefined ) {
				return;
			}
			sendOperation = true;
                }
		if (sendOperation) {
                        var roxid = $("[operation=" + operation + "]").attr("menutgt");
                        sendEditingCommand(roxid, operation);
                        return;

		}
                //first key (CTRL), or key combination (CTRL+ALT), to enable the next key as a command
                if ( keyControlInactive() && ctrPressedNow || ctrPressedNow && altAlsoPressed || altPressedNow && ctrAlsoPressed || ctrAlsoPressed && altAlsoPressed ){
                        activateKeyControl();
                        return;
                }
        } catch (e){
                alertError("processShortcut:\n" + e);
        }
}

/** sends the last word of edit text area to hints text area */

function setLastEditWordToHint(evt) {
	var txt = $("#menutext").val().split(" ");
	txt = txt[txt.length-1];
	txt = txt.replace("(","").trim();
	$("#hinttxt").val(txt);
}

/** send to server a request with the edting command */

function sendEditingCommand(roxid, operation) {
	//get the editing element (jquery object)
	var job = $("[roxid='" + roxid + "']");
	//the dom object
	var ob = job.get(0);
	//set the operation as an att of the editing element (!)
        job.attr("operation", operation);
        if (operation.toLowerCase()=="update") {
	        var text = $("#menutext").val();
        	ob.innerHTML = text;
        	ob.textContent = text;
        }
        //clean the status
        $("#bottomstatus").empty();
	//send the edited element with the request.
        sendRequestToServer("editelement", ob);
}

/** hides left menu and edit text area */

function hideMenu() {
        $("#tbleftmmenu").css("display", "none");
        $("#tdmenu").css("display", "none");
        $("#menuarea").empty();
}

/** builds the menu for the element and prepare it for receiving editing commands. Open the editor text area. */

function activateForEditing(roxid) {
        resetKeyControl();
	var job = $("[roxid='" + roxid + "']");
        var ob = job.get(0);
        var roxid = ob.getAttribute("roxid");
        var txt = visibleText(ob);
        var tp = job.attr("type");
        if (tp=="list" || tp=="listdescriptor") {
                txt = job.attr("listtype")
        }
        $("#menuarea").html( makeEditMenu(roxid) );

        $("#tgtelement").attr("tgt0", roxid);
	$("#tgtelement").attr("onclick", "goToElement('" + roxid + "')");

        $("#menutext").unbind ("keydown", processShortcut);
        $("#menutext").keydown(processShortcut);
        $("#menutext").unbind ("keyup", setLastEditWordToHint);
        $("#menutext").keyup(setLastEditWordToHint);

        $("#menutext").attr("menutgt",roxid);
        $("#menutext").val(txt);

        $("#tbhints").css("display","inline");
        if (tp=="list" || tp=="listdescriptor") {
                $("#menutext").attr("class","menutextlist");
        } else {
                $("#menutext").attr("class","menutextatom");
        }
        $("#tdmenu").css("display", "inline");
	$("#tbleftmmenu").css("display", "inline");
        $("#menutext").focus();
        $("#menutext").select();
}

/** sends the text in edit text area to hints text area */
 
function txtToHint() {
	$("#hinttxt").val($("#menutext").val());
}

/** to be triggered by an hint button. Identifies the hint operation and sends request to server. */
 
function hintButtonClick(hint) {
    var job = $("[hint='" + hint + "']");
    if (hint=="case") {
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
        job.attr("operation", hint);
        job.attr("texto", $("#hinttxt").val());
        $("#curhint").html( hint);
        sendRequestToServer("hint", job.get(0));
   }
}

/** makes the html for the hints button and set it */
function setHintButtons() {
	var hints = ["complete", "similar", "search", "def", "suggest", "examples", "history", "hyperspec", "case"];
	var columns = 3;
	var table = "<table>";
	var to = hints.length + ( columns - hints.length % columns) % columns;
	for (var i = 0; i < to; i++) {
		if ((i % columns) == 0) {
			table += "<tr>";
		}
		if (i < hints.length) {
	                table += "<td><button hint='" +  hints[i]+ "' class='hintbutton' onclick=\"hintButtonClick('" + hints[i] + "')\" >" + hints[i] + "</button></td>";	
		} else {
			table += "<td />";
		}
		if ((i % columns) == (columns-1)) {
                        table += "</tr>";
                }
	}
	table += "</table>";
	$("#hintbuttons").html(table);
}

/** called onclick from #fileslist, the list of paths */

function markSelectedFilename () {
        var job = $("#fileslist");
        var valor = job.val();
        $("#filename").val(valor);	
}

/** from the path, retrives the filename and extension */

function parseFileName(path) {
	var name = path;
	//for windows and linux
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

/** replaces the tab title with the file name and extension */

function updateTitle() {
	var path = $("#curfileid").html();
	var name = parseFileName(path);
	$("title").html(name);
}

/** finish procesing onclick of an hints list item */

function hintComplete(txt, focus) {
	if (focus==true) {
	        var job = $("#hinttxt");
		job.val(txt);
		job.focus();	
		job.select();
	}
}

var signalMemo;
function waitSignal (signalOn) {
	if (signalOn) {
		if (signalMemo == undefined) {
			signalMemo = $("#topmenu").css("background-color");
		}
		$("#topmenu").css("background-color", "grey");
	} else {
        	$("#topmenu").css("background-color", signalMemo);
	}
}
