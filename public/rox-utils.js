/**
 * Some utilities
 * by Nuno Rox 2011
 */


/**
 * Returns a random color
 */
function randomColor() {
	var ret="rgb(";
	ret += Math.round(100*Math.random()) + "%,";
	ret += Math.round(100*Math.random()) + "%,";
	ret += Math.round(100*Math.random()) + "%)";
	return ret;
}

function hashMapToString(array, separator) {
	var ret = "";
	for (obj in array) {
		ret += obj + "=" + array[obj] + separator;
	}
	return ret;
}

function readFile(filePath) {
	var xmlhttp;
	if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
		xmlhttp=new XMLHttpRequest();
	} else {// code for IE6, IE5
		xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
	}
	var ret = "";
	xmlhttp.onreadystatechange=function() {
		if (xmlhttp.readyState==4 && xmlhttp.status==200) {
			ret = xmlhttp.responseText;
		}
	}
	xmlhttp.open("GET",filePath,true); //syncro.
	xmlhttp.send();
	return ret;
}
