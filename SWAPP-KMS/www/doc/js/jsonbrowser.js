	

function isArray(obj) {
    return obj && (obj.constructor == Array);
}

function html(s) {
	return s.toString().replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}
function display(obj, path) {
	if (isArray(obj)) {
		return this.array(obj, path);
	} else if (typeof obj == "boolean") {
		return this.bool(obj);
	} else if (typeof obj == null) {
		return '<span title="null" class="NULL">null</span>';
	} else if (typeof obj == "number") {
		return '<span title="Number" class="NUMBER">' + obj.toString() + '</span>';
	} else if (typeof obj == "string") {
		return this.string(obj);
	} else if (typeof obj == "object") {
		return this.obj(obj, path);
	}
	alert(obj);
	return '<span class="IDK">[Unknown Data Type]</span>';
}
function array(a, pathIn) {
	var body = '';
	for (var x = 0; x < a.length; x++) {
		var pathOut = pathIn + '[' + x + ']';
		body += '\n<tr><th class="prop" onclick=\'parent.leftFrame.paste(\"'+pathOut+'\",\"black\");\'>' + x + 
		         '</th><td class="prop">'  + this.display(a[x], pathOut) + '</td></tr>';
	}
	if (body.length) {
		return '<table class="prop ARRAY"><caption>ARRAY</caption>' + body + '</table>';
	}
	return '<span title="Array" class="ARRAY">[]</span>';
}
function bool(b) {
	return (b) ?
		'<span title="Boolean" class="BOOL">true</span>' :
		'<span title="Boolean" class="BOOL">false</span>';
}
function string(s) {
	if (s.length === 0) {
		return '<span title="String" class="EMPTY">""</span>';
	}
	if (document.getElementById("jsonTrunc").checked && s.length > 70) {
		s = s.substring(0, 70) + '\u2026'; // 2026 = "..."
	}
	return '<span title="String" class="STRING">' + this.html(s) + '</span>';
}
function obj(o, pathIn) {
	var body = '';
	for (var x in o) {
		if (o.hasOwnProperty(x)) {
			var pathOut = pathIn + '.' + x;
			body += '<tr><th class="prop" onclick=\'parent.leftFrame.paste(\"'+pathOut+'\",\"black\");\'>' + this.html(x) + 
			        '</th><td class="prop">' + this.display(o[x], pathOut) + '</td></tr>';
		}
	}
	if (body.length) {
		return '<table class="prop OBJ"><caption>OBJECT</caption>' + body + '</table>';
	}
	return '<span title="Object" class="OBJ">{}</span>';
}


