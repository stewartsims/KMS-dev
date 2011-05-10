function switchPageIfNeeded() {
    if (!parent.display.document.getElementById('data')) {
        parent.display.location.href = 'display.html';
    }
}

function working() {
    feedback = setTimeout(function() {
        document.getElementById("feedback").innerHTML = "<img align='top' src='../img/working.gif'/>";
    }, 200);
}
	    

function abortGet() {
    YAHOO.util.Connect.abort(req);
    document.getElementById("feedback").innerHTML = "";
    document.getElementById("get").disabled = false;
    document.getElementById("abort").disabled = true;
}


function scroll_to_end() {
    var frame = parent.display;
    var node  = frame.document.getElementById('data').lastChild;
    var y = parseInt(node.offsetTop, 10);
    frame.scrollTo(0, y);
}


function paste(s, color) {
    var e = parent.path.document.body;
    e.innerHTML = s;
    e.style.color = color;
    e.style.fontFamily = 'monospace';
    e.style.fontSize = '80%';
}


function clearFields(fields) {
    for (var i in fields) {
        document.getElementById(fields[i]).value = '';
    }
}


function encode64(input) {
    var output = "";
    var chr1, chr2, chr3 = "";
    var enc1, enc2, enc3, enc4 = "";
    var i = 0;
    var keyStr = "ABCDEFGHIJKLMNOP" +
                "QRSTUVWXYZabcdef" +
                "ghijklmnopqrstuv" +
                "wxyz0123456789+/" +
                "=";
    do {
      chr1 = input.charCodeAt(i++);
      chr2 = input.charCodeAt(i++);
      chr3 = input.charCodeAt(i++);

      enc1 = chr1 >> 2;
      enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
      enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
      enc4 = chr3 & 63;

      if (isNaN(chr2)) {
         enc3 = enc4 = 64;
      } else if (isNaN(chr3)) {
         enc4 = 64;
      }

      output = output +
         keyStr.charAt(enc1) +
         keyStr.charAt(enc2) +
         keyStr.charAt(enc3) +
         keyStr.charAt(enc4);
      chr1 = chr2 = chr3 = "";
      enc1 = enc2 = enc3 = enc4 = "";
    } while (i < input.length);

    return output;
}
        