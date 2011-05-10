//
// This work is licensed under the Creative Commons Attribution 2.5 License. To 
// view a copy of this license, visit
// http://creativecommons.org/licenses/by/2.5/
// or send a letter to Creative Commons, 543 Howard Street, 5th Floor, San
// Francisco, California, 94105, USA.
//
// All copies and derivatives of this source must contain the license statement 
// above and the following attribution:
//
// Author: Kyle Scholz      http://kylescholz.com/
// Copyright: 2006
//

    var threadCount=0;

    function Parameters() {
    }

    function getRequestObject() {
      var req;
      if (window.XMLHttpRequest) {
        try {
          req = new XMLHttpRequest();
        } catch(e) {
          req = false;
        }
        // branch for IE/Windows ActiveX version
      } else if(window.ActiveXObject) {
          try {
          req = new ActiveXObject("Msxml2.XMLHTTP");
        } catch(e) {
          try {
            req = new ActiveXObject("Microsoft.XMLHTTP");
          } catch(e) {
            req = false;
          }
        }
      }
      return req;
    }

    function get( url, handler, params ) {
      var request = getRequestObject();
      try {
        request.open("GET", url, true);
        request.onreadystatechange = function() {
          handler( request, params );
        }
//        threadIn();
        request.send(null);
        delete request;
      } catch( e ) {
        alert("(Mozilla) - " + e);
      }
    }

// merge this with get
    function post( url, handler, post, params ) {
      var request = getRequestObject();
      try {
        request.open("POST", url, true);
        request.onreadystatechange = function() {
          handler( request, params );
        }
//        threadIn();
        request.send(post);
        delete request;
      } catch( e ) {
        alert("(Mozilla) - " + e);
      }
    }

    function getTextFromNode( xml ) {
      // Moz
      if (xml.textContent) return xml.textContent;
      // IE
      if (xml.innerText) return xml.innerText;
      // other
      if (xml.text) return xml.text;
    }

