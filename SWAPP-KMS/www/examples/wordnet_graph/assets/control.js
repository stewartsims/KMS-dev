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

// mouse move capture for IE
var IE = document.all?true:false;
if (!IE) document.captureEvents(Event.MOUSEMOVE)

    function handleQuery( r, params ) {
      if (r.readyState == 4) {
        var response = JSON.parse(r.responseText);
        response = response.bindings[0].Json;
        // obtain the array of synsets and loop through it
        var synsets = response.synsets;
        for( var i=0; i<synsets.length; i++ ) {
          var id = synsets[i].id;
          var synNode = control.addNode( '', true );

          var words = synsets[i].words;
          var pos = synsets[i].pos;
          
          for( var j=0; j<words.length; j++ ) {
            var thisWord = words[j];

            if ( thisWord != word ) {
              var termNode = control.addNode( "<a href=\"?"+thisWord+"\">"+thisWord+"</a>", false );
              control.addEdge( synNode, termNode );
            }

            var domNode = graphui.getNode( synNode.id );

            if ( pos == 'n' ) { pos = 'noun'; }
            if ( pos == 'v' ) { pos = 'verb'; }
            if ( pos == 'a' ) { pos = 'adjective'; }
            if ( pos == 's' ) { pos = 'adjective'; }
            if ( pos == 'r' ) { pos = 'adverb'; }

            domNode.innerHTML = "<img src=\"" + color[pos] + "\">";
          
            var gloss = synsets[i].gloss.slice(1, -1);
			
            var context = new Object();
            context.gloss = gloss;
            context.pos = pos;
            context.obj = domNode;
			
            // add a mouseoverent handler
            domNode.onmouseover = new EventHandler( context, 
              function( context, e ) {
              
                if ( graph['selectedNode'] == -1 ) {
                  gloss = context.gloss;
                  gloss = gloss.replace( /\`/g, "\'" );
                  gloss = gloss.replace( /\"/g, "\'" );
                  gloss = gloss.replace( /#(.*?)#/g, "\"<i>$1\"</i>" );
  
                  var definition = '<div style="border-bottom:1px dashed #444444;margin-bottom:4px;padding-bottom:4px"><b>Definition:</b></div> (' + context.pos + '): ' + gloss;
			
                  document.getElementById('gloss').innerHTML=definition; 

                  var target = context.obj;
                  if ( target ) {
             	    document.getElementById('gloss').style.left=parseInt(target.style.left) - 210;
                    document.getElementById('gloss').style.top=parseInt(target.style.top) - 10;
                  }                
                  document.getElementById('gloss').style.visibility="visible";
                  timer.stop();
                }
              }
            );

            // add a mouseoutvent handler
            domNode.onmouseout = new EventHandler( gloss, 
              function( gloss, e ) {
                document.getElementById('gloss').style.visibility="hidden"; 
                timer.start();
              }
            );

          }
        }
      }
    }


// UserControl:
var UserControl = function(){};
UserControl.prototype = {

  initialize: function( timer, graph, ui ) {
    this['timer'] = timer;
    this['graph'] = graph;
    this['ui'] = ui;
    this['lastSelectedNode']=-1;
    this['liveNode']=-1;
    var context=this;

    // append the mousemove eventhandler
    window.onresize = function( e ) {
      return(
        context.resizeFrame( e )
      );
    };

    // append the mousemove eventhandler
    document.onmousemove = function( e ) {
      return(
        context.moveSelected( e )
      );
    };

    // append the mouseup eventhandler
    document.onmouseup = function( e ) {
      return(
        context.unselectNode( e )
      );
    };
  },

  // resize the frame when the windo size changes
  resizeFrame: function( e ) {
    var FRAME_WIDTH;
    var FRAME_HEIGHT;

    if (IE) {
      FRAME_WIDTH = document.body.offsetWidth;
      FRAME_HEIGHT = document.body.offsetHeight;
    } else {
      FRAME_WIDTH = window.innerWidth;
      FRAME_HEIGHT = window.innerHeight;
    }

    FRAME_WIDTH -= (parseInt(document.getElementById('frame').style.left)*2);
    FRAME_HEIGHT -= (parseInt(document.getElementById('frame').style.top)+20);

    this.ui['frame_width']=FRAME_WIDTH;
    this.ui['frame_height']=FRAME_HEIGHT;

    this.graph['frame_width']=FRAME_WIDTH;
    this.graph['frame_height']=FRAME_HEIGHT;

    graph.origin.position['x'] = parseInt(this.graph['frame_width']/2);
    graph.origin.position['y'] = parseInt(this.graph['frame_height']/2);

    this.ui.drawOrigin( this.graph['origin'] );
  },

  // make this node 'selected' in the graph
  selectNode: function( e, node ) {
    if ( node == this['liveNode'] ) {
        this.unattach( e, this['liveNode'] );
    } else if ( this['liveNode'] != -1 ) {
      this.graph.addEdge( graph.getNode(this['liveNode']), graph.getNode(node), 48 );
    }
    this['graph'].setSelected( node );
  },

  focusNode: function( e, node ) {
    this['liveNode'] = node;
    graphui.setLive( graph.getNode(node) );
  },

  unattach: function( e, node ) {
    if ( this['liveNode'] != -1 ) {
      var at = this['liveNode'];
      graphui.unSetLive( graph.getNode(node) );
      this['liveNode'] = -1;
    }
  },

  // unselect this node in the graph
  unselectNode: function() {
    this['lastSelectedNode'] = this['graph'].getSelected();
    this['graph'].clearSelected();
  },

  // handle mouse movement when a node is selected
  moveSelected: function( e ) {
    if ( graph.hasSelected() ) {
      var selectedNode = graph.getSelected();

      var tempX;
      var tempY;
      // get the cursor position
      if (IE) {
        tempX = event.clientX + document.body.scrollLeft;
        tempY = event.clientY + document.body.scrollTop;
      } else {
        tempX = e.pageX;
        tempY = e.pageY;
      }

      tempX -= graphui.frame_left;
      tempY -= graphui.frame_top;

      // adjust for center
      tempX -= parseInt( this.ui.getNode( selectedNode ).style.width ) / 2;
      tempY -= parseInt( this.ui.getNode( selectedNode ).style.height ) / 2;

      // set the node position
      graph.getNode( selectedNode ).position['x']=tempX;
      graph.getNode( selectedNode ).position['y']=tempY;

      // if the timer is interupted, we still want the graph to move while we move the selected node around
      if ( this.timer['interupt'] ) {
        this.graph.applyForce();
      }
    }
  },

  // 
  addEdge: function( node1, node2, weight ) {
    if ( !weight ) { weight=48; }
    this.graph.addEdge( node1, node2, weight );
  },

  // 
  addNode: function( text, originEdge, weight ) {

    var node;
    node = graph.addNode( 1, text );

    var domNode = this.ui.getNode( node.id );
    var context = this;

    if ( originEdge ) { 
      if ( !weight ) { weight=92 }
      this.graph.addOriginEdge( node, weight );
      this.ui.addEdge( node, graph.getOrigin(), weight );
    }

    // add a mousedown event handler
    domNode.onmousedown=function(e) {
      return(
        context.selectNode(e, node.id)
      );
    };

    return node;
  }
}
