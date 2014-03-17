var ctrlpressed
var config
var background
var wearefocused = true
var slowdown = false
var framerate
var maxframerate = 100
var minframerate = 1
var mousedown = false
var cachedimages = false
var updatedparams = false
var loadedcoords = false
var savedcoordsexist = false

$.extend({
    getUrlVars: function( item ) {
        var vars = {};
        var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) { vars[key] = value; });
        return vars[item];
    }
});

var focus = $.getUrlVars('focus');
if (typeof (focus) === 'undefined') {wearefocused = false;}

// We abuse the rightmousebutton for something else:
$(document).bind("contextmenu", function(e) {
    return false;
});

function preloadImages(array) {
    if (!preloadImages.list) {
        preloadImages.list = [];
    }
    for (var i = 0; i < array.length; i++) {
        var img = new Image();
        img.src = 'images/' + array[i];
        preloadImages.list.push(img);
    }
}

function TestForSavedData()
{
    var page = escape(document.URL)
    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "retrieve", variable: "coordinates_"+page }
    })
    .done(function(msg) {
        if (msg !== "") { savedcoordsexist = true }
    })
}

function ClearSavedData()
{
    var page = escape(document.URL)
    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "delete", variable: "coordinates_"+page }
    })

    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "delete", variable: "bounds_"+page }
    })
    
    .done(function(msg) {
        // Also reload:
        location.reload();
    })

}

function SaveView(){
    var page = escape(document.URL)

    // Try to store all positions in coords
    var coords = []
    sys.eachNode(function(node, unused){
	thisnode = sys.getNode(node.name);
	newnode = {}
	newnode['name'] = node.name
	newnode['x'] = thisnode.p.x
	newnode['y'] = thisnode.p.y
	coords.push(newnode)
    })
    jsoncoords = JSON.stringify(coords)
console.log(jsoncoords)

    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "store", variable: "coordinates_"+page, content: jsoncoords }
    })


    // Need the view settings also, otherwise it will get centered
    bounds = sys.getBounds();
    bounddata = {}
    bounddata['brx'] = bounds.bottomright.x
    bounddata['bry'] = bounds.bottomright.y
    bounddata['tlx'] = bounds.topleft.x
    bounddata['tly'] = bounds.topleft.y
    jsonbounddata = JSON.stringify(bounddata)
console.log(jsonbounddata)

    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "store", variable: "bounds_"+page, content: jsonbounddata}
    })
    .done(function(msg) {
        console.log("Saved view");
    })



}

function RestoreView(sys){
    // If this is the first time they're loaded, check server cookie for saved coordinates:
    if (loadedcoords == true) { return }

    var page = escape(document.URL)
    console.log('Trying to restore saved locations');

    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "retrieve", variable: "coordinates_"+page }
    })
    .done(function(coords) {
	if (coords !== '') {
console.log(coords);
	    coords = JSON.parse(coords)
		$.each(coords, function( count, onesavednode ) {
		node = sys.getNode(onesavednode.name)
		if (node == 'undefined') {
			console.log("Node " + onesavednode.name + " not found.");
		}else{
			node.p.x = onesavednode.x
			node.p.y = onesavednode.y
			node.fixed = true
		}
	    })
	    console.log("Restored coordinates");
	}
    })

    // Also restore view:
    $.ajax({
        type: "POST",
        url: 'ajax_cookie.php',
        data: { command: "retrieve", variable: "bounds_"+page }
    })
    .done(function(bounds) {
	if (bounds !== '') {
console.log(bounds);
	    bounddata = JSON.parse(bounds)
	    curbounds = sys.getBounds()
	    curbounds.bottomright.x = bounddata['brx']
	    curbounds.bottomright.y = bounddata['bry']
	    curbounds.topleft.x = bounddata['tlx']
	    curbounds.topleft.y = bounddata['tly']
            sys.screenStep(0);
	    sys.setBounds(curbounds)
	    console.log("Restored bounds");
	}
    })
}


(function($){
    DeadSimpleRenderer = function(canvas){
        var canvas = $(canvas).get(0)
        var ctx = canvas.getContext("2d");
        var gfx = arbor.Graphics(canvas)
        var particleSystem = null
        var sys;
        var detailnode = null

        var that = {

            // the particle system will call the init function once, right before the
            // first frame is to be drawn. it's a good place to set up the canvas and
            // to pass the canvas size to the particle system

            init:function(system){
                // save a reference to the particle system for use in the .redraw() loop
                particleSystem = system

                // inform the system of the screen dimensions so it can map coords for us.
                // if the canvas is ever resized, screenSize should be called again with
                // the new dimensions
                $(window).resize(that.resize)
                that.resize()
                particleSystem.screenSize(canvas.width, canvas.height) 
                particleSystem.screenPadding(80) // leave an extra 80px of whitespace per side
                particleSystem.screenStep(1)
                if (wearefocused) { particleSystem.screenPadding(180); }

                // Prepare bg image:
                if (typeof(config) !== 'undefined' && typeof(config.BGIMG) !== 'undefined' && !wearefocused ) {
                    background = new Image();
                    background.src = config.BGIMG;
                }

                that.initMouseHandling()
            },

            // redraw will be called repeatedly during the run whenever the node positions
            // change. the new positions for the nodes can be accessed by looking at the
            // .p attribute of a given node. however the p.x & p.y values are in the coordinates
            // of the particle system rather than the screen. you can either map them to
            // the screen yourself, or use the convenience iterators .eachNode (and .eachEdge)
            // which allow you to step through the actual node objects but also pass an
            // x,y point in the screen's coordinate system

            initMouseHandling:function(){
                // no-nonsense drag and drop (thanks springy.js)
                selected = null;
                nearest = null;
                var dragged = null;
                var oldmass = 1

                $(canvas).mousedown(function(e){
                    var pos = $(this).offset();
                    var p = {x:e.pageX-pos.left, y:e.pageY-pos.top}
                    selected = nearest = dragged = particleSystem.nearest(p);

                    // If we are slowed down, speed up :)
                    particleSystem.fps(maxframerate)
                    slowdown = false
                    mousedown = true

                    if (e.which == 3){ // Right mouse button!
                        url = dragged.node.data.url + dragged.node.data.nodeAtom; // dragged.node.data.Node;
                        if (dragged.node.data.url !== '') {
                            // If in focused window, open in same window. Otherwise, in a new one:
                            if (wearefocused) { 
                                document.location.href = url;
                            }else{
                                window.open(url);
                            }
                        }
                        selected = nearest = dragged = null;
                        return false
                    }

                    if (selected.node !== null){
                        // dragged.node.tempMass = 10000
                        dragged.node.fixed = true
                        // Display the extra info for this node? (Toggle)
                        if (dragged.node.data.label == detailnode){ 
                            detailnode = null
                        }else{
                            detailnode = dragged.node.data.label
                        }
                    }

                    return false
                });

                $(canvas).mousemove(function(e){
                    var old_nearest = nearest && nearest.node._id
                    var pos = $(this).offset();
                    var s = {x:e.pageX-pos.left, y:e.pageY-pos.top};

                    nearest = particleSystem.nearest(s);
                    if (!nearest) return

                    if (dragged !== null && dragged.node !== null){
                        var p = particleSystem.fromScreen(s)
                        dragged.node.p = {x:p.x, y:p.y}
                        // dragged.tempMass = 10000
                    }
                    // If this item had an infobox, remove it:
                    if (dragged !==  null && dragged.node.data.label == detailnode){detailnode = null}
                    return false
                });

                $(window).bind('mouseup',function(e){
                    if (dragged===null || dragged.node===undefined) return
                    if (ctrlpressed == true) {
                        dragged.node.fixed = true
                    } else {
                        dragged.node.fixed = false
                    }
                    dragged.node.tempMass = 100
                    dragged = null
                    selected = null
                    mousedown = false
                    return false
                });

            },
            // End mousehandling

            // This is where we decide what gets drawn! *** HERE ***
            redraw:function(){
                var energy = particleSystem.energy();
                if (energy.mean < 0.01 && slowdown == false && mousedown == false) { 
                    console.log('Sleeping due to low energy!');
                    framerate = particleSystem.fps();
                    particleSystem.fps(minframerate);
                    slowdown = true;
                }
                if (energy.mean > 0.1 && slowdown == true) {
                    console.log('Waking up!');
                    particleSystem.fps(framerate);
                    slowdown = false;
                }
                ctx.clearRect(0,0, canvas.width, canvas.height)
                // Create background image:
                if (typeof(background) === 'object') {
                        ctx.drawImage(background,0,0);   
                }

                // Restore stored locations:  
                if (loadedcoords == false && savedcoordsexist == true) {
                    nodesexist = false
                    particleSystem.eachNode(function(node, p){ nodesexist = true })
                    if (nodesexist) {
                        RestoreView(particleSystem)
                        loadedcoords = true
                    }
                }

                particleSystem.eachEdge(function(edge, pt1, pt2){
                    // edge: {source:Node, target:Node, length:#, data:{}}
                    // pt1:  {x:#, y:#}  source position in screen coords
                    // pt2:  {x:#, y:#}  target position in screen coords

                    // draw a line from pt1 to pt2
                    //ctx.strokeStyle = "rgba(20,20,20, 1)"
                    ctx.strokeStyle = edge.data.statuscolour

                    ctx.lineWidth = 1 + 4*edge.data.weight
                    ctx.lineWidth = edge.data.linewidth                    //1 + 4*edge.data.weight
                    ctx.setLineDash = 4 ///edge.data.linestyle                    //1 + 4*edge.data.weight
                    ctx.beginPath()
                    ctx.moveTo(pt1.x, pt1.y)
                    ctx.lineTo(pt2.x, pt2.y)
                    ctx.stroke()
                })

                // For each node:
                particleSystem.eachNode(function(node, pt){

                    // Node has the following attributes:
                    /*
                    "Node_1380798249_984679": {
                        "Node": "Node_1380798249_984679",
                        "nodeType": "Network",
                        "nodeTitel": "TITAAN",
                        "nodeSymbool": "CiscoNetworkSymbol1",
                        "nodeStatus": "Green",
						"nodeAtom" : "[Naam van atom zoals in Ampersand bekend]",
                        "View": "NWview",
                        "nodeimage": {
                            "0": "ciscon1.png"
                        }
                    }*/

                    var img = new Image();
                    // nodeimage[0] = filename, [1] = width, [2] = height
                    if (node.data !== null) {
                        if (typeof(node.data.nodeimage) === 'undefined') {alert('Missing image for type ' + node.data.nodeSymbool); return;}
                        img.src = 'images/' + node.data.nodeimage[0]
                        imgwidth = node.data.nodeimage[1]
                        imgheight = node.data.nodeimage[2]
                        ctx.drawImage(img,pt.x-(imgwidth/2),pt.y-(imgheight/2),imgwidth,imgheight); // src, dx, dy, dw, dh
                    }   
                    // Labels:
                    labelfillstyle = 'rgba(0,0,0,0.5)'
                    if (node.data.nodeTitel !== null){
						var label = node.data.nodeTitel  
					}else{
						var label = node.data.nodeAtom
					}
                    var w = ctx.measureText(label||"").width + 10
                    var h = 18

                    // Calculate label center?:
                    if (!(label||"").match(/^[ \t]*$/)){
                        pt.x = Math.floor(pt.x)
                        pt.y = Math.floor(pt.y) - 30
                    }else{
                        label = 'undef'
                    }

                    // First draw status in a larger size, overwrite with smaller label tag
                    // Defaults:
                    ctx.fillStyle = node.data.statuscolour
                    linewidth = 3;

                    ctx.fillRect(pt.x-w/2-linewidth, pt.y-h/2-linewidth, w+linewidth*2,h+linewidth*2)

                    ctx.fillStyle = labelfillstyle
                    ctx.fillRect(pt.x-w/2, pt.y-h/2, w,h)
                    // gfx.oval(pt.x-w/2, pt.y-h/2,w,h, {fill:ctx.fillStyle})

                    // draw the text
                    if (label){
                        ctx.font = "bold 8pt Calibri"//Arial"
                        ctx.textAlign = "center"
                        ctx.fillStyle = 'white'
                        ctx.fillText(label||"", pt.x, pt.y+4)
                    }

                    if (node.data.label == detailnode) {
                        displaynode = node
                        displaypt = pt
                    }

                })  // End eachnode function 1
               
/* 
                // Restore stored locations:  
                if (loadedcoords == false && savedcoordsexist == true) {
                    nodesexist = false
                    particleSystem.eachNode(function(node, p){ nodesexist = true })
                    if (nodesexist) {
                        RestoreView(particleSystem)
                        loadedcoords = true
                    }
                }
*/
                 
                  // Display info box:
                  if (detailnode !== null) {
                      pt = displaypt
                      pt.y = pt.y+70
                      ctx.fillStyle = 'black'

                      txt1 = 'Node / Type : ' + displaynode.data.Node + ' / ' + displaynode.data.nodeType
                      txt2 = ''
                      
                      txt3 = 'Status : ' + displaynode.data.nodeStatus

                      var l1 = ctx.measureText(txt1).width
                      var l2 = ctx.measureText(txt2).width
                      var l3 = ctx.measureText(txt3).width
                      var w = Math.max(l1,l2);
                      w = Math.max(w,l3);
                      w = w + 10
                      var h = 60
                      var margin = 10
                      // Rectangle background:
                      ctx.fillStyle = 'black'
                      ctx.fillRect(pt.x-(w+margin)/2, pt.y-(h+margin)/2, w+margin, h+margin)

                      // Text:
                      ctx.font = "bold 8pt Calibri"//Arial"
                      ctx.textAlign = "left"
                      ctx.fillStyle = 'white'
                      ctx.fillText(txt1,pt.x-w/2,pt.y-h/3)
                      ctx.fillText(txt2,pt.x-w/2,pt.y)
                      ctx.fillText(txt3,pt.x-w/2,pt.y+h/3)

                  }


            }, // End redraw function


            resize:function(){
                var w = $(window).width(),
                h = $(window).height();
                margin = 1;
                canvas.width = w-margin; canvas.height = h-margin // resize the canvas element to fill the screen
                particleSystem.screenSize(w,h) // inform the system so it can map coords for us
                that.redraw()
            }, // End resize function

        } // End var 'that'

        return that
    } // End deadsimplerenderer





    $(document).ready(function(){
        sys = arbor.ParticleSystem({repulsion:1,stiffness:1,friction:1,gravity:false}) // create the system 
        sys.renderer = DeadSimpleRenderer("#viewport") // our newly created renderer will have its .init() method called shortly by sys...
        UpdateContent();
    }) // end documentreadyfunction

    function UpdateContent()
    {
	    StartJSONRead($.getUrlVars('view'),$.getUrlVars('focus'));
    }
    
    function StartJSONRead(view,focus){
        $.getJSON("jsonread.php?view="+view+"&focus="+focus, function(data) {
            var nodes = data.nodes
            var imagelist = []
            $.each(nodes, function(name, info){
                info.label=name // 'name' is the key, we don't actually need this for nodes.
                if (typeof(info.nodeimage) !== 'undefined' && cachedimages == false) {
                    imagelist.push(info.nodeimage[0])
                }
            })
            sys.merge({nodes:nodes, edges:data.edges})
            setTimeout(function() { StartJSONRead(view,focus) }, 5000);

            // Load config settings
            config = data.config

            // If specific particlesystem parameters are defined, update them 1x:
            if (typeof (config.parsysparams) !== 'undefined') {
                if (updatedparams == false) {
                    repulsion = config.parsysparams['repulsion']
                    stiffness = config.parsysparams['stiffness']
                    friction  = config.parsysparams['friction']
                    gravity   = config.parsysparams['gravity']
                    paraobj = {}
                    paraobj['repulsion'] = repulsion;
                    paraobj['stiffness'] = stiffness;
                    paraobj['friction'] = friction;
                    paraobj['gravity'] = gravity;
                    sys.parameters(paraobj)
                    console.log("Changed particle system parameters to: " + repulsion + '/' + stiffness + '/' + friction + '/' + gravity)
                    updatedparams = true
                }
            }


            // Preload images so they don't flicker:
            if (cachedimages == false) {
                console.log("Preloading " + imagelist.length + " images");
                preloadImages(imagelist)
                cachedimages = true
            }
        })

    } // End StartJSONRead

    $(document).keydown(function(e){
        if(e.keyCode==17) {
            ctrlpressed = true
            // No more changing positions to fill up space
            sys.screenStep(0);

        }

    })
    $(document).keyup(function(e){
        if(e.keyCode==17) {
            ctrlpressed = false
        }
    })

    TestForSavedData();

})
(this.jQuery)
