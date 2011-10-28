// navigation

function initializeLinks(interfacesMap) {
  $(".Atom").map(function () {
    var concept = $(this).attr('concept');
    var atom = $(this).attr('atom');
    var interfaces = interfacesMap[concept];
    if (typeof(interfaces) != 'undefined') { // if there are no interfaces, don't change the pointer and don't add a click event
      $(this).css("cursor","pointer");
      $(this).css("color","blue"); // add an attr and use stylesheet for this
      $(this).click(function (event) {
        console.log('click menu');
        if (interfaces.length == 1)
          window.location.href = "Interfaces.php?interface="+interfaces[0]+"&atom="+atom;
        else
          mkInterfaceMenu(event, $(this), interfaces, atom);
      });
    }     
  });

}

function mkInterfaceMenu(event, $parent, interfaces, atom) {
  $('.InterfaceContextMenu').remove();
  var $menu = $('<div class=InterfaceContextMenu>');
  $parent.append($menu);
  $menu.offset({ top: event.pageY, left: event.pageX });

  for (i=0; i<interfaces.length; i++) {
    var url = "Interfaces.php?interface="+interfaces[i]+"&atom="+atom;
    var $item = $('<a class=InterfaceContextMenuItem hsref='+url+' interface='+
                interfaces[i]+'>'+interfaces[i]+'</a>');   

    $menu = $menu.append($item);
    
    addClickEvent($item,url);
  }
}

function addClickEvent($item, url) { // need a separate function here, to prevent dynamic scoping (see bug below)
  $item.click(function () {
    window.location.href = url;
    $('.InterfaceContextMenu').remove(); // so the menu is gone when we press back
    return false;
  });
}


// util

function mapInsert(map, key, value) {
  if (map[key])
    map[key].push(value);
  else
    map[key] = [value];
}










// javascript bug?

// putting functions with references to local variables in an object or array
// seems to result in some kind of dynamic scoping.

function test() {
  var fns = bug();
  fns[0]();
  fns[1]();
  fns[2]();
  for ( i=0; i<3; i++) {  // no var before i
    fns[i]();
  }
/* results:
i:3 j:2     both i and j have the value after bug has been executed
i:3 j:2     ''
i:3 j:2     ''
i:0 j:2     i takes its valu from the loop variable
i:1 j:2
i:2 j:2
*/
}

function bug() {
    //var k = 99;
    var fns = new Array();
    for ( i=0; i<3; i++) {  // no var before i
      var j = i;
      
      fns.push( function() {
        console.log('i:'+i +' j:'+j);
      });  
    }
    return fns;
}

/* Putting the function push in a separate function doesn't have this problem:
results for okay():
i:0 j:0
i:1 j:1
i:2 j:2
i:0 j:0
i:1 j:1
i:2 j:2
*/


function okay() {
    fns = new Array();
    for (i=0; i<3; i++) {
      var j = i;
      
      writeFn(fns,i,j);  
    }
    fns[0]();
    fns[1]();
    fns[2]();
    for (i=0; i<3; i++)
      fns[i]();
}

function writeFn(fns,i,j) {
  fns.push( function() {
    console.log('i:'+i +' j:'+j);
  });
}

