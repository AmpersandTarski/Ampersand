function startEditing() {
  $('.Atom').unbind('click').css("cursor","default").css("color","black"); // undo coloring by initializeLinks. Not nice, css cannot be used now. TODO: fix this by using attr to signal presence of interfaces
  $('body').attr('editing','True');
  initializeEditButtons();

}

// todo interfacesMap arg is annoying
//      maybe there's an easy way to prevent having to do initializeLinks again (check for 'editing' in the click handler)
function stopEditing(interfacesMap) {
  $('.Atom').unbind('click');
  $('body').attr('editing','False');
  initializeLinks(interfacesMap);
}

// navigation

function navigateTo(interface, atom) {
  window.location.href = "Interface.php?interface="+encodeURIComponent(interface)+"&atom="+encodeURIComponent(atom);     
}

function initializeLinks(interfacesMap) {
  $(".Atom").map(function () {
    $containerElt = $(this).parents().filter(".Container"); 
    concept =$containerElt.attr('concept');
    var atom = $(this).attr('atom');
    var interfaces = interfacesMap[concept];
    if (typeof(interfaces) != 'undefined') { // if there are no interfaces for this concept, don't change the pointer and don't add a click event
      $(this).css("cursor","pointer");
      $(this).css("color","blue"); // todo add an attr and use stylesheet for this
      $(this).click(function (event) {     // todo: figure out return value for click handlers
        if (interfaces.length == 1)
          navigateTo(interfaces[0], atom);
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
    var $item = $('<div class=InterfaceContextMenuItem interface='+
                interfaces[i]+'>'+interfaces[i]+'</div>');   

    $menu = $menu.append($item);
    
    addClickEvent($item,interfaces[i],atom);
  }
}

function addClickEvent($item, interface, atom) { // need a separate function here, to prevent dynamic scoping (see bug below)
  $item.click(function () {
    navigateTo(interface, atom);
    $('.InterfaceContextMenu').remove(); // so the menu is gone when we press back
    return false;
  });
}

// Editing
// todo: editing -> editingHover oid
//       explain hover
//       clean up css, now container and AtomList are used next to each other.
function initializeEditButtons() {

  $('.Container').hover(function () {
    var $parentInterface = getParentContainer($(this));
    
    $parentInterface.attr('hover', 'False');
    if ($(this).attr('relation'))
        $(this).attr('hover', 'True');
    }, function () {
    $parentInterface = getParentContainer($(this));
    if ($parentInterface.attr('relation'))
        $parentInterface.attr('hover', 'True');
    $(this).attr('hover', 'False');
  });
  $('.Atom').click(function(){
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    if (relation) {
      var relationIsFlipped = $containerElt.attr('relationIsFlipped'); 
      var srcAtom =$containerElt.attr('srcAtom');
      startAtomEditing($(this));
    }
  });
  $('.DeleteStub').click(function() {
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = $containerElt.attr('relationIsFlipped'); 
    var srcAtom =$containerElt.attr('srcAtom');
    var $atomElt = $(this).next().children().first();
    var atom =$atomElt.attr('atom')
    if (relationIsFlipped)
      alert('Delete: ('+atom+','+srcAtom+ ') from ~'+relation);
    else 
      alert('Delete: ('+srcAtom+','+atom+ ') from '+relation);
  });
  $('.AddStub').click(function (event) {
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = $containerElt.attr('relationIsFlipped'); 
    var srcAtom =$containerElt.attr('srcAtom');
    var atom = 'new';
    if (relationIsFlipped)
      alert('Add: ('+atom+','+srcAtom+ ') to ~'+relation);
    else 
      alert('Add: ('+srcAtom+','+atom+ ') to '+relation);
  });
}

// Create a form that contains a text field, put it after $atom, and hide $atom.
function startAtomEditing($atom) {
  var atom = $atom.attr('atom');
  $textfield = $('<input type=text value="'+atom+'"/>');
  $form = $('<form id=atomEditor style="margin:0px"/>'); // we use a form to catch the Return key event
  $form.append($textfield);
  $atom.after($form);
  $($textfield).focus();
  $atom.hide();

  // stop editing when the textfield loses focus
  $textfield.blur(function () {
    stopAtomEditing($atom);
  });
  // and when the user presses the return key
  $form.submit(function () {
    stopAtomEditing($atom);
    return false; // this prevents the browser from actually submitting the form
  });

}

// take the old value from $atom and replace its atom attribute as well as its text
// contents with the new value from the text field. Then show $atom again and
// remove the text field.
function stopAtomEditing($atom) {
  var atom = $atom.attr('atom');
  $atom.attr('atom',newAtom);
  var $form = $('#atomEditor');
  var newAtom = $form.children().filter('input').attr('value');
  $form.remove();
  $atom.text(newAtom);
  $atom.show();
  if (newAtom!=atom) {
    var $containerElt = getParentContainer($atom);
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = $containerElt.attr('relationIsFlipped'); 
    var srcAtom =$containerElt.attr('srcAtom');
    if (relationIsFlipped)
      alert('Remove: ('+atom+','+srcAtom+ ') from ~'+relation+'\nAdd: ('+newAtom+','+srcAtom+ ') to ~'+relation);
    else 
      alert('Remove: ('+srcAtom+','+atom+ ') from '+relation+'\nAdd: ('+srcAtom+','+newAtom+ ') to '+relation);
  }
}

// todo: blur events come after delete and add events
//       make sure this does not cause problems when deleting
//       the edited atom
//       or when pressing cancel!


// utils

function getParentContainer($elt) {
  return $elt.parents().filter('.Container').first();
}

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

