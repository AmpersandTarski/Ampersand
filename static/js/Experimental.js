function startEditing() {
  sendCommands([{cmd: 'editstart'}]);
  /* code below is for dynamic editstart (without refreshing page from server)
  $('.Atom').unbind('click').css("cursor","default").css("color","black"); 
  $('body').attr('editing','True');
  initializeEditButtons();
*/
}

function commitEditing() {
  sendCommands([{cmd: 'editcommit'}]);
}

function cancelEditing() {
  sendCommands([{cmd: 'editrollback'}]);
  /* code below is for dynamic editrollback (without refreshing page from server)
// maybe there's an easy way to prevent having to do initializeLinks again (check for 'editing' in the click handler)
  $('.Atom').unbind('click');
  $('body').attr('editing','False');
  initializeLinks(interfacesMap);
  */
}

function addNewCommand(relation, destination, otherAtom) {
  return {cmd: 'editdatabase', dbcommand: {dbcmd: 'addnew', rel: relation, dest: destination, otheratom: otherAtom}};
}

function addCommand(relation, src, tgt) {
  return {cmd: 'editdatabase', dbcommand: {dbcmd: 'add', rel: relation, src: src, tgt: tgt}};
}

function deleteCommand(relation, src, tgt) {
  return {cmd: 'editdatabase', dbcommand: {dbcmd: 'delete', rel: relation, src: src, tgt: tgt}};
}


function sendCommands(commandArray) {
  window.location.href = 'Interface.php?interface='+encodeURIComponent($('body').attr('interface'))+'&atom='+encodeURIComponent($('body').attr('atom'))+
                         '&'+'commands='+encodeURIComponent(JSON.stringify(commandArray));
}

// navigation

// todo interfacesMap arg is annoying
function navigateTo(interface, atom) {
  window.location.href = "Interface.php?interface="+encodeURIComponent(interface)+"&atom="+encodeURIComponent(atom);     
}

function initialize(interfacesMap) {
  console.log('initialize');
  if ($('body').attr('editing') == 'true')
    initializeEditButtons();  
  else
    initializeLinks(interfacesMap);
}

// undo coloring by initializeLinks. Not nice, css cannot be used now. TODO: fix this by using attr to signal presence of interfaces
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

  for (var i=0; i<interfaces.length; i++) {
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
    
    $parentInterface.attr('hover', 'false'); // todo: move to if below?
    if ($(this).attr('relation'))
        $(this).attr('hover', 'true');
    }, function () {
    $parentInterface = getParentContainer($(this));
    if ($parentInterface.attr('relation'))
        $parentInterface.attr('hover', 'true');
    $(this).attr('hover', 'false');
  });
  $('.Atom').click(function(){
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    if (relation) {
      startAtomEditing($(this));
    }
  });
  $('.DeleteStub').click(function() {
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = $containerElt.attr('relationIsFlipped'); 
    var srcAtom =$containerElt.attr('srcAtom'); // todo: name srcAtom is not okay, depends on isFlipped
    var $atomElt = $(this).next().children().first();
    var atom =$atomElt.attr('atom');
    if (relationIsFlipped) {
      //alert('Delete: ('+atom+','+srcAtom+ ') from ~'+relation);
      sendCommands([deleteCommand(relation,atom,srcAtom)]);
    } else {
        //alert('Delete: ('+srcAtom+','+atom+ ') from '+relation);
    	sendCommands([deleteCommand(relation,srcAtom,atom)]);
    }
  });
  $('.AddStub').click(function (event) {
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = attrBoolValue($containerElt.attr('relationIsFlipped'));
    alert(relationIsFlipped);
    var otherAtom =$containerElt.attr('srcAtom'); // todo: name otherAtom okay?
    if (relationIsFlipped) {
      alert('Add: (new,'+otherAtom+ ') to ~'+relation);
      sendCommands([addNewCommand(relation,'src',otherAtom)]);
    }else {
      alert('Add: ('+otherAtom+',new) to '+relation);
      sendCommands([addNewCommand(relation,'tgt',otherAtom)]);
    }
  });
}

// Create a form that contains a text field, put it after $atom, and hide $atom.
function startAtomEditing($atom) {
  var atom = $atom.attr('atom');
  $textfield = $('<input type=text value="'+atom+'"/>');
  $form = $('<form id=atomEditor style="margin:0px"/>'); // we use a form to catch the Return key event
  $form.append($textfield);
  $atom.after($form);
  $textfield.focus().select();
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
    var srcAtom =$containerElt.attr('srcAtom'); // todo: name srcAtom is not okay, depends on isFlipped
    if (relationIsFlipped) {
      //alert('Remove: ('+atom+','+srcAtom+ ') from ~'+relation+'\nAdd: ('+newAtom+','+srcAtom+ ') to ~'+relation);
      sendCommands([ deleteCommand(relation,atom,srcAtom)
                   , addCommand(relation,newAtom,srcAtom) ]);
    } else {
      //alert('Remove: ('+srcAtom+','+atom+ ') from '+relation+'\nAdd: ('+srcAtom+','+newAtom+ ') to '+relation);
      sendCommands([ deleteCommand(relation,srcAtom,atom)
                   , addCommand(relation,srcAtom,newAtom) ]);
    }
  }
}


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

function attrBoolValue(attrStr) {
  return attrStr.toLowerCase()=="true" ? true : false;
}








// javascript bug?
// putting functions with references to local variables in an object or array
// seems to result in some kind of dynamic scoping.

//after figuring this out, update code for mkInterfaceMenu

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

