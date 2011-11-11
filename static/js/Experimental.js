var commandQueue = new Array();

function queueCommands(commandArray) {
  jQuery.map(commandArray, function (command) {
    $('.CommandQueue').append(showCommand(command)+'<br>');
  });
  commandQueue = commandQueue.concat(commandArray);
}

function sendCommands(commandArray) {
  $.post(window.location.href,  
  { commands: JSON.stringify(commandArray) },
  function(data) {
    $('body').html(data);
    init(); 
    // This is the init() defined in Interface.php. The global reference is not ideal, but saves a lot of parameter passing.
    // note: javascripts are not replaced by the post action.
  });
}

function initialize(interfacesMap) {
  console.log('initialize');
  if ($('#AmpersandRoot').attr('editing') == 'true') {  
    commandQueue = new Array();
    setEditHandlers();
    $('#AmpersandRoot').prepend('<div class="CommandQueue">Edit History:<br/></div>');
  }
  else
    setNavigationHandlers(interfacesMap);
}

function startEditing() {
  sendCommands([{cmd: 'editstart'}]);
  /* code below is for dynamic editstart (without refreshing page from server)
  $('.Atom').unbind('click').css("cursor","default").css("color","black"); 
  $('#AmpersandRoot').attr('editing','True');
  setEditHandlers();
*/
}

function commitEditing() {
  queueCommands([{cmd: 'editcommit'}]);
  sendCommands(commandQueue);
}

function cancelEditing() {
  sendCommands([{cmd: 'editrollback'}]);
  /* code below is for dynamic editrollback (without refreshing page from server)
// maybe there's an easy way to prevent having to do setNavigationHandlers again (check for 'editing' in the click handler)
  $('.Atom').unbind('click');
  $('#AmpersandRoot').attr('editing','False');
  setNavigationHandlers(interfacesMap);
  */
}

// only interested in database commands
function showCommand(command) {
  switch (command.cmd) {
    case 'editDatabase':
      var dbCommand = command.dbCommand;
      switch (dbCommand.dbcmd) {
        case 'insertNew':
          return 'InsertNew '+dbCommand.rel+' '+(dbCommand.dest=='src' ? '('+dbCommand.otherAtom+',new)' : '(new,'+dbCommand.otherAtom+')');
        case 'insert':
          return 'Insert '+dbCommand.rel+' '+(dbCommand.dest=='src' ? '(>'+dbCommand.src+'<,'+dbCommand.tgt+')' : '('+dbCommand.src+',>'+dbCommand.tgt+'<)');
        case 'delete':
          return 'Delete '+dbCommand.rel+' ('+dbCommand.src+','+dbCommand.tgt+')';
      }
  }
  return 'Undefined command: '+command;
}

function insertNewCommand(relation, dest, otherAtom) {
  return {cmd: 'editDatabase', dbCommand: {dbcmd: 'insertNew', rel: relation, dest: dest, otherAtom: otherAtom}};
}

// dest specifies which of the atoms in the inserted tuple may be new (and in that case will need to be inserted to a concept table)
function insertCommand(relation, dest, src, tgt) {
  return {cmd: 'editDatabase', dbCommand: {dbcmd: 'insert', rel: relation, dest: dest, src: src, tgt: tgt}};
}

function deleteCommand(relation, src, tgt) {
  return {cmd: 'editDatabase', dbCommand: {dbcmd: 'delete', rel: relation, src: src, tgt: tgt}};
}


// navigation

// todo interfacesMap arg is annoying
function navigateTo(interface, atom) {
  window.location.href = "Interface.php?interface="+encodeURIComponent(interface)+"&atom="+encodeURIComponent(atom);     
}

function setNavigationHandlers(interfacesMap) {
  $(".AtomName").map(function () {
    $containerElt = getParentContainer($(this)); 
    $atom=getParentAtom($(this));
    concept =$containerElt.attr('concept');
    var atom = $atom.attr('atom');
    var interfaces = interfacesMap[concept];
    if (typeof(interfaces) != 'undefined') { // if there are no interfaces for this concept, don't change the pointer and don't insert a click event
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

function mkInterfaceMenu(event, $parentDiv, interfaces, atom) {
  $('.InterfaceContextMenu').remove();
  var $menu = $('<div class=InterfaceContextMenu>');
  $parentDiv.append($menu);
  $menu.offset({ top: event.pageY, left: event.pageX });

  for (var i=0; i<interfaces.length; i++) {
    var $item = $('<div class=InterfaceContextMenuItem interface='+
                interfaces[i]+'>'+interfaces[i]+'</div>');   

    $menu = $menu.append($item);
    
    addClickEvent($item,interfaces[i],atom);
    // We need this separate function here to get a reference to i's value rather than the variable i.
    // (otherwise we encounter the 'infamous loop problem': all i's have the value of i after the loop)
  }
}

function addClickEvent($item, interface, atom) { 
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
function setEditHandlers() {
  setEditHandlersBelow($('#AmpersandRoot'));
}

function setEditHandlersBelow($elt) {

  $elt.find('.Container').hover(function () {
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
  $elt.find('.AtomName').click(function(){
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    if (relation) {
      startAtomEditing(getParentAtom($(this)));
    }
  });
  $elt.find('.DeleteStub').click(function() {
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = attrBoolValue($containerElt.attr('relationIsFlipped'));
    var $atomElt = $(this).next().children().first();
    var atom =$atomElt.attr('atom');
    var srcAtom=getParentAtom($atomElt).attr('atom');
    if (relationIsFlipped) {
      //alert('Delete: ('+atom+','+srcAtom+ ') from ~'+relation);
      queueCommands([deleteCommand(relation,atom,srcAtom)]);
    } else {
        //alert('Delete: ('+srcAtom+','+atom+ ') from '+relation);
    	queueCommands([deleteCommand(relation,srcAtom,atom)]);
    }
    getParentTableRow($(this)).remove(); // remove the row of the table containing delete stub and atom
  });
  $elt.find('.InsertStub').click(function (event) {
    var $containerElt = getParentContainer($(this));
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = attrBoolValue($containerElt.attr('relationIsFlipped'));
     // todo: name otherAtom okay?
    var otherAtom=getParentAtom($(this)).attr('atom');
    
    $newAtomTemplate = $containerElt.children().children().filter('.NewAtomTemplate');
                    // <table>       <tbody>   <tr>
    
    $newAtomTableRow = $newAtomTemplate.clone();

    $newAtomTableRow.attr('class',''); // remove the NewAtomTemplate class to make the new atom visible
    $newAtomTemplate.before( $newAtomTableRow ); 
    
    setEditHandlersBelow($newAtomTableRow); // add the necessary handlers to the new element
    // don't need to add navigation handlers, since page will be refreshed before navigating is allowed
    
    if (relationIsFlipped) {
      //alert('Insert: (new,'+otherAtom+ ') to ~'+relation);
      queueCommands([insertNewCommand(relation,'src',otherAtom)]);
    }else {
      //alert('Insert: ('+otherAtom+',new) to '+relation);
      queueCommands([insertNewCommand(relation,'tgt',otherAtom)]);
    }
  });
}

// Create a form that contains a text field, put it after $atom, and hide $atom.
function startAtomEditing($atom) {
  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  $textfield = $('<input type=text size=1 style="width:100%" value="'+atom+'"/>');
  $form = $('<form id=atomEditor style="margin:0px"/>'); // we use a form to catch the Return key event
  $form.append($textfield);
  $atomName.after($form);
  $textfield.focus().select();
  $atomName.hide();

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
  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  
  var $form = $('#atomEditor');
  var newAtom = $form.children().filter('input').attr('value');
  $form.remove();

  $atom.attr('atom',newAtom);
  $atom.attr('newAtom','false');
  
  $atomName.text(newAtom);
  $atomName.show();
  if (newAtom!=atom) {
    var $containerElt = getParentContainer($atom);
    var relation = $containerElt.attr('relation'); 
    var relationIsFlipped = attrBoolValue($containerElt.attr('relationIsFlipped'));
    // todo: name srcAtom is not okay, depends on isFlipped
    var srcAtom=getParentAtom($atom).attr('atom');
if (relationIsFlipped) {
      //alert('Remove: ('+atom+','+srcAtom+ ') from ~'+relation+'\nInsert: ('+newAtom+','+srcAtom+ ') to ~'+relation);
      queueCommands([ deleteCommand(relation,atom,srcAtom)
                   , insertCommand(relation,'src',newAtom,srcAtom) ]);
    } else {
      //alert('Remove: ('+srcAtom+','+atom+ ') from '+relation+'\nInsert: ('+srcAtom+','+newAtom+ ') to '+relation);
      queueCommands([ deleteCommand(relation,srcAtom,atom)
                   , insertCommand(relation,'tgt',srcAtom,newAtom) ]);
    }
  }
}


// utils

function getParentAtom($elt) {
  return $elt.parents().filter('.Atom').first();
}
function getParentContainer($elt) {
  return $elt.parents().filter('.Container').first();
}

function getParentTableRow($elt) {
  return $elt.parents().filter('tr').first();
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