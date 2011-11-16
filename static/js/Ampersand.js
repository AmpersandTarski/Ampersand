var commandQueue = new Array();

function initialize(interfacesMap) {
  console.log('initialize');
  if($('#PhpLog').children().length==0) 
    $('#PhpLog').remove();
  if ($('#AmpersandRoot').attr('editing') == 'true') {  
    commandQueue = new Array();
    setEditHandlers();
    traceDbCommands(); // to initialize
  }
  else
    setNavigationHandlers(interfacesMap);
}

function queueCommands(commandArray) {
  jQuery.map(commandArray, function (command) {
    $('.CommandQueue').append(showDbCommand(command)+'<br>');
  });
  commandQueue = commandQueue.concat(commandArray);
}

function showRelation(relation, isFlipped) {
  return '<span style="font-family: Arial">'+relation+(isFlipped?'~':'')+'</span>';  
}

function showAtom(atom) {
  return atom ? atom : '<span style="color:red">EMPTY</span>';
}

function showDbCommand(dbCommand) {
  switch (dbCommand.dbCmd) {
    case 'update':
      var originalPair = '('+(dbCommand.parentOrChild == 'parent' ? dbCommand.originalAtom + ',' + dbCommand.childAtom 
                                                                  : dbCommand.parentAtom + ',' + dbCommand.originalAtom) + ')';
      var newPair = '('+showAtom(dbCommand.parentAtom)+','+showAtom(dbCommand.childAtom)+')';
      return 'Update in   '+ showRelation(dbCommand.relation,dbCommand.isFlipped) +': '+
                           (dbCommand.originalAtom =='' ? 'add ' : originalPair+' ~> ')+newPair;
    case 'delete':
      return 'Delete from '+showRelation(dbCommand.relation,dbCommand.isFlipped)+': ('+showAtom(dbCommand.parentAtom)+','+showAtom(dbCommand.childAtom)+')';
  }
  return 'Undefined command: '+dbCommand;
}

// update with '' as originalAtom is insert
function mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, parentOrChild, originalAtom) {
  return {dbCmd: 'update', relation:relation, isFlipped:relationIsFlipped, parentAtom:parentAtom, childAtom:childAtom,
                           parentOrChild:parentOrChild, originalAtom:originalAtom};
}

function mkDbCommandDelete(relation, relationIsFlipped, parentAtom, childAtom) {
  return {dbCmd: 'delete', relation:relation, isFlipped:relationIsFlipped, parentAtom:parentAtom, childAtom:childAtom};
}

function computeDbCommands() {
  dbCommands = new Array();
  $('.Atom .Atom').map(function () {
    $childAtom = $(this);
    if (getParentAtomRow($childAtom).attr('rowType')!='NewAtomTemplate') {
      var $atomListElt = getParentAtomList($childAtom);
      var relation = $atomListElt.attr('relation'); 
     
      if (relation) {
        var relationIsFlipped = $atomListElt.attr('relationIsFlipped') ? attrBoolValue($atomListElt.attr('relationIsFlipped')) : false;
        var $parentAtom = getParentAtom($childAtom);
        var parentAtom = $parentAtom.attr('atom');
        var childAtom = $childAtom.attr('atom');

        // parent deleted does not affect child
        // if parent is new, then there will only be new children which are handled below. no need to do anything for parent
        
        if( $parentAtom.attr('status') == 'modified') {
           if ($childAtom.attr('status') != 'new' && $childAtom.attr('status') != 'deleted' ) {
              var originalAtom = $parentAtom.attr('originalAtom');
              var unmodifiedChildAtom = $childAtom.attr('originalatom') ?  $childAtom.attr('originalAtom') : childAtom;
              // we want to delete/update the original tuple with the original child, not a modified one
              dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, unmodifiedChildAtom, 'parent', originalAtom));
            }
        }

        switch($childAtom.attr('status')) {
          case 'new':
            dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, 'child', ''));
            break;
          case 'deleted':
            console.log('parent '+$childAtom.attr('status'));
            var unmodifiedParentAtom = $parentAtom.attr('originalAtom') ?  $parentAtom.attr('originalAtom') : parentAtom;
            var unmodifiedChildAtom = $childAtom.attr('originalatom') ?  $childAtom.attr('originalAtom') : childAtom;
            dbCommands.push(mkDbCommandDelete(relation, relationIsFlipped, unmodifiedParentAtom, unmodifiedChildAtom));
            break;
          case 'modified':
            if ($parentAtom.attr('status') != 'new' && $childAtom.attr('status') != 'deleted' ) {
              var originalAtom = $childAtom.attr('originalAtom');
              // if parent is modified, the original tuple will already have been deleted (tree traversal handles parent first)
              // so we can update the tuple with the modified parent. Hence no special case for parent with status=modified
              dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, 'child', originalAtom));
              
            }
            break;
        }     
      }
    }
  });
  return dbCommands;
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

function traceCommand(dbCmd) {
  $('#CommandQueue').append('<div>'+showDbCommand(dbCmd)+'</div>');
}

function traceDbCommands() {
  $('#CommandQueue').remove();
  $('#AmpersandRoot').prepend('<div id=CommandQueue></div>');
  $('#CommandQueue').append('<div><b>Edit operations:</b></div>');
  computeDbCommands().map( function(dbCmd) {
    traceCommand(dbCmd);
  });
}

function startEditing() {
  sendCommands([{cmd: 'editStart'}]);
}

function getEmptyAtomsNotInTemplates() {
  $emptyAtomsNotInTemplates = $('.Atom[atom=""]').map( function() {
    if ($(this).parents().filter('.NewAtomTemplate').length)
      return null;
    else {
      return $(this);
    }
  });
  return $emptyAtomsNotInTemplates;
}

function commitEditing() {
  $emptyAtomsNotInTemplates = getEmptyAtomsNotInTemplates();
  
  if ($emptyAtomsNotInTemplates.length > 0) {
    alert('Please fill out all <new> atoms first.');
    return;
  }
  var dbCommands = computeDbCommands();
  var commands = new Array();
  
  for (var i=0; i<dbCommands.length; i++) {
    commands.push({cmd: 'editDatabase', dbCommand: dbCommands[i]});
  }
  commands.push({cmd: 'editCommit'});
  console.log(commands);
  sendCommands(commands);
}

function cancelEditing() {
  sendCommands([{cmd: 'editRollback'}]);
}

// navigation

function navigateTo(interface, atom) {
  window.location.href = "Interface.php?interface="+encodeURIComponent(interface)+"&atom="+encodeURIComponent(atom);     
}

//todo interfacesMap arg is annoying
function setNavigationHandlers(interfacesMap) {
  $(".AtomName").map(function () {
    $atomListElt = getParentAtomList($(this)); 
    $atom=getParentAtom($(this));
    concept =$atomListElt.attr('concept');
    var atom = $atom.attr('atom');
    var interfaces = interfacesMap[concept];
    if (typeof(interfaces) != 'undefined') { // if there are no interfaces for this concept, don't change the pointer and don't insert a click event
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
function setEditHandlers() {
  setEditHandlersBelow($('#AmpersandRoot'));
}

function setEditHandlersBelow($elt) {

  $elt.find('.AtomList').hover(function () {
    var $parentInterface = getParentAtomList($(this));
    
    $parentInterface.attr('hover', 'false'); // todo: move to if below?
    if ($(this).attr('relation'))
        $(this).attr('hover', 'true');
    }, function () {
    $parentInterface = getParentAtomList($(this));
    if ($parentInterface.attr('relation'))
        $parentInterface.attr('hover', 'true');
    $(this).attr('hover', 'false');
  });
  $elt.find('.AtomName').click(function(){
    var $atomListElt = getParentAtomList($(this));
    var relation = $atomListElt.attr('relation'); 
    if (relation) {
      startAtomEditing(getParentAtom($(this)));
    }
  });
  $elt.find('.DeleteStub').click(function() {
    var $atomElt = $(this).next().children().first(); // children is for AtomListElt

    if ($atomElt.attr('status')=='new')
      getParentAtomRow($(this)).remove(); // remove the row of the table containing delete stub and atom
    else {
      if ($atomElt.attr('status') == 'modified') // restore the original atom name on delete
        $atomElt.find('.AtomName').text($atomElt.attr('originalAtom'));
      $atomElt.attr('status','deleted');
      getParentAtomRow($(this)).attr('rowstatus','deleted'); // to make the entire row invisible
      $atomElt.find('.InterfaceList').remove(); // delete all interfaces below to prevent any updates on the children to be sent to the server
    }
    
    traceDbCommands();

  });  $elt.find('.InsertStub').click(function (event) {
    var $atomListElt = getParentAtomList($(this));
    
    $newAtomTemplate = $atomListElt.children().filter('[rowType=NewAtomTemplate]');
    
    $newAtomTableRow = $newAtomTemplate.clone();

    $newAtomTableRow.attr('rowType','Normal'); // remove the NewAtomTemplate class to make the new atom visible
    $newAtomTemplate.before( $newAtomTableRow ); 
    
    setEditHandlersBelow($newAtomTableRow); // add the necessary handlers to the new element
    // don't need to add navigation handlers, since page will be refreshed before navigating is allowed
    
    traceDbCommands();
  });
}

// Create a form that contains a text field, put it after $atom, and hide $atom.
function startAtomEditing($atom) {
  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  if ($atom.attr('status')=='deleted')
    return;
  
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
  
  $atomName.text(newAtom);
  $atomName.show();
  
  if (newAtom!=atom) {
    
    if ($atom.attr('status')!='new') {
      $atom.attr('status','modified');
      if (!$atom.attr('originalAtom')) // first time we edit this field, set originalAtom to the old value 
        $atom.attr('originalAtom',atom); 
    }

    traceDbCommands();
  }
}


// utils

function getParentAtom($elt) {
  return $elt.parents().filter('.Atom').first();
}
function getParentAtomList($elt) {
  return $elt.parents().filter('.AtomList').first();
}

function getParentAtomRow($elt) {
  return $elt.parents().filter('.AtomRow').first();
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