var navigationWarningEnabled = true;

function initialize() {
  initCreateNewMenu();
  initLogWindows();  // Cannot call this from the post callback in sendCommands, since the existing click events somehow
  initializeAtoms(); // cannot be unbound from there. Therefore, initialization is split in two functions: 
                     // initialize and initializeAtoms (the latter also being called from sendCommands).
  startRefreshTimer();
}

function initializeAtoms() {
  if ($('#AmpersandRoot').attr('editing') == 'true') {  
    setUniqueNamesForEmptyAtoms($('#AmpersandRoot'));
    setEditHandlers();
    traceDbCommands(); // to initialize command list
  }
  else
    setNavigationHandlers();
}

function resetSession() {
  $.ajax({ url: 'php/Database.php/php/Database.php?resetSession',
           cache: false,
           success: function(data){ window.location.reload(); }
  });
}

function resetDatabase() {
  $.ajax({ url: 'Installer.php',
           cache: false,
           success: function(data){
                      $resetPage = $('<div>').html(data);
                      if ($resetPage.find("#ResetSuccess").length > 0) {
                        alert('The database has been reset to its initial population.');
                        window.location.reload();
                      } else
                     // In case of an error, we simply navigate to the old Installer.php to show the error
                     // Of course there is a very small possibility that reset will succeed now, but that does not cause any problems
                        window.location.href = 'Installer.php'; 
                    },
           error: function(data){ alert('NETWORK ERROR\n\nThe database has not been reset.'); }
  });
}

/* A clone of the top-level atom is parked on #Rollback at edit start. On cancel, the atom and its navigation handlers are put back 
 * on #ScrollPane. This is a feasible solution since the interfaces will be of a manageable size */
function startEditing() {
  $('#Rollback').empty(); // in case we start twice for some reason
  if ($('#AmpersandRoot').attr('isNew')!='true') // for create new, we don't need to roll back (cancel is navigate back)
    $('#Rollback').append($('#ScrollPane > .Atom').clone(true, true)); /* (true,true) is needed to deep-copy edit handlers */
  $('#AmpersandRoot').attr('editing','true');
  $('#AmpersandRoot').attr('style',''); // dummy update to have Safari refresh css (it doesn't recognize non-standard attribute changes)
  clearNavigationHandlers();
  setEditHandlers();
  traceDbCommands(); // to initialize command list

  clearLogItems($('#ErrorLog')); // lists are cleared here and in cancelEditing, in case back button causes multiple start or cancel actions
  clearLogItems($('#PhpLog'));
}

function cancelEditing() {
  window.onbeforeunload = null; // disable the navigation warning (it is set in computeDbCommands)
  
  if ($('#AmpersandRoot').attr('isNew')=='true') {
    // When the creation of an atom is canceled, we simply navigate back. The atom was created in a temporary transaction,
    // so we don't need to delete anything.
    history.go(-1);
  } else {
    $('#ScrollPane > .Atom').remove();  
    $('#ScrollPane').append($('#Rollback > .Atom')); // this constitutes a move
    
    clearLogItems($('#ErrorLog')); // lists are cleared here and in startEditing, in case back button causes multiple start or cancel actions
    clearLogItems($('#PhpLog'));
    
    $('#AmpersandRoot').attr('editing','false');
    $('#AmpersandRoot').attr('style',''); // dummy update to have Safari refresh css (it doesn't recognize non-standard attribute changes)
  }
}

function commitEditing() {
  $editedAtom = getEnclosingAtom( $('#atomEditor') );
  if ($editedAtom.length > 0) // autocomplete is extremely slow in its cancel and somehow blurs after the cancel event is handled,
    stopAtomEditing($editedAtom); // so we check whether it was active and stop any editing here.
  
  if (getEmptyAtomsNotInTemplates().length > 0) {
    alert('Please fill out all <new> atoms first.');
    return;
  }
  if (getNonUniqueAtomLists().length > 0) {
    alert('Please resolve duplicate atom names.');
    return;
  }
  
  var dbCommands = computeDbCommands();
  window.onbeforeunload = null; // disable the navigation warning (it is set in computeDbCommands)

  if ($('#AmpersandRoot').attr('isNew')=='true') {
    // If we commit a newly created atom, we also need to add the atom to the concept table.
    var atom = $('#AmpersandRoot').attr('atom');
    var concept = $('#AmpersandRoot').attr('concept');
    dbCommands.unshift( mkDbCommandAddToConcept(atom,concept) ); // put addToConcept command in front
  }
  sendCommands(dbCommands);
}

function sendCommands(commandArray) {
  $.post('php/Database.php',  
  { commands: JSON.stringify(commandArray), role: getSelectedRole() },
  function(data) {
    $results = $(data);
    $errors = $(data).find('.Error');
    $logMessages = $(data).find('.LogMsg');
    $invariantViolations = $(data).find('#InvariantRuleResults > .AmpersandErr');
    $signals = $(data).find('#ProcessRuleResults > .AmpersandErr');
    setLogItems($('#SignalLog'), $signals);

    setLogItems($('#PhpLog'), $logMessages);
    
    if ($errors.length + $invariantViolations.length > 0) {
      setLogItems($('#ErrorLog'), $invariantViolations);
      addLogItems($('#ErrorLog'), $errors);
      $('#ErrorLog').attr('minimized','false'); // always maximize window
    } 
    else { // we could simply reload here, but this way the state of the log windows is not changed/
      getNoCache( window.location.href, function(data){
        $newPage = $('<div>').html(data); 
        // NOTE: need to be careful here not to put elements with id's in the DOM before removing the old ones

        // replace the root Atom with the new one
        $('#ScrollPane > .Atom').remove();
        $('#ScrollPane').append($newPage.find('#ScrollPane > .Atom'));

        // update the timestamp
        $('#AmpersandRoot').attr('timestamp', $newPage.find('#AmpersandRoot').attr('timestamp') );
        $('#AmpersandRoot').attr('editing','false');

        // we ignore the signals from $newPage, since we have just set them.
        
        initializeAtoms(); 
        $('#AmpersandRoot').attr('style',''); // dummy update to have Safari refresh css (it doesn't recognize non-standard attribute changes)
      });
    }
  });
}


function getEmptyAtomsNotInTemplates() {
  $emptyAtomsNotInTemplates = $('#AmpersandRoot .Atom[atom=""]').map( function() {
    if ($(this).parents().filter('[rowType=NewAtomTemplate]').length)
      return null;
    else 
      return $(this);
  });
  return $emptyAtomsNotInTemplates;
}

function getNonUniqueAtomLists() {
  $nonUniqueAtomLists =$('#AmpersandRoot .AtomList').map( function() { // called also on lists in templates, but that's not a big deal
    $atoms = $(this).find('>.AtomRow[rowType=Normal]>.AtomListElt>.Atom');
    for (var i=0; i<$atoms.length; i++)
      for (var j=i+1; j<$atoms.length; j++)
        if ($($atoms[i]).attr('atom') == $($atoms[j]).attr('atom'))
          return $(this); 
    return null;
  });
  return $nonUniqueAtomLists;
}

// Edit commands

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


function mkDbCommandAddToConcept(atom, concept) {
  return {dbCmd: 'addToConcept', atom:atom, concept:concept};
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
  $('.Atom .Atom').map(function () { // for every parent child pair of atoms (only immediate, so no <parent> .. <atom>  .. <child>)
    $childAtom = $(this);
    if ($childAtom.parents().filter('[rowType=NewAtomTemplate]').length == 0) {
      //log(getEnclosingAtom($childAtom).attr('atom') + '<-->' + $childAtom.attr('atom'));
      
      var $atomList = getEnclosingAtomList($childAtom);
      var relation = $atomList.attr('relation'); 
     
      if (relation) {
        var relationIsFlipped = $atomList.attr('relationIsFlipped') ? attrBoolValue($atomList.attr('relationIsFlipped')) : false;
        var $parentAtom = getEnclosingAtom($childAtom);
        var parentAtom = $parentAtom.attr('atom');
        var childAtom = $childAtom.attr('atom');

        // parent deleted does not affect child
        // if parent is new, then all updates will be handled at child
        
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
            var unmodifiedParentAtom = $parentAtom.attr('originalAtom') ?  $parentAtom.attr('originalAtom') : parentAtom;
            var unmodifiedChildAtom = $childAtom.attr('originalatom') ?  $childAtom.attr('originalAtom') : childAtom;
            dbCommands.push(mkDbCommandDelete(relation, relationIsFlipped, unmodifiedParentAtom, unmodifiedChildAtom));
            break;
          case 'modified':
            var originalAtom = $childAtom.attr('originalAtom');
            // - if parent is modified, the original tuple will already have been deleted (tree traversal handles parent first)
            // so we can update the tuple with the modified parent. Hence no special case for parent with status=modified
            // - if parent is new, original atom will be empty, since the atom was automatically inserted because of multiplicities
            // - parent cannot be deleted, since then its children would also be deleted
            dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, 'child', originalAtom));
            break;
        }     
      }
    }
  });
  
  if (dbCommands.length > 0 && navigationWarningEnabled) { 
    window.onbeforeunload = function() { return "The page has been modified, are you sure you wish to navigate away?"; };
  } else {
    window.onbeforeunload = null;
  }

  return dbCommands;
}

function traceDbCommands() {
  clearLogItems($('#EditLog'));
  var dbCmds = computeDbCommands();
  for (var i=0; i<dbCmds.length; i++)
    addLogItems($('#EditLog'), $('<div class=Command>'+showDbCommand(dbCmds[i])+'</div>'));
}



// Editing UI

function clearEditHandlers() {
  $('#AmpersandRoot .AtomList').unbind('mouseenter mouseleave');
  $('#AmpersandRoot .AtomName').unbind('click');
  $('#AmpersandRoot .DeleteStub').unbind('click');
  $('#AmpersandRoot .InsertStub').unbind('click');
}

function setEditHandlers() {
  setEditHandlersBelow($('#AmpersandRoot'));
}

function setEditHandlersBelow($elt) {

  //on hover over an AtomList, set the attribute hover to true for that AtomList, but not for any child AtomLists
  // (this is not possible with normal css hover pseudo elements)
  $elt.find('.AtomList').hover(function () { 
    // mouse enter handler
    
    var $atomList = getEnclosingAtomList($(this));
    $atomList.attr('hover', 'false');
    $(this).attr('hover', 'true');
  }, function () {
    // mouse exit handler
      
    $atomList = getEnclosingAtomList($(this));
    $atomList.attr('hover', 'true');
    $(this).attr('hover', 'false');
  });
  
  $elt.find('.AtomName').click(function(){
    var $atomList = getEnclosingAtomList($(this));
    var relationToParent = $atomList.attr('relation');
    
    var $atom = getEnclosingAtom($(this)); 
    
    // this code also allows editing if there is a child atom in an editable relation
    //var childrenWithRelation = $atom.find('>.InterfaceList>.Interface>.AtomList[relation]');
    if (relationToParent /* || childrenWithRelation.length != 0 */) {
      startAtomEditing($atom);
    }
  });
 
  $elt.find('.DeleteStub').click(function() {
    var $atom = $(this).next().children().first(); // children is for AtomListElt
    var $atomList = getEnclosingAtomList($(this));
    
    if ($atom.attr('status')=='new')
      getEnclosingAtomRow($(this)).remove(); // remove the row of the table containing delete stub and atom
    else {
      if ($atom.attr('status') == 'modified') { // restore the original atom name on delete
        $atom.attr('atom', $atom.attr('originalAtom'));
        $atom.find('.AtomName').text($atom.attr('originalAtom')); // text() automatically escapes text
      }
      $atom.attr('status','deleted');
      getEnclosingAtomRow($(this)).attr('rowStatus','deleted'); // to make the entire row invisible
      // we cannot use rowType for this, since we only change visibility, the rest of the style should be same as for Normal
      
      $atom.find('.InterfaceList').remove(); // delete all interfaces below to prevent any updates on the children to be sent to the server
    }
    
    updateNrOfAtoms($atomList);
    traceDbCommands();
  });
  
  $elt.find('.InsertStub').click(function (event) {
    var $atomList = getEnclosingAtomList($(this));
    
    $newAtomTemplate = $atomList.children().filter('[rowType=NewAtomTemplate]');
    
    $newAtomTableRow = $newAtomTemplate.clone();

    $newAtomTableRow.attr('rowType','Normal'); // remove the NewAtomTemplate class to make the new atom visible
    $newAtomTemplate.before( $newAtomTableRow ); 
        
    setEditHandlersBelow($newAtomTableRow); // add the necessary handlers to the new element
    // don't need to add navigation handlers, since page will be refreshed before navigating is allowed
    
    var $newAtom = $newAtomTableRow.find('>.AtomListElt>.Atom');
    setUniqueNamesForEmptyAtoms($newAtom); // set unique name if atom name is invisible (also for child atoms) 

    // if the atom name is visible, we start editing it.
    if ( $newAtom.find('>.AtomName').is(":visible") )
      startAtomEditing($newAtom);
    
    updateNrOfAtoms($atomList);
    traceDbCommands();
  });
}

function updateNrOfAtoms($atomList) {
  var $normalRows = $atomList.find('>.AtomRow[rowType="Normal"]');
  $atomList.attr('nrOfAtoms', $normalRows.length - $normalRows.filter('[rowStatus="deleted"]').length );
  // with --dev, rows are not removed but have their status set to deleted
}

// Create a form that contains a text field, put it after $atom, and hide $atom.
function startAtomEditing($atom) {
  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  if ($atom.attr('status')=='deleted')
    return;
      
  $textfield = $('<input type=text size=1 style="width:100%" value="'+$atomName.text()+'"/>');
  $form = $('<form id=atomEditor style="margin:0px"/>'); // we use a form to catch the Return key event
  $form.append($textfield);
  $atomName.after($form);

  // stop editing when the textfield loses focus
  
  $textfield.focusout(function (obj) {
    // jQuery bug (or really bad feature): a select in the autocomplete menu triggers a blur event on the textfield.
    // If we stopEditing on this blur, the select action is not registered.
    // As a workaround, we don't stopEditing if the menu is visible. In that case, stopEditing is called in the 
    // autocomplete close event handler below. A consequence of this approach is that whenever the autocomplete menu is
    // closed, atom editing is also ended. However, this is not a big deal and also seems to be the only solution, as
    // there is no way to detect whether the blur was a genuine blur, or caused by select.
    if ($('.ui-autocomplete:visible').length == 0)
      stopAtomEditing($atom);
    
    return true;
  });
  // and when the user presses the return key
  $form.submit(function () {
    stopAtomEditing($atom);
    return false; // this prevents the browser from actually submitting the form
  });

  initializeAutocomplete($textfield, $atom);
  $textfield.focus().select();
  $atomName.hide();

}

// take the old value from $atom and replace its atom attribute as well as its text
// contents with the new value from the text field. Then show $atom again and
// remove the text field.
function stopAtomEditing($atom) {
  var $atomList = getEnclosingAtomList($atom); // todo, make function
  var concept = $atomList.length ? $atomList.attr('concept') : $('#AmpersandRoot').attr('concept'); 

  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  
  var $form = $('#atomEditor');
  var newAtomText = $form.children().filter('input').attr('value');
  $form.remove();

  // todo if newAtom is keyed and has no atom, don't do anything
  var newAtom;
  if (conceptHasKey(concept)) { // if concept has a key, we map the key back onto an atom (todo: use autocomplete selection, so we don't need this lookup)
    newAtom = getAtomForKey(newAtomText, concept)
    
    if (!newAtom) { // If the entered key does not correspond to an atom, the edit operation is ignored.
      $atomName.attr('style',''); // Undo the hide() action from above. We don't use show, because that sets a style attribute on the div,
                                  // which overrides all stylesheets
      return;
    }
  } else {
    newAtom = newAtomText; // if concept has no key, then newAtom is simply atomText
  }
  $atom.attr('atom',newAtom);
  
  $atomName.text(newAtomText);
  $atomName.attr('style',''); // undo hide()
  
  if (newAtom!=atom) {
    
    if ($atom.attr('status')!='new') {
      
      if (!$atom.attr('originalAtom')) { // first time we edit this field, set originalAtom to the old value 
        $atom.attr('originalAtom',atom);
        $atom.attr('status','modified');
      } else                            // the atom was edited before
          if ($atom.attr('originalAtom')==newAtom)
            $atom.attr('status','unchanged'); // apparently, it was changed back to its original value
    }

    traceDbCommands();
  }
}

// For documentation, make sure to read docs.jquery.com/UI/Autocomplete and not docs.jquery.com/Plugins/autocomplete (which is incorrect)
function initializeAutocomplete($textfield, $atom) {
  var $atomList = getEnclosingAtomList($atom);
  var concept = $atomList.length ? $atomList.attr('concept') : $('#AmpersandRoot').attr('concept'); 
  // If there is no enclosing list, we are at the top-level atom and take the concept from AmpersandRoot 

  $textfield.autocomplete({ source: getAllConceptAtoms(concept)
                          , minLength: 0
                          , close: function(event, ui) { 
                            if (typeof event.originalEvent != 'undefined')
                              stopAtomEditing($atom);
                            // We only stop editing here if the close was caused by an explicit event.
                            // If the menu disappears because the user is typing (when the text does not match any autocomplete string) 
                            // the originalEvent is not defined and we don't stop editing.
                          }
                          , select: function(event, ui) { 
                              // Another jQuery problem: on a mouse click, the textfield has not been updated
                              // at this point, so stopAtomEditing does not register the selected value.
                              // As a workaround, we explicitly set the value to the menu selection.
                              // Strangely enough, there is no problem for the Return key event.
                              if (event.originalEvent && event.originalEvent.originalEvent &&
                                  event.originalEvent.originalEvent.type == "click")
                                $textfield.attr('value', ui.item.value);

                              stopAtomEditing($atom); 
                              return true;
                           }});
}


//Navigation

function navigateTo(interface, atom) {
  window.location.href = 'index.php?'+(typeof(interface) != 'undefined' &&  typeof(atom) != 'undefined' ?
                                            'interface='+encodeURIComponent(interface)+'&atom='+encodeURIComponent(atom) : '')
                                         +'&role='+getSelectedRole();
  // the check for undefined is necessary for when navigateTo is called on a role change from the Main page
}

function navigateToNew(interface, concept) {
  var atom = mkUniqueAtomName(concept);
  window.location.href = 'index.php?interface='+encodeURIComponent(interface)+'&atom='+encodeURIComponent(atom) + 
                         '&role='+getSelectedRole();       
}

function clearNavigationHandlers() {
  $('#AmpersandRoot .AtomName').unbind('click'); 
}

function setLogNavigationHandlers($logWindow) {
  $logWindow.find(".Pair > .PairAtom").map(function () {
    $pairAtom = $(this); 
    var concept =$pairAtom.attr('concept');
    var atom = $pairAtom.attr('atom');
    
    setAtomNavigationHandler( $pairAtom, atom, concept );
  });
}

function setNavigationHandlers() {
  $("#AmpersandRoot .AtomName").map(function () {
    $atomList = getEnclosingAtomList($(this)); 
    $atom=getEnclosingAtom($(this));
    var concept =$atomList.attr('concept');
    var atom = $atom.attr('atom');
    
    setAtomNavigationHandler( $(this), atom, concept );
  });
}

function setAtomNavigationHandler($elt, atom, concept) {
  var interfaces = getInterfacesMap()[concept];  // NOTE: getInterfacesMap is assumed to be declared 
  // (since js has no import mechanism and we don't want to pass variables around all the time, a more elegant solution is not possible)
  
  if (typeof(interfaces) != 'undefined' && interfaces.length > 0) { // if there are no interfaces for this concept, don't change the pointer and don't insert a click event
                                                                     // undefined means no interfaces are defined, length == 0 means no interfaces are visible for selected role
    $elt.click(function (event) {
      if (interfaces.length == 1)
        navigateTo(interfaces[0], atom);
      else
        mkInterfaceMenu(event, interfaces, atom);
    });
  }       
}

function mkInterfaceMenu(event, interfaces, atom) {
  $('#InterfaceContextMenu').remove();
  var $fullScreenMask = $("<div id=FullScreenMask/>");  /* the mask is for hiding the menu if we click anywhere outside it */
  var $menu = $('<div id=InterfaceContextMenu>');

  $fullScreenMask.click(function () {
    $menu.remove();
    $fullScreenMask.remove();
  });
  $('body').append($fullScreenMask);
  $fullScreenMask.append($menu);
 
  for (var i=0; i<interfaces.length; i++) {
    var $item = $('<div class=InterfaceContextMenuItem interface='+
                interfaces[i]+'>'+interfaces[i]+'</div>');   

    $menu = $menu.append($item);
    
    addClickEvent($item,interfaces[i],atom);
    // We need this separate function here to get a reference to i's value rather than the variable i.
    // (otherwise we encounter the 'infamous loop problem': all i's have the value of i after the loop)
  }
  
  // take into account menu size and page size, so menu is always visible, also at the edges
  $menu.offset({ left: Math.min(event.pageX, $(document).width() - $menu.outerWidth() - 2 )
               , top: Math.min(event.pageY, $(document).height() - $menu.outerHeight() - 2) });

}

function addClickEvent($item, interface, atom) { 
  $item.click(function () {
    navigateTo(interface, atom);
    $('#InterfaceContextMenu').remove(); // so the menu + mask are gone when we press back (maybe not necessary anymore, since we disable the cache)
    $('#FullScreenMask').remove();
    return false;
  });
}


//Keys

function conceptHasKey(concept) {
  return getEditableConceptInfo()[concept]['hasKey'];
}

// return the atom for key, or null if the key is not in the atomKeyMap
function getAtomForKey(key, concept) {
  var conceptAtomKeyMap = getEditableConceptInfo()[concept]['atomKeyMap'];
  
  var atom = null;
  for (var i=0; i<conceptAtomKeyMap.length; i++)
    if (conceptAtomKeyMap[i].key == key) {
      atom = conceptAtomKeyMap[i].atom;
      break;
    }
  
  return atom;
}


// Create-new dropdown menu

function initCreateNewMenu() {
  $('#MenuBarNew').click(function () {
    var $fullScreenMask = $("<div id=FullScreenMask/>");  /* the mask is for hiding the menu if we click anywhere outside it */
 
    $fullScreenMask.click(function () {
      $('#CreateMenu').hide();
      $fullScreenMask.remove();
    });
    $('#CreateMenu').show();
    $('body').append($fullScreenMask);
  });
  
  $('#CreateMenu>.MenuItem').map(function () {
    $(this).click(function () {
      navigateToNew($(this).attr('interface'), $(this).attr('concept'));
      $('#CreateMenu').hide(); // so the menu + mask are gone when we press back (maybe not necessary anymore, since we disable the cache)
      $fullScreenMask.remove();
    });
  });
}


// LogWindows

function initLogWindows() {
  $('.LogWindow>.Title').click(minimaximizeParentLogWindow);
  $('.LogWindow>.MinMaxButton').click(minimaximizeParentLogWindow);
  $('#SignalLog>.LogMsg').remove();  // don't show logs here
  if ($('#SignalLog>.LogItem').length>0)
    $('#SignalLog').attr('nonEmpty','true');
  setLogNavigationHandlers($(".LogWindow"));
}

function minimaximizeParentLogWindow(event) {
  $logWindow = $(this).parents().filter('.LogWindow');
  $logWindow.attr('minimized', $logWindow.attr('minimized')=='true' ? 'false' : 'true');
  return false;
}

function clearLogItems($logWindow) {
  $logWindow.find('.LogItem').remove();
  $logWindow.attr('nonEmpty','false');
}

// $logItems is a jQuery set of divs (LogItem class is added here)
function addLogItems($logWindow, $logItems) {
  $logItems.addClass('LogItem'); 
  $logWindow.append($logItems);
  if ($logItems.length > 0)
    $logWindow.attr('nonEmpty','true');
  setLogNavigationHandlers($logItems);
}

function setLogItems($logWindow, $logItems) {
  clearLogItems($logWindow);
  addLogItems($logWindow, $logItems);
}


// Roles

function changeRole() {
  var interface = $('#AmpersandRoot').attr('interface');
  var atom = $('#AmpersandRoot').attr('isNew')=='true' ? '' : $('#AmpersandRoot').attr('atom');
  // need to check for new and use atom='' to generate a new temporary atom, rather than navigate to the non-existing
  // current temporary atom.
  navigateTo(interface, atom); // navigate to takes the role from the updated selector
}


// Refresh timer

function startRefreshTimer() {
  var refreshInterval = $('#AmpersandRoot').attr('refresh');

  if (refreshInterval>0) {
    setRefreshTimer(refreshInterval*1000);
  }
}

function setRefreshTimer(ms) {  
  timer = setTimeout('checkDbUpdates()',ms);
}

function stopRefreshTimer() {
  clearTimeout(timer);
}

function checkDbUpdates() {
  if ($('#AmpersandRoot').attr('editing') == 'true') // no refresh during editing  
    startRefreshTimer();
  else {
    var currentTimestamp = $('#AmpersandRoot').attr('timestamp');
    
    // todo: is this okay without nocache?
    $.post("php/Database.php?getTimestamp",function(data){
      var dbTimestamp = $(data).attr('timestamp');
      var dbIsModified = dbTimestamp != currentTimestamp;
      log(currentTimestamp + ' vs '+dbTimestamp + ' ' + (dbIsModified ? 'modified' : 'not modified'));        
      
      // If the timestamp on the server is different, it must be newer, so we need a refresh
      if (dbIsModified) {
        getNoCache(window.location.href, function(data){
          $newPage = $('<div>').html(data);
          // NOTE: need to be careful here not to put elements with id's in the DOM before removing the old ones

          // replace the root Atom with the new one
          var $oldRootAtom = $('#ScrollPane > .Atom'); // save the old atom so we can do a diff below
          $('#ScrollPane > .Atom').remove();  
          $('#ScrollPane').append($newPage.find('#ScrollPane > .Atom'));

          // update the signals
          $signals = $(data).find('#SignalLog > .AmpersandErr');
          setLogItems($('#SignalLog'), $signals);

          // update the timestamp
          $('#AmpersandRoot').attr('timestamp', $newPage.find('#AmpersandRoot').attr('timestamp') );

          initializeAtoms();   
          startRefreshTimer();
          markDifference($('#ScrollPane > .Atom'), $oldRootAtom);
        });
      }
      else
        startRefreshTimer();
    });
  }
}

function markDifference($newAtom, $oldAtom) {
  $diffRoot = getDiffRoot($newAtom, $oldAtom);
  if ($diffRoot)
    animateDifference($diffRoot);
}

function getDiffRoot($newAtom, $oldAtom) {
  //log ($newAtom.attr('atom') + ' vs ' + $oldAtom.attr('atom'));
  if ($newAtom.find('>.AtomName').text() != $oldAtom.find('>.AtomName').text())
    return $newAtom.find('>.AtomName');
  else {
    var $newChildInterfaces = $newAtom.find('>.InterfaceList>.Interface'); 
    var $oldChildInterfaces = $oldAtom.find('>.InterfaceList>.Interface');
    
    for (var j=0; j<$newChildInterfaces.length; j++) { 
      // interfaces won't change, but we need to traverse them to be able to create markers at the AtomList level
        
      var $newChildAtoms = $newChildInterfaces.eq(j).find('>.AtomList>.AtomRow[rowType=Normal]>.AtomListElt>.Atom'); 
      var $oldChildAtoms = $oldChildInterfaces.eq(j).find('>.AtomList>.AtomRow[rowType=Normal]>.AtomListElt>.Atom'); 
      
      if ($newChildAtoms.length != $oldChildAtoms.length)
        return $newChildInterfaces.eq(j);
      else {
        for (var i=0; i<$newChildAtoms.length; i++) {
          //log ('child '+i);
          var $differentElt = getDiffRoot($newChildAtoms.eq(i), $oldChildAtoms.eq(i));
          if ($differentElt)
            return $differentElt;
        }
      }
    }
    return null;
  }
}

function animateTo($elt, color, k) {
  $elt.animate({ backgroundColor: color }, 500, null, k);
}

function animateDifference( $elt ) {
  var color = "red"; 
  animateTo($elt, color, function () {
  animateTo($elt, "transparent", function () {
  animateTo($elt, color, function () {
  animateTo($elt, "transparent", function () {
  animateTo($elt, color, function () {
  animateTo($elt, "transparent", function () {
  }); }); }); }); }); });
}


// Utils

function log(msg) {
  if (typeof console!='undefined') // only log when Firebug console is present
    console.log(msg);
}

// for errors that should only show up for a user with Firebug 
function logError(msg) {
  if (typeof console!='undefined')
    console.error(msg);
}

// precondition: concept appears on the page (and is therefore represented in getEditableConceptInfo)
function getAllConceptAtoms(concept) {
  var atomKeyMap = getEditableConceptInfo()[concept]["atomKeyMap"];
  var atoms = new Array();
  for (var i=0; i<atomKeyMap.length; i++) {
    atoms.push( atomKeyMap[i]["key"] ); // if the concept has no key, "key" contains the atom name.
  }
  return atoms;
}

function getEnclosingAtom($elt) {
  return $elt.parents().filter('.Atom').first();
}

function getEnclosingAtomList($elt) {
  return $elt.parents().filter('.AtomList').first();
}

function getEnclosingAtomRow($elt) {
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

function getSelectedRole() {
  return $('#RoleSelector').val();
}

// a replacement of get that uses ajax to disable any caching (as html meta tags do not always succeed in disabling the cache)
function getNoCache(url, successCallback) {
  $.ajax({ url: url,
           cache: false,
           success: successCallback
  });
}

// For all empty atoms below $elt that have a hidden atom name, replace the name attribute with a unique atom name.
// We cannot do this in php because it depends on the visibility of the name, which is not known at the php level.
function setUniqueNamesForEmptyAtoms($elt) {
  $elt.find('.Atom').andSelf().filter('.Atom[atom=""]').map( function() { // need find .Atom and filter. Atom because jquery doesn't have find on descendants + self
    if ($(this).parents().filter('[rowType=NewAtomTemplate]').length == 0 && $(this).find('>.AtomName').is(":hidden")) {
        var concept = getEnclosingAtomList($(this)).attr('concept')
        $(this).attr('atom', mkUniqueAtomName(concept));
        $(this).attr('status', 'new'); // by setting =new here instead of in php, visible atoms are not new, so if we cancel an edit operation or navigate away
                                       // without having changed any of the new strings, we won't get a warning.   
    }
  });
}

function mkUniqueAtomName(concept) {
  var mSecsSince1970 = ''+(new Date()).getTime();
  var seconds = mSecsSince1970.slice(0, mSecsSince1970.length-3);
  var microSeconds = mSecsSince1970.slice(mSecsSince1970.length-3, mSecsSince1970.length)+'000'; // php is a bit more accurate
  return 'atom',concept+'_'+seconds+'_'+microSeconds;
}
