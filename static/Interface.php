<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "Interfaces.php"; // defines $dbName, $isDev, $relationTableInfo and $allInterfaceObjects
require "php/DatabaseUtils.php";
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Strict//EN">
<html>
<head>
<link href="css/Ampersand.css" rel="stylesheet" type="text/css"/>
<link href="css/Custom.css" rel="stylesheet" type="text/css"/>

<link href="css/smoothness/jquery-ui-1.8.16.custom.css" rel="stylesheet" type="text/css"/>
<script src="js/jquery-1.6.2.min.js"></script>
<script src="js/jquery-ui-1.8.16.custom.min.js"></script>

<script src="js/Ampersand.js"></script>
<script type="text/javascript">

function init() {
  initialize();
}

<?php echo generateInterfaceMap($allInterfaceObjects); ?>

</script>
</head>
<body onload="init()">
<div id="Header"><div id="Logo"></div><div id="Decoration"></div></div>

<?php
echo '<div id="TopLevelInterfaces">';
echo topLevelInterfaceLinks($allInterfaceObjects);
echo '</div>';

if (!isset($_REQUEST['interface']) || !isset($_REQUEST['atom'])) {
  echo '<a href="Installer.php">Reset database</a>';
  echo '<h3>Interfaces</h3>';
  echo topLevelInterfaceLinks($allInterfaceObjects);
  echo '<h3>Create</h3>';
  echo newAtomLinks($allInterfaceObjects);
} else {
    
  $interface=$_REQUEST['interface'];
  $atom=$_REQUEST['atom'];
  $isNew = $atom==''; // if the atom is '', we create a unique new atom in the concept and set editing to true
  if ($isNew) {
    $atom = createNewAtom($allInterfaceObjects[$interface]['srcConcept']);
  }
  echo '<div id=AmpersandRoot interface='.showHtmlAttrStr($interface).' atom='.showHtmlAttrStr($atom).
       ' editing='.($isNew?'true':'false').' dev="'.($isDev?'true':'false').'">';

  echo '<div id=DbCommandList></div>';
  echo '<div id=PhpLog></div>';
  echo '<div id=IssueList></div>';
  echo '<button class="Button EditButton" onclick="startEditing()">Edit</button>';
  echo '<button class="Button SaveButton" onclick="commitEditing()">Save</button>';
  echo '<button class="Button CancelButton" onclick="cancelEditing()">Cancel</button>';
  echo generateAtomInterfaces($dbName, $allInterfaceObjects[$interface], $atom, true); 

  echo '</div>';
  echo '<div id=Rollback></div>';
  
} ?>
</body>
</html>

<?php 

function topLevelInterfaceLinks($interfaces) {
  echo '<ul>';
  foreach($interfaces as $interface) {
    if ($interface['srcConcept']=='ONE')
      echo '<li><a href="Interface.php?interface='.escapeHtmlAttrStr(escapeURI($interface['name'])).'&atom=1">'.htmlSpecialChars($interface['name']).'</a></li>';
  }
  echo '</ul>';
}

function newAtomLinks($interfaces) {
  echo '<ul>';
  foreach($interfaces as $interface) {
    if ($interface['srcConcept']!='ONE')
      echo '<li><a href="Interface.php?interface='.escapeHtmlAttrStr(escapeURI($interface['name'])).'&atom=">Create new '.htmlSpecialChars($interface['srcConcept']).' ('.htmlSpecialChars($interface['name']).')</a></li>';
  }
  echo '</ul>';
}

function generateInterfaceMap($interfaces) {
  echo 'function getInterfacesMap() {';
  echo '  var interfacesMap = new Array();';
  foreach($interfaces as $interface) {
    echo '  mapInsert(interfacesMap, '.showHtmlAttrStr($interface['srcConcept']).', '.showHtmlAttrStr($interface['name']).');';
  }
  echo '  return interfacesMap;';
  echo '}';
}

function generateInterface($db, $interface, $srcAtom) {
/*
 *  <Interface label='interface label'>
 *   <Label>interface label</Label>
 *   <AtomList concept=.. [relation=..  relationIsFlipped=..]>
 *     ..
 *     for each $tgtAtom in codomain of relation of $interface
 *     <AtomRow rowType=Normal>         <DeleteStub/> <AtomListElt> generateAtomInterfaces($interface, $tgtAtom) </AtomListElt> </AtomRow>
 *     ..
 *     
 *     <AtomRow rowType=NewAtomTemplate> <DeleteStub/> <AtomListElt> generateAtomInterfaces($interface, null) </AtomListElt>     </AtomRow>
 *     
 *     <AtomRow rowType=InsertAtomStub> <DeleteStub/> <InsertStub>Insert new .. </InsertStub>                                  </AtomRow>
 *   </AtomList>
 * </Interface> 
 */
  
  $html = "";
  emit($html, '<div class=Interface label='.showHtmlAttrStr($interface['name']).'>');
  emit($html, "<div class=Label>".htmlSpecialChars($interface['name']).'</div>');
  
  if ($srcAtom == null)
    $codomainAtoms = array (); // in case the table would contain (null, some atom)  
  else
    $codomainAtoms = array_filter(getCoDomainAtoms($db, $srcAtom, $interface['sqlQuery'])); // filter, in case table contains ($srcAtom, null)

  $codomainAtoms[] = null; // the null is presented as a NewAtomTemplate (which is cloned when inserting a new atom)
  
  $relationAttrs = $interface['relation']=='' ? '' : ' relation='.showHtmlAttrStr($interface['relation']).' relationIsFlipped='.showHtmlAttrStr(jsBool($interface['relationIsFlipped']));
  emit($html, '<div class="AtomList" concept='.showHtmlAttrStr($interface['tgtConcept']).$relationAttrs.'>');
  
  foreach($codomainAtoms as $tgtAtom) {
    emit($html, '<div class=AtomRow  rowType='.($tgtAtom==null?'NewAtomTemplate':'Normal').'><div class=DeleteStub>&nbsp;</div>'.
                  '<div class=AtomListElt>');
    emit($html, generateAtomInterfaces($db, $interface, $tgtAtom));
    emit($html,'</div></div>');  
  }
  
  emit($html, '<div class=AtomRow rowType=InsertAtomRow><div class=DeleteStub>&nbsp;</div>'.
                '<div class=InsertStub>Insert new '.htmlSpecialChars($interface['tgtConcept']).'</div></div>');
  
  emit($html, '</div></div>'); // close .AtomList and .Interface
  return $html;
}

function generateAtomInterfaces($db, $interface, $atom, $isTopLevelInterface=false) {
/* if $interface is a top-level interface, we only generate for $interface itself
 * otherwise, we generate for its subinterfaces 
 * 
 *  <Atom atom='atom name'>
 *   <AtomName>atom name</AtomName>
 *   <InterfaceList>
 *     ..
 *     for each subInterface in $interface: generateInterface($interface, $atom)        (or $interface, if $isTopLevelInterface)
 *     ..
 *   </InterfaceList>
 * </Atom>
 * 
 * if $atom is null, we are presenting a template
 */
  $html = "";
  $interfaces = $isTopLevelInterface ? array ($interface) : $interface['subInterfaces'];


  $nrOfInterfaces = count(getTopLevelInterfacesForConcept($interface['tgtConcept']));
  $hasInterfaces = $nrOfInterfaces == 0 ? '' : ' hasInterface=' . ($nrOfInterfaces == 1 ? 'single' : 'multiple');
  
  emit($html, '<div class=Atom atom='.showHtmlAttrStr($atom).$hasInterfaces.' status='.($atom?'unchanged':'new').' atomic='.jsBool(count($interfaces)==0).'>');
  // can be hidden with css if necessary (old prototype did not show it)
    
  emit($html, "<div class=AtomName>".htmlSpecialChars($atom).'</div>');
  if (count($interfaces) > 0) {
    emit($html, '<div class=InterfaceList>');
    foreach($interfaces as $interface) {
      emit($html, generateInterface($db, $interface, $atom));
    }
    emit($html, '</div>'); // div class=InterfaceList
  }
  emit($html, '</div>'); // div class=Atom
  return $html;
}
?>