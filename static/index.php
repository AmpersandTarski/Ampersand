<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "Generics.php"; 
// defines $dbName, $isDev, $relationTableInfo, $allInterfaceObjects, $allRulesSql, $invariantRuleNames, and $allRoles

require "php/DatabaseUtils.php";
require "php/Database.php";
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Strict//EN">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/> 
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
$roleNr = $_REQUEST['role']; // 0 (or not specified) means no role is selected
$roleName = $roleNr ? $allRoles[$roleNr-1]['name'] : '';

echo '<div id="TopLevelInterfaces">';
echo '<ul>';

// TODO: until there is more time to design a nice user interface, we put the role selector as a list item in the top-level interfaces list
echo '<select id=RoleSelector onchange="changeRole()">';
echo '<option value="0"'.($roleNr==0 ? ' selected=yes' : '').'>Algemeen</option>'; // selected if role==0 or role is not specified
for ($i=0; $i<count($allRoles); $i++) {
  $roleNm = $allRoles[$i]['name'];
  echo '<option value="'.($i+1).'"'.($roleNr==($i+1) ? ' selected=yes' : '').'>'.$roleNm.'</option>';
}
echo '</select>'; // the select is in front of the rest, so it floats to the right before the reset item does.

echo '<li id="LinkToMain"><a href="index.php'.($roleNr>0? '?role='.$roleNr : '').'"><span class=TextContent>Main</span></a></li>';
if ($isDev) { // with --dev on, we show the reset-database link in the menu bar
  echo '<li id="MenuBarReset"><a href="Installer.php"><span class=TextContent>Reset</span></a></li>';
}

echo topLevelInterfaceLinks($allInterfaceObjects);

echo '</ul>';
echo '</div>';

if (!isset($_REQUEST['interface']) || !isset($_REQUEST['atom'])) {
  echo '<ul id="Maintenance">';
  echo '<li id="Reset"><a href="Installer.php"><span class=TextContent>Reset database</span></a></li>';
  echo '</ul>';
  echo '<h3 id="CreateHeader"><span class=TextContent>Create</span></h3>';
  echo newAtomLinks($allInterfaceObjects);
  echo '<div id=SignalAndPhpLogs>';
  genSignalLogWindow($roleNr, $roleName);
  echo '</div>';
} else {
    
  $interface=$_REQUEST['interface'];
  $atom = $_REQUEST['atom'];
  $isNew = $atom==''; 
  // if the atom is '', this means that a new atom should be created. We create a unique new atom in a temporary transaction,
  // so we can generate the interface in the normal way (by querying the database). When the interface is done, the transaction
  // is rolled back. On save, the atom is added to the concept table again.
  // TODO: with multiple users, this mechanism may lead to non-unique new atom names, until we enocode a session number
  //       in the unique atom name. But since the atom names are based on microseconds, the chances of a problem are pretty slim.
  if ($isNew) {
    DB_doquer($dbName, 'START TRANSACTION');
    $atom = createNewAtom($allInterfaceObjects[$interface]['srcConcept']);
  }

  echo '<div id=AmpersandRoot interface='.showHtmlAttrStr($interface).' atom='.showHtmlAttrStr($atom).
       ' concept='.showHtmlAttrStr($allInterfaceObjects[$interface]['srcConcept']).
       ' editing='.($isNew?'true':'false').' isNew='.($isNew?'true':'false').
       ' dev="'.($isDev?'true':'false').'">';

  echo '<div id=DbCommandList class=LogWindow minimized=false><div class=MinMaxButton></div><div class=Title>Edit commands</div></div>';
  echo '<div id=IssueList class=LogWindow minimized=false><div class=MinMaxButton></div><div class=Title>Errors</div></div>';
  
  echo '<div id=SignalAndPhpLogs>';
  echo '<div id=PhpLog class=LogWindow minimized=false><div class=MinMaxButton></div><div class=Title>Php log </div></div>';
  genSignalLogWindow($roleNr, $roleName);
  echo '</div>';
  
  echo '<button class="Button EditButton" onclick="startEditing()">Edit</button>';
  echo '<button class="Button SaveButton" onclick="commitEditing()">Save</button>';
  echo '<button class="Button CancelButton" onclick="cancelEditing()">Cancel</button>';

  // we need an extra ScrollPane div because the log windows need to be outside scroll area but inside ampersand root
  // (since their css depends on the 'editing' attribute)
  echo '<div id=ScrollPane>';
  echo generateAtomInterfaces($dbName, $allInterfaceObjects[$interface], $atom, true); 
  echo '</div>';
  
  echo '</div>';
  echo '<div id=Rollback></div>'; // needs to be outside AmpersandRoot, so it's easy to address all interface elements not in the Rollback
  
  if ($isNew) {
    DB_doquer($dbName, 'ROLLBACK');
  }
} ?>
</body>
</html>

<?php 

function topLevelInterfaceLinks($interfaces) {
  global $roleNr;
  foreach($interfaces as $interface) {
    if ($interface['srcConcept']=='ONE')
      echo '<li interface="'.escapeHtmlAttrStr(escapeURI($interface['name']))
          .'"><a href="index.php?interface='.escapeHtmlAttrStr(escapeURI($interface['name'])).'&atom=1'.($roleNr>0? '&role='.$roleNr : '')
          .'"><span class=TextContent>'.htmlSpecialChars($interface['name']).'</span></a></li>';
  }
}

function newAtomLinks($interfaces) {
  global $roleNr;
  echo '<ul id=CreateList>';
  foreach($interfaces as $interface) {
    if ($interface['srcConcept']!='ONE')
      echo '<li interface="'.escapeHtmlAttrStr(escapeURI($interface['name']))
           .'"><a href="index.php?interface='.escapeHtmlAttrStr(escapeURI($interface['name']))
           .'&atom='.($roleNr>0? '&role='.$roleNr : '').'"><span class=TextContent>Create new '.htmlSpecialChars($interface['srcConcept'])
           .' ('.htmlSpecialChars($interface['name']).')</spin></a></li>';
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
    $codomainAtoms = array_filter(getCoDomainAtoms($db, $srcAtom, $interface['expressionSQL'])); // filter, in case table contains ($srcAtom, null)

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

function genSignalLogWindow($roleNr, $roleName) {
  if ($roleNr > 0) {
    echo "<div id=SignalLog class=LogWindow minimized=false><div class=MinMaxButton></div><div class=Title>Signals for $roleName</div>";
    checkRoleRules($roleNr);
    echo "</div>";
  }
}
?>