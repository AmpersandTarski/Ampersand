<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "Interfaces.php"; // defines $dbName, $relationTables and $allInterfaceObjects
require "php/DatabaseUtils.php";

session_start();

function showCommandQueue() {
  foreach ($_SESSION['commandQueue'] as $command) {
    switch($command['dbCmd']) {
      case 'addnew':
        if ($command['dest'] == 'src')
          echo $command['rel'].': add (NEW,'.$command['otheratom'].')<br/>';
        else
          echo $command['rel'].': add ('.$command['otheratom'].',NEW)<br/>';
        break;
      case 'delete':
        echo $command['rel'].': delete ('.$command['src'].','.$command['tgt'].')<br/>';
        break;
      case 'update':
        if ($command['dest'] == 'src')
          echo $command['rel'].': ('.$command['src'].','.$command['tgt'].') ~> ('.$command['newval'].','.$command['tgt'].')<br/>';
        else
          echo $command['rel'].': ('.$command['src'].','.$command['tgt'].') ~> ('.$command['src'].','.$command['newval'].')<br/>'; 
        break;
          
    }
  }
}
/*
bug:
sometimes statics are replaced when current versions are newer. (or maybe the same?)
// todo:

make example with multiple relations, all in one table
check delete and add on that

check for empty string input

// use POST for db updates, now commands are in the url, preventing refresh from working
// rename add to insert
// can we use this somewhere? $_SERVER['PHP_SELF']
// relation & concept with equal name cause name clash? table names are not case sensitive (or does quoting help?)

// handle double events when clicking on a button (add, delete, cancel, commit, etc.) while editing a text field (strangely enough the blur event arrives later)

Newly inserted Identifier atom goes wrong when we navigate to it (maybe related to absence in id[Thing]
*/

/*
Efficiency might be a problem after all. Solutions: no multiple edits, mimic the updates without accessing the database (tricky, and probably resulting in a far more primitive interface)

addnew add new tuple in relation. Check if id[concept(new)] contains new, if not, the concept is new and the table we edited was not its concept-list table, so add
delete put null in deleted target (so we need a dest here too). Check if id[concept(new)] contains new, if not, we are in the concept-list table and accidentally removed the concept, so add
update is combination of the above

Mabye when editing, there will be some nulls in columns that are used as concept list, so maybe we need to filter. Are there any other problems possible?

probably easy to support editing on I[Concept], V[ONE*Concept], and V[Concept,ONE] (maybe we don't need the latter though)
When editing I[Concept], adding is easy, but what about delete? First check all columns of all tables that contain Concept?



*/

function error($msg) {
  die("<h3 style=\"color: red\">Error: $msg</h3>");
}

function processCommands() {  
  $commandsJson =$_REQUEST['commands']; 
  if (isset($commandsJson)) {
    $commandArray = json_decode($commandsJson);
    if (!$commandArray) {
        error('Malformed commands');
    } else {
      $isEditing = false;

      foreach ($commandArray as $command)
        $isEditing = processCommand($command);
        
      return $isEditing;
    }
  }
}

function processCommand($command) {
  global $dbName;         // necessary, since these are declared in a different module 
  if (!isset($command->cmd))
    error("Malformed command, missing 'cmd'");
  
  switch ($command->cmd) {
    case 'editstart':
      $_SESSION['commandQueue'] = array();
      showCommandQueue();
      dbStartTransaction($dbName);
      return true;
    case 'editdatabase':
      processEditDatabase($command->dbcommand);
      return true;
    case 'editcommit':
      showCommandQueue();
      dbCommitTransaction($dbName);
      return false;
    case 'editrollback':
      showCommandQueue();
      $_SESSION['commandQueue']= array();
      dbRollbackTransaction($dbName);
      return false;
    default:
      error("Unknown command '$command->cmd'");
  }
}

function processEditDatabase($dbCommand) {
  if (!isset($dbCommand))
    error("Malformed database command, missing 'dbcomand'");
  
  if (!isset($dbCommand->dbcmd))
    error("Malformed database command, missing 'dbcmd'");

  switch ($dbCommand->dbcmd) {
    case 'addnew':
      if ($dbCommand->rel && $dbCommand->dest && $dbCommand->destConcept && $dbCommand->otheratom)
        editAddNew($dbCommand->rel, $dbCommand->dest, $dbCommand->destConcept, $dbCommand->otheratom);
      else 
        error("Database command $dbCommand->dbcmd is missing parameters");
      break;
    case 'add':
      if ($dbCommand->rel && $dbCommand->src && $dbCommand->tgt)
        editAdd($dbCommand->rel, $dbCommand->src, $dbCommand->tgt);
      else 
        error("Database command $dbCommand->dbcmd is missing parameters");
      break;
    case 'delete':
      if ($dbCommand->rel && $dbCommand->src && $dbCommand->tgt)
        editDelete($dbCommand->rel, $dbCommand->src, $dbCommand->tgt);
      else 
        error("Database command $dbCommand->dbcmd is missing parameters");
      break;
    default:
      error("Unknown database command '$dbCommand->dbcmd'");
  }
}

$newAtomPrefix = 'New';

function mkUniqueAtom($existingAtoms, $concept) {
  global $newAtomPrefix;
  if (!in_array($newAtomPrefix.' '.$concept, $existingAtoms))
    return $newAtomPrefix.' '.$concept;
  
  $newAtomNrs = array();
  foreach ($existingAtoms as $atom) {
    preg_match('/\A'.$newAtomPrefix.' '.$concept.' \((?P<number>[123456789]\d*)\)\z/', $atom, $matches); 
    // don't match nrs with leading 0's since we don't generate those
    $newAtomNrs[] = $matches['number'];
  }

  $newAtomNrs = array_unique(array_filter($newAtomNrs)); // filter out all the non-numbers and double numbers
  sort($newAtomNrs);
  print_r($newAtomNrs);
  foreach ($newAtomNrs as $i=>&$nr) {
    if ($nr != $i+1) // as soon as $newAtomNrs[i] != i+1, we arrived at a gap in the sorted number sequence and we can use i+1
      return $newAtomPrefix.' '.$concept.' ('.($i+1).')';
  }
  return $newAtomPrefix.' '.$concept.' ('.(count($newAtomNrs)+1).')';
}
    
function editAddNew($rel, $dest, $destConcept, $otherAtom) {
  global $dbName;         // necessary, since these are declared in a different module 
  global $idRelationTables;
  $conceptTable = $idRelationTables[$destConcept]['table'];
  $conceptColumn = $idRelationTables[$destConcept]['srcCol'];
  $existingAtoms = firstCol(DB_doquer($dbName, "SELECT $conceptColumn FROM $conceptTable"));
  
  $newAtom = mkUniqueAtom($existingAtoms, $destConcept);
  if ($dest=='src')
    editAdd($rel, $newAtom, $otherAtom);
  else
    editAdd($rel, $otherAtom, $newAtom);
  DB_doquer($dbName, "INSERT INTO $conceptTable ($conceptColumn) VALUES ('$newAtom')");
}
function editAdd($rel, $src, $tgt) {
  global $dbName;         // necessary, since these are declared in a different module 
  global $relationTables; //
  echo "editAdd($rel, $src, $tgt)";
  $table = $relationTables[$rel]['table'];
  $srcCol = $relationTables[$rel]['srcCol'];
  $tgtCol = $relationTables[$rel]['tgtCol'];
  DB_doquer($dbName, "INSERT INTO $table ($srcCol, $tgtCol) VALUES ('$src', '$tgt')");
}
// TODO use backquote for table names? 
// TODO check escaping for table names
function editDelete($rel, $src, $tgt) {
  global $dbName;         // necessary, since these are declared in a different module 
  global $relationTables; //
  echo "editDelete($rel, $src, $tgt)";
  $table = $relationTables[$rel]['table'];
  $srcCol = $relationTables[$rel]['srcCol'];
  $tgtCol = $relationTables[$rel]['tgtCol'];
  DB_doquer($dbName, 'DELETE FROM '.$table.' WHERE '.$srcCol.'=\''.$src.'\' AND '.$tgtCol.'=\''.$tgt.'\';');
}

function editUpdate($rel, $src, $tgt,$dest,$newVal) {
  echo "editUpdate($rel, $src, $tgt,$dest,$newVal)";    
}

// todo: get rid of these echoes by ending php here.
echo '<html>';
echo '<head>';
echo '<link href="css/Experimental.css" rel="stylesheet" type="text/css"/>';
echo '<link href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" rel="stylesheet" type="text/css"/>';
echo '<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script>';
echo '<script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script>';
echo '<script src="js/Experimental.js"></script>';
echo '<script type="text/javascript">';
echo 'function init() {';
echo '  initialize(getInterfacesMap());';
echo '}';
echo '';
echo generateInterfaceMap($allInterfaceObjects);
echo '</script>';
echo '</head>';

if (!isset($_REQUEST['interface']) || !isset($_REQUEST['atom'])) {
  echo '<body onload="init()">';
  echo '<h3>Top-level interfaces</h3>';
  echo topLevelInterfaceLinks($allInterfaceObjects);
  echo '</body>';
} else {

  $isEditing = processCommands();
  $interface=$_REQUEST['interface'];
  $atom=$_REQUEST['atom'];
  
  // store the interface and atom as attrs of body and set editing to true or false
  echo '<body onload="init()" interface='.showHtmlAttrStr($interface).' atom='.showHtmlAttrStr($atom).' editing="'.($isEditing?'true':'false').'">';
  // todo: maybe remember editing? (not an issue now, since during editing there is no navigation)
  
  echo '<h3>Interface \''.htmlSpecialChars($interface).'\' for atom \''.htmlSpecialChars($atom).'\'</h3>';
  echo '<button class="EditButton" onclick="startEditing()">Edit</button>';
  echo '<button class="SaveButton" onclick="commitEditing()">Save</button>';
  echo '<button class="CancelButton" onclick="cancelEditing()">Cancel</button>';
  echo generateInterface($dbName, $allInterfaceObjects[$interface], $atom); 

  echo '</body>';
}
echo '</html>';
?>
