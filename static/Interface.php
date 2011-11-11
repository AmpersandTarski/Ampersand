<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "Interfaces.php"; // defines $dbName, $relationTables and $allInterfaceObjects
require "php/DatabaseUtils.php";

session_start();

function showCommandQueue() {
  foreach ($_SESSION['commandQueue'] as $command) {
    switch($command['dbCmd']) {
      case 'insertNew':
        if ($command['dest'] == 'src')
          echo $command['rel'].': insert (NEW,'.$command['otherAtom'].')<br/>';
        else
          echo $command['rel'].': insert ('.$command['otherAtom'].',NEW)<br/>';
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
// todo:

make example with multiple relations, all in one table
check delete and insert on that

use better way to access/update concept table

setNavigationHandlers now sets colors. We should set an attr, so the colors can be specified in css
// use POST for db updates, now commands are in the url, preventing refresh from working
// can we use this somewhere? $_SERVER['PHP_SELF']
// relation & concept with equal name cause name clash? table names are not case sensitive (or does quoting help?)

// handle double events when clicking on a button (insert, delete, cancel, commit, etc.) while editing a text field (strangely enough the blur event arrives later)

Newly inserted Identifier atom goes wrong when we navigate to it (maybe related to absence in id[Thing]

Later: css content can acces attributes, so we don't need to put the Insert new .. string in the generator. it can be put in the css

*/

/*

insertNew insert new tuple in relation. Check if id[concept(new)] contains new, if not, the concept is new and the table we edited was not its concept-list table, so insert
delete put null in deleted target (so we need a dest here too). Check if id[concept(new)] contains new, if not, we are in the concept-list table and accidentally removed the concept, so insert
update is combination of the above


probably easy to support editing on I[Concept], V[ONE*Concept], and V[Concept,ONE] (maybe we don't need the latter though)
When editing I[Concept], inserting is easy, but what about delete? First check all columns of all tables that contain Concept?



*/

function error($msg) {
  die("<h3 style=\"color: red\">Error: $msg</h3>");
}

function processCommands() {  
  $commandsJson =$_POST['commands']; 
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
  global $dbName; 
  if (!isset($command->cmd))
    error("Malformed command, missing 'cmd'");
  
  switch ($command->cmd) {
    case 'editstart':
      $_SESSION['commandQueue'] = array();
      showCommandQueue();
      dbStartTransaction($dbName);
      return true;
    case 'editDatabase':
      processEditDatabase($command->dbCommand);
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
    case 'insertinsert':
      if ($dbCommand->rel && $dbCommand->dest && $dbCommand->otherAtom)
        editInsertNew($dbCommand->rel, $dbCommand->dest, $dbCommand->otherAtom);
      else 
        error("Database command $dbCommand->dbcmd is missing parameters");
      break;
    case 'insert':
      if ($dbCommand->rel && $dbCommand->dest && $dbCommand->src && $dbCommand->tgt)
        editInsert($dbCommand->rel, $dbCommand->dest, $dbCommand->src, $dbCommand->tgt);
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
  foreach ($newAtomNrs as $i=>&$nr) {
    if ($nr != $i+1) // as soon as $newAtomNrs[i] != i+1, we arrived at a gap in the sorted number sequence and we can use i+1
      return $newAtomPrefix.' '.$concept.' ('.($i+1).')';
  }
  return $newAtomPrefix.' '.$concept.' ('.(count($newAtomNrs)+1).')';
}

function editInsertNew($rel, $dest, $otherAtom) {
  global $dbName; 
  global $relationTables;
  global $idRelationTables;
  echo "editInsertNew($rel, $dest, $otherAtom)";
  
  $destConcept = $dest=='src' ? $relationTables[$rel]['srcConcept'] :  $relationTables[$rel]['tgtConcept'];
  $conceptTable = $idRelationTables[$destConcept]['table'];
  $conceptColumn = $idRelationTables[$destConcept]['srcCol'];
  $existingAtoms = firstCol(DB_doquer($dbName, "SELECT $conceptColumn FROM $conceptTable"));
  $newAtom = mkUniqueAtom($existingAtoms, $destConcept);

  DB_doquer($dbName, "INSERT INTO $conceptTable ($conceptColumn) VALUES ('$newAtom')");
  
  if ($dest=='src')
    insertInRelation($rel, $newAtom, $otherAtom);
  else
    insertInRelation($rel, $otherAtom, $newAtom);
}

function editInsert($rel, $dest, $src, $tgt) {
	global $dbName;
	global $relationTables;
	global $idRelationTables;
  echo "editInsert($rel, $dest, $src, $tgt)";
  	
	insertInRelation($rel, $src, $tgt);
	
	$possiblyNewAtom = $dest=='src' ? $src : $tgt;
	
	$destConcept = $dest=='src' ? $relationTables[$rel]['srcConcept'] :  $relationTables[$rel]['tgtConcept'];
	$conceptTable = $idRelationTables[$destConcept]['table'];
	$conceptColumn = $idRelationTables[$destConcept]['srcCol'];
	$existingAtoms = firstCol(DB_doquer($dbName, "SELECT $conceptColumn FROM $conceptTable"));
	
	// if the destination atom was not in its concept table (either because it already existed, or the table
	// we inserted the tuple into contained the concept table), we insert it.
	if (!in_array($possiblyNewAtom, $existingAtoms )) {
		DB_doquer($dbName, "INSERT INTO $conceptTable ($conceptColumn) VALUES ('$possiblyNewAtom')");
	}
}

function insertInRelation($rel, $src, $tgt) {
  global $dbName; 
  global $relationTables;
  $table = $relationTables[$rel]['table'];
  $srcCol = $relationTables[$rel]['srcCol'];
  $tgtCol = $relationTables[$rel]['tgtCol'];
  DB_doquer($dbName, "INSERT INTO $table ($srcCol, $tgtCol) VALUES ('$src', '$tgt')");
}

// TODO use backquote for table names? 
// TODO check escaping for table names
function editDelete($rel, $src, $tgt) {
  global $dbName; 
  global $relationTables;
  echo "editDelete($rel, $src, $tgt)";
  $table = $relationTables[$rel]['table'];
  $srcCol = $relationTables[$rel]['srcCol'];
  $tgtCol = $relationTables[$rel]['tgtCol'];
  DB_doquer($dbName, 'DELETE FROM '.$table.' WHERE '.$srcCol.'=\''.$src.'\' AND '.$tgtCol.'=\''.$tgt.'\';');
}

function editUpdate($rel, $src, $tgt,$dest,$newVal) {
  echo "editUpdate($rel, $src, $tgt,$dest,$newVal)";    
}?>

<html>
<head>
<link href="css/Experimental.css" rel="stylesheet" type="text/css"/>
<link href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" rel="stylesheet" type="text/css"/>
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script>
<script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script>
<script src="js/Experimental.js"></script>
<script type="text/javascript">

function init() {
  initialize(getInterfacesMap());
}

<?php echo generateInterfaceMap($allInterfaceObjects); ?>

</script>
</head>
<?php
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
  echo '<body onload="init()">';
  echo '<div id=AmpersandRoot interface='.showHtmlAttrStr($interface).' atom='.showHtmlAttrStr($atom).' editing="'.($isEditing?'true':'false').'">';
  // todo: maybe remember editing? (not an issue now, since during editing there is no navigation)
  
  echo '<h3>Interface \''.htmlSpecialChars($interface).'\' for atom \''.htmlSpecialChars($atom).'\'</h3>';
  echo '<button class="EditButton" onclick="startEditing()">Edit</button>';
  echo '<button class="SaveButton" onclick="commitEditing()">Save</button>';
  echo '<button class="CancelButton" onclick="cancelEditing()">Cancel</button>';
  echo generateInterface($dbName, $allInterfaceObjects[$interface], $atom); 

  echo '</div></body>';
} ?>
</html>