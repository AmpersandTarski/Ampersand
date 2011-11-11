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

add check for empty atom values

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

cleanup post mechanism a bit, so we don't send the whole page on a post (only need to send the AmpersandRoot div)

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
    case 'editStart':
      $_SESSION['commandQueue'] = array();
      showCommandQueue();
      dbStartTransaction($dbName);
      return true;
    case 'editDatabase':
      processEditDatabase($command->dbCommand);
      return true;
    case 'editCommit':
      showCommandQueue();
      dbCommitTransaction($dbName);
      return false;
    case 'editRollback':
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
    error("Malformed database command, missing 'dbcommand'");
  
  if (!isset($dbCommand->dbCmd))
    error("Malformed database command, missing 'dbCmd'");

  switch ($dbCommand->dbCmd) {
    case 'insert':
      if (array_key_exists('relation', $dbCommand) && array_key_exists('isFlipped', $dbCommand) && array_key_exists('parentAtom', $dbCommand) && array_key_exists('childAtom', $dbCommand))
        editInsert($dbCommand->relation, $dbCommand->isFlipped, $dbCommand->parentAtom, $dbCommand->childAtom);
      else 
        error("Database command $dbCommand->dbCmd is missing parameters");
      break;
    case 'delete':
      if (array_key_exists('relation', $dbCommand) && array_key_exists('isFlipped', $dbCommand) && array_key_exists('parentAtom', $dbCommand) && array_key_exists('childAtom', $dbCommand))
        editDelete($dbCommand->relation, $dbCommand->isFlipped, $dbCommand->parentAtom, $dbCommand->childAtom);
      else {
        print_r($dbCommand);
        error("Database command $dbCommand->dbCmd is missing parameters");
      }
      break;
    default:
      error("Unknown database command '$dbCommand->dbCmd'");
  }
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

function editInsert($rel, $isFlipped, $parentAtom, $childAtom) {
	global $dbName;
	global $relationTables;
	global $idRelationTables;
  echo "editInsert($rel, $isFlipped, $parentAtom, $childAtom)";
  $src = $isFlipped ? $childAtom : $parentAtom;
  $tgt = $isFlipped ? $parentAtom : $childAtom;

  $table = $relationTables[$rel]['table'];
  $srcCol = $relationTables[$rel]['srcCol'];
  $tgtCol = $relationTables[$rel]['tgtCol'];
  DB_doquer($dbName, "INSERT INTO $table ($srcCol, $tgtCol) VALUES ('$src', '$tgt')");
  
  /*
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
	*/
}


// TODO use backquote for table names? 
// TODO check escaping for table names
function editDelete($rel, $isFlipped, $parentAtom, $childAtom) {
  global $dbName; 
  global $relationTables;
  echo "editDelete($rel, $isFlipped, $parentAtom, $childAtom)";
  $src = $isFlipped ? $childAtom : $parentAtom;
  $tgt = $isFlipped ? $parentAtom : $childAtom;
  
  $table = $relationTables[$rel]['table'];
  $srcCol = $relationTables[$rel]['srcCol'];
  $tgtCol = $relationTables[$rel]['tgtCol'];
  $query = 'DELETE FROM '.$table.' WHERE '.$srcCol.'=\''.$src.'\' AND '.$tgtCol.'=\''.$tgt.'\';';
  echo $query;
  DB_doquer($dbName, $query);
}

?>

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