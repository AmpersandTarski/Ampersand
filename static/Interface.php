<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "Interfaces.php"; // defines $dbName, $isDev, $relationTableInfo and $allInterfaceObjects
require "php/DatabaseUtils.php";

session_start();

/*
// todo:
 * 

tickets 138

todo: does Atom Atom in computeDbCommands also match descendants? (eg. .. ATOM .. atom .. ATOM ..)
todo: fix edit start. maybe even make start local
todo: modified atom values are not escaped
todo: don't delete if original is null
todo: maybe don't use column unique and not null, since these might be weaker than the multiplicities (sometimes a surjective relation will allow nulls, depending on other relations in the same table, although possibly the table prop generator is wrong and will contain non-null in that case)
todo: box shadow gebruiken?

todo: sqlRelPlugNames also returns list. Change to maybe?

insert goes wrong if we have [keyA, keyB, keyC] and insert (valA1,valB1) (valA2,valB2), since unique keyC will contain 2 nulls.
kind of a pathological case, since tuples for valA1 will most likely be inserted before any valA2 tuples.

field editable also if it has children in an editable relation

another problem is that if the interface leaves certain fields null, then multiple inserts (which are rare) will cause a sql error
rather than a rule failing.
 
If stable atom is not unique, but modified is, do we want an update? Could be useful, but is kind of vague.

make example with multiple relations, all in one table
check delete and insert on that


BUG: empty list has insert button to the left, or is this ok?

support editing on interface atom (also possible in old prototype)
what happens when we change it? does it even make sense? what if interface is not a relation and subinterfaces are neither?
what happens when we change an atom that has subinterfaces? explicitly update those subinterfaces? (if they have a (flip)relation)
maybe on modification, always also update subinterfaces that are relations. If the table width already causes the update, no harm is done, but
if table width does not cause update, unexpected behavior may occur. e.g. if rel1 is same table as parent relation, but rel2 isn't, then a [rel1: b, rel2:c] ~> aa [rel1:b, rel2:c] will cause (a,b)~>(aa,b), but not (a,c)~>(aa,c)

support make new stuff. (generate unique? or ask user? both are not that hard) 

update is delete+insert does not work, as delete removes all attributes in row, so add update edit op

check for double atom names

checking presence of the atom and replacing it can be combined in a single query

fix univalency and atomic container/ atomlist container. is a bit messy now.

maybe not insert template & stub when not editing or no relation present

use better way to access/update concept table

insert is not optimized at all: 
  rows could be grouped together
  insert into concept table may be executed repeatedly
  
insert atom into multiple concept tables?


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
      dbStartTransaction($dbName);
      return true;
    case 'editDatabase':
      processEditDatabase($command->dbCommand);
      return true;
    case 'editCommit':
      dbCommitTransaction($dbName);
      return false;
    case 'editRollback':
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
    case 'update':
      if (array_key_exists('relation', $dbCommand) && array_key_exists('isFlipped', $dbCommand) &&
          array_key_exists('parentAtom', $dbCommand) && array_key_exists('childAtom', $dbCommand) &&
          array_key_exists('parentOrChild', $dbCommand) && array_key_exists('originalAtom', $dbCommand))
        editUpdate($dbCommand->relation, $dbCommand->isFlipped, $dbCommand->parentAtom, $dbCommand->childAtom
                  ,$dbCommand->parentOrChild, $dbCommand->originalAtom);
      else 
        error("Database command $dbCommand->dbCmd is missing parameters");
      break;
    case 'delete':
      if (array_key_exists('relation', $dbCommand) && array_key_exists('isFlipped', $dbCommand) &&
          array_key_exists('parentAtom', $dbCommand) && array_key_exists('childAtom', $dbCommand))
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

function editUpdate($rel, $isFlipped, $parentAtom, $childAtom, $parentOrChild, $originalAtom) {
  global $dbName;
  global $relationTableInfo;
  global $conceptTableInfo;
  global $tableColumnInfo;
  
  echo "editUpdate($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom, $parentOrChild, $originalAtom).'<br/>'";
  //$src = $isFlipped ? $childAtom : $parentAtom;
  //$tgt = $isFlipped ? $parentAtom : $childAtom;
  
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  $parentCol = $isFlipped ? $tgtCol : $srcCol;
  $childCol =  $isFlipped ? $srcCol : $tgtCol;
  
  $modifiedCol = $parentOrChild == 'parent' ? $parentCol : $childCol;
  $modifiedAtom= $parentOrChild == 'parent' ? $parentAtom : $childAtom;
  $stableCol   = $parentOrChild == 'parent' ? $childCol : $parentCol;
  $stableAtom  = $parentOrChild == 'parent' ? $childAtom: $parentAtom;
  
  if ($tableColumnInfo[$table][$stableCol]['unique']) {
    $query = "UPDATE $table SET $modifiedCol='$modifiedAtom' WHERE $stableCol='$stableAtom'";
    echo "update query is $query";
    DB_doquer($dbName, $query);
  }
  else /* if ($tableColumnInfo[$table][$modifiedCol]['unique']) { // todo: is this ok? no, we'd also have to delete stableAtom originalAtom and check if modified atom even exists, otherwise we need an insert, not an update.
    $query = "UPDATE $table SET $stableCol='$stableAtom' WHERE $modifiedCol='$modifiedAtom'";
    echo "update query is $query";
    DB_doquer($dbName, $query);
  }
  else */ {
    $query = 'DELETE FROM '.$table.' WHERE '.$stableCol.'=\''.$stableAtom.'\' AND '.$modifiedCol.'=\''.$originalAtom.'\';';
    echo $query.'<br/>';
    DB_doquer($dbName, $query);
    $query = "INSERT INTO $table ($stableCol, $modifiedCol) VALUES ('$stableAtom', '$modifiedAtom')";
    echo $query.'<br/>';
    DB_doquer($dbName, $query);
  }
  // if the new atom is not in its concept table, we add it
  $childConcept = $isFlipped ? $relationTableInfo[$rel]['srcConcept'] : $relationTableInfo[$rel]['tgtConcept'];
  $parentConcept =  $isFlipped ? $relationTableInfo[$rel]['tgtConcept'] : $relationTableInfo[$rel]['srcConcept'];
  $modifiedConcept = $parentOrChild == 'parent' ? $parentConcept : $childConcept;
  
  $conceptTable = $conceptTableInfo[$modifiedConcept]['table'];
  $conceptColumn = $conceptTableInfo[$modifiedConcept]['col'];
  //echo "Checking existence of $childAtom : $childConcept in table $conceptTable, column $conceptColumn";
  $allConceptAtoms = firstCol(DB_doquer($dbName, "SELECT $conceptColumn FROM $conceptTable"));
  if (!in_array($modifiedAtom, $allConceptAtoms)) {
    //echo 'not present';
    DB_doquer($dbName, "INSERT INTO $conceptTable ($conceptColumn) VALUES ('$modifiedAtom')");
  } else {
    // echo 'already present';
  }
}


// TODO use backquote for table names? 
// TODO check escaping for table names
function editDelete($rel, $isFlipped, $parentAtom, $childAtom) {
  global $dbName; 
  global $relationTableInfo;
  echo "editDelete($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom).'<br/>'";
  $src = $isFlipped ? $childAtom : $parentAtom;
  $tgt = $isFlipped ? $parentAtom : $childAtom;
  
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  $query = 'DELETE FROM '.$table.' WHERE '.$srcCol.'=\''.$src.'\' AND '.$tgtCol.'=\''.$tgt.'\';';
  echo $query.'<br/>';
  DB_doquer($dbName, $query);
}

?>

<html>
<head>
<link href="css/Ampersand.css" rel="stylesheet" type="text/css"/>
<link href="css/Custom.css" rel="stylesheet" type="text/css"/>
<link href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" rel="stylesheet" type="text/css"/>
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script>
<script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script>
<script src="js/Ampersand.js"></script>
<script type="text/javascript">

function init() {
  initialize(getInterfacesMap());
}

<?php echo generateInterfaceMap($allInterfaceObjects); ?>

</script>
</head>
<body onload="init()">
<div id="Header"><div id="Decoration"></div></div>
<?php
echo '<div id="TopLevelInterfaces">';
echo topLevelInterfaceLinks($allInterfaceObjects);
echo '</div>';

if (!isset($_REQUEST['interface']) || !isset($_REQUEST['atom'])) {
  echo '<h3>Top-level interfaces</h3>';
  echo topLevelInterfaceLinks($allInterfaceObjects);
} else {
  
  echo '<div id="PhpLog" dev="'.($isDev?'true':'false').'">'; // We cannot put PhpLog inside AmpersandRoot since its editing attribute
  $isEditing = processCommands();                             // depends on result of processCommands. Hence, the duplicated dev attr.
  echo '</div>';
  
  $interface=$_REQUEST['interface'];
  $atom=$_REQUEST['atom'];
  
  // store the interface and atom as attrs of body and set editing to true or false
  echo '<div id=AmpersandRoot interface='.showHtmlAttrStr($interface).' atom='.showHtmlAttrStr($atom).
       ' editing="'.($isEditing?'true':'false').'" dev="'.($isDev?'true':'false').'">';
  // todo: maybe remember editing? (not an issue now, since during editing there is no navigation)
  
  echo '<button class="Button EditButton" onclick="startEditing()">Edit</button>';
  echo '<button class="Button SaveButton" onclick="commitEditing()">Save</button>';
  echo '<button class="Button CancelButton" onclick="cancelEditing()">Cancel</button>';
  echo generateAtomInterfaces($dbName, $allInterfaceObjects[$interface], $atom, true); 

  echo '</div>';
} ?>
</body>
</html>