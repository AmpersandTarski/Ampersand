<?php
error_reporting(E_ALL);
ini_set("display_errors", 1);

require __DIR__.'/../Generics.php'; 
require_once __DIR__.'/DatabaseUtils.php';
require 'loadplugins.php';

initSession();

// This module handles four requests: 
//
//     Database.php?resetSession
//     Database.php?getTimestamp
//     Database.php?testRule=..
//     Database.php?commands=..

$selectedRoleNr = isset($_REQUEST['role']) ? $_REQUEST['role'] : -1; // role=-1 (or not specified) means no role is selected

if (isset($_REQUEST['resetSession']) ) {
  resetSession();
} else if (isset($_REQUEST['getTimestamp']) ) {
  timestampHtml();
} else if (isset($_REQUEST['testRule']) ) {
  testRule($_REQUEST['testRule']);
} else if (isset($_REQUEST['ip']) ) {
  echo $_SERVER['SERVER_ADDR'];
} else if (isset($_REQUEST['commands']) ) {
  echo '<div id="UpdateResults">';

  dbStartTransaction();
  emitLog('BEGIN');

  processCommands(); // update database according to edit commands

// Process processRules first, because the ExecEngine may execute code while processing this stuff.
  echo '<div id="ProcessRuleResults">';
  checkRoleRules($selectedRoleNr);
  echo '</div>';
  
  echo '<div id="InvariantRuleResults">';
  $invariantRulesHold = checkInvariantRules();
  echo '</div>';
  
  if ($invariantRulesHold) {
    setTimeStamp();
    emitLog('COMMIT');
    dbCommitTransaction();
  } else {
    emitLog('ROLLBACK');
    dbRollbackTransaction();
  }
  echo '</div>';
}

function processCommands() {  
  $commandsJson =$_POST['commands']; 
  if (isset($commandsJson)) {
    $commandArray = json_decode($commandsJson);
          
    foreach ($commandArray as $command)
      processCommand($command);
       
  }
}

function processCommand($command) {
  if (!isset($command->dbCmd))
    error("Malformed command, missing 'dbCmd'");

  switch ($command->dbCmd) {
    case 'addToConcept':
      if (array_key_exists('atom', $command) && array_key_exists('concept', $command))
        editAddToConcept($command->atom, $command->concept);
      else 
        error("Command $command->dbCmd is missing parameters");
      break;
    case 'update':
      if (array_key_exists('relation', $command) && array_key_exists('isFlipped', $command) &&
          array_key_exists('parentAtom', $command) && array_key_exists('childAtom', $command) &&
          array_key_exists('parentOrChild', $command) && array_key_exists('originalAtom', $command))
        editUpdate($command->relation, $command->isFlipped, $command->parentAtom, $command->childAtom
                  ,$command->parentOrChild, $command->originalAtom);
      else 
        error("Command $command->dbCmd is missing parameters");
      break;
    case 'delete':
      if (array_key_exists('relation', $command) && array_key_exists('isFlipped', $command) &&
          array_key_exists('parentAtom', $command) && array_key_exists('childAtom', $command))
        editDelete($command->relation, $command->isFlipped, $command->parentAtom, $command->childAtom);
      else {
        error("Command $command->dbCmd is missing parameters");
      }
      break;
    default:
      error("Unknown command '$command->dbCmd'");
  }
}

function editAddToConcept($atom, $concept) {
  emitLog("editAddToConcept($atom, $concept)");
  addAtomToConcept($atom, $concept);  
}

// NOTE: if $originalAtom == '', editUpdate means insert
function editUpdate($rel, $isFlipped, $parentAtom, $childAtom, $parentOrChild, $originalAtom) {
  global $relationTableInfo;
  global $tableColumnInfo;
  
  emitLog("editUpdate($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom, $parentOrChild, $originalAtom)");
/* There seems to be a bug in 'editUpdate', nl. when a $relation occurs multiple times as KEY in the relationTableInfo (which we have seen happening when you overload an (Ampersand) relation (name). The following code may be used to find the right entry in the relationTableInfo, but that is not used by 'editUpdate'.
  // check if $relation appears in $relationTableInfo
  if (array_key_exists($relation, $relationTableInfo))
  { foreach($relationTableInfo as $key => $arr)
  	if($key == $relation)
  	{ if($arr['srcConcept'] == $srcConcept && $arr['tgtConcept'] == $tgtConcept)
		  { $table = $arr['table'];
        $srcCol = $arr['srcCol'];
			  $tgtCol = $arr['tgtCol'];
			  echo "<br>[FOUND: table=$table, srcCol=$srcCol, tgtCol=$tgtCol]";
	    }
	  }
  } else
  { echo "ERROR: Relation $relation does not exist (in table info)";
  }
*/
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  $parentCol = $isFlipped ? $tgtCol : $srcCol;
  $childCol =  $isFlipped ? $srcCol : $tgtCol;
  
  $modifiedCol = $parentOrChild == 'parent' ? $parentCol : $childCol;
  $modifiedAtom= $parentOrChild == 'parent' ? $parentAtom : $childAtom;
  $stableCol   = $parentOrChild == 'parent' ? $childCol : $parentCol;
  $stableAtom  = $parentOrChild == 'parent' ? $childAtom: $parentAtom;
  
  $tableEsc = escapeSQL($table);
  $modifiedColEsc = escapeSQL($modifiedCol);
  $stableColEsc = escapeSQL($stableCol);
  $modifiedAtomEsc = escapeSQL($modifiedAtom);
  $stableAtomEsc = escapeSQL($stableAtom);
  $originalAtomEsc = escapeSQL($originalAtom);
  
  // only if the stable column is unique, we do an update
  // TODO: maybe we can do updates also in non-unique columns
  if ($tableColumnInfo[$table][$stableCol]['unique']) { // note: this uniqueness is not set as an SQL table attribute
    $query = "UPDATE `$tableEsc` SET `$modifiedColEsc`='$modifiedAtomEsc' WHERE `$stableColEsc`='$stableAtomEsc'";
    emitLog ($query);
    queryDb($query);
  }
  else /* if ($tableColumnInfo[$table][$modifiedCol]['unique']) { // todo: is this ok? no, we'd also have to delete stableAtom originalAtom and check if modified atom even exists, otherwise we need an insert, not an update.
    $query = "UPDATE `$tableEsc` SET `$stableColEsc`='$stableAtomEsc' WHERE `$modifiedColEsc`='$modifiedAtomEsc'";
    emitLog ($query);
    queryDb($query);
  }
  else */ {
    // delete only if there was an $originalAtom
    if ($originalAtom!='') {
      $query = "DELETE FROM `$tableEsc` WHERE `$stableColEsc`='$stableAtomEsc' AND `$modifiedColEsc`='$originalAtomEsc';";
      emitLog ($query);
      queryDb($query);
    }
    $query = "INSERT INTO `$tableEsc` (`$stableColEsc`, `$modifiedColEsc`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')";
    emitLog ($query);
    queryDb($query);
  }
  
  // ensure that the $modifiedAtom is in the concept tables for $modifiedConcept
  $childConcept = $isFlipped ? $relationTableInfo[$rel]['srcConcept'] : $relationTableInfo[$rel]['tgtConcept'];
  $parentConcept =  $isFlipped ? $relationTableInfo[$rel]['tgtConcept'] : $relationTableInfo[$rel]['srcConcept'];
  $modifiedConcept = $parentOrChild == 'parent' ? $parentConcept : $childConcept;
  emitLog ("adding to concept tables: $modifiedAtom : $modifiedConcept");
  addAtomToConcept($modifiedAtom, $modifiedConcept);
  // TODO: errors here are not reported correctly
}

function editDelete($rel, $isFlipped, $parentAtom, $childAtom) {
  global $relationTableInfo;
  global $tableColumnInfo;
  
  emitLog ("editDelete($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom)");
  $srcAtom = $isFlipped ? $childAtom : $parentAtom;
  $tgtAtom = $isFlipped ? $parentAtom : $childAtom;
  
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  
  $tableEsc = escapeSQL($table);
  $srcAtomEsc = escapeSQL($srcAtom);
  $tgtAtomEsc = escapeSQL($tgtAtom);
  $srcColEsc = escapeSQL($srcCol);
  $tgtColEsc = escapeSQL($tgtCol);
  
  if ($tableColumnInfo[$table][$tgtCol]['null']) // note: this uniqueness is not set as an SQL table attribute
    $query = "UPDATE `$tableEsc` SET `$tgtColEsc`=NULL WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc';";
  else
    $query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`='$srcAtomEsc' AND `$tgtColEsc`='$tgtAtomEsc';";
  
  emitLog ($query);
  queryDb($query);
}

// NOTE: log messages emited here are only shown on a commit, not during normal navigation.
function checkRoleRules($roleNr) {
  global $allRoles;
  if ($roleNr == -1) // if no role is selected, evaluate the rules for all roles
  { for ($r = 0; $r < count($allRoles); $r++)
      checkRoleRulesPerRole($r);  
  }
  else
  { $role = $allRoles[$roleNr];
    checkRoleRulesPerRole($roleNr);
  }
//See that the ExecEngine gets the opportunity to do some computations.
//Note that this requires that in ADL, every VIOLATION that starts with {EX} must be assigned to the role ExecEngine!!!
  for ($r = 0; $r < count($allRoles); $r++)
  { $role = $allRoles[$r];
    if ($role['name'] == 'ExecEngine') 
      for ($i = 0; $i < 3; $i++) // limit the effort in case there are loops.
         checkRules($role['ruleNames']);
  }
}

// Precondition: $roleNr >= 0
function checkRoleRulesPerRole($roleNr) {
  global $allRoles;
  
  $role = $allRoles[$roleNr];
  emitLog("Checking rules for role $role[name]");
  checkRules($role['ruleNames']);
}

function checkInvariantRules() {
  global $invariantRuleNames;
  return checkRules($invariantRuleNames);
}

function checkRules($ruleNames)
{ global $allRulesSql;
  global $selectedRoleNr;
// Hier staan de signals in // Sander
  $allRulesHold = true;
  $error = '';

  foreach ($ruleNames as $ruleName)
  { $ruleSql = $allRulesSql[$ruleName];
    $rows = DB_doquerErr($ruleSql['violationsSQL'], $error);
    if ($error) error("While evaluating rule '$ruleName': ".$error);
    
    if (count($rows) > 0)
    { // if the rule has an associated message, we show that instead of the name and the meaning
      $message = $ruleSql['message'] ? $ruleSql['message'] 
                                     : "Rule '$ruleSql[name]' is broken: $ruleSql[meaning]";
      emitAmpersandErr($message);
      $srcNrOfIfcs = getNrOfInterfaces($ruleSql['srcConcept'], $selectedRoleNr);
      $tgtNrOfIfcs = getNrOfInterfaces($ruleSql['tgtConcept'], $selectedRoleNr);
      
      $pairView = $ruleSql['pairView'];
      foreach($rows as $violation)
      { if ($pairView[0]['segmentType'] == 'Text' && strpos($pairView[0]['Text'],'{EX}') === 0) // Check for execution (or not)
        { $theMessage = execPair($violation['src'], $ruleSql['srcConcept'],
                                 $violation['tgt'], $ruleSql['tgtConcept'], $pairView);
  	      $theCleanMessage = strip_tags($theMessage);
      		$theCleanMessage=substr($theCleanMessage,4); // Strip {EX} tag
      		$params = explode(';',$theCleanMessage); // Split off variables
      		$cleanparams = array();
      		foreach ($params as $param) $cleanparams[] = trim($param);
      		$params = $cleanparams;
      		unset($cleanparams);
      		$func = array_shift($params); // First parameter is function name

      		if (function_exists($func))
      		{ emitAmpersandErr($theMessage);
      			call_user_func_array($func,$params);
      		}else{
      			emitAmpersandErr("TODO: Create function $func with " . count($params) . " parameters.");
      		}

        } else //ouwe code incl. die van Sander, moet straks worden opgeruimd
        { $theMessage = showPair($violation['src'], $ruleSql['srcConcept'], $srcNrOfIfcs,
                          $violation['tgt'], $ruleSql['tgtConcept'], $tgtNrOfIfcs, $pairView);
  
  	      $theCleanMessage = strip_tags($theMessage);
  
        	// Message format is '{EX} ChangeColour; Elementname; "Colour"
        	// Check for the {EX} tag to see if we need to run the eXecution Engine:
        	if (strpos($theCleanMessage,'{EX}') === 0)
        	{
        		// Strip EX tag and split variables:
        		$theCleanMessage=substr($theCleanMessage,4);
        		$params = explode(';',$theCleanMessage);
        		$cleanparams = array();
        		foreach ($params as $param) $cleanparams[] = trim($param);
        		$params = $cleanparams;
        		unset($cleanparams);
        		$func = array_shift($params); // First parameter is function name
        
        		if (function_exists($func))
        		{ emitAmpersandErr("Execution Engine will fix this: ".$theMessage);
        			call_user_func_array($func,$params);
        		}else{
        			emitAmpersandErr("TODO: Create function $func with " . count($params) . " parameters.");
        		}
        	} else
        	{	emitAmpersandErr('- ' . $theMessage);
        	}
  	    }
      }
      emitLog('Rule '.$ruleSql['name'].' is broken');
      $allRulesHold = false;
    } else
    { emitLog('Rule '.$ruleSql['name'].' holds');
    }
  }
  return $allRulesHold;
}

function dbStartTransaction() {
  queryDb('START TRANSACTION');
}

function dbCommitTransaction() {
  queryDb('COMMIT');
}

function dbRollbackTransaction() {
  queryDb('ROLLBACK');
}

function queryDb($querySql) {
  $result = DB_doquerErr($querySql, $error);
  if ($error)
    error($error);
  
  return $result;
}

function emitAmpersandErr($err) {
  echo "<div class=\"LogItem AmpersandErr\">$err</div>";
}

function emitLog($msg) {
  echo "<div class=\"LogItem LogMsg\">$msg</div>";
}

function error($msg) {
  die("<div class=\"LogItem Error\">Error in Database.php: $msg</div>");
} // because of this die, the top-level div is not closed, but that's better than continuing in an erroneous situtation
  // the current php session is broken off, which corresponds to a rollback. (doing an explicit roll back here is awkward
  // since it may trigger an error again, causing a loop)

function testRule($ruleName) {
  global $isDev;
  global $allRulesSql;
  
  if (!$isDev) {
    echo "<span style=\"color: red\">Rule test unavailable: prototype was not generated with <tt>--dev</tt> option.</span>";
    return;
  }
  if (!$allRulesSql[$ruleName]) {
    echo "<span style=\"color: red\">Error: rule \"$ruleName\" does not exist.</span>";
    return;
  }
  
  echo "<a href=\"../Installer.php\" style=\"float:right\">Reset database</a>";
  echo "<h2>Testing rule $ruleName</h2>";
  $ruleSql = $allRulesSql[$ruleName];
  $ruleAdl = escapeHtmlAttrStr($ruleSql['ruleAdl']);
  echo "<b>ADL:</b>&nbsp;<tt style=\"color:blue\">$ruleAdl</tt><h4>Rule SQL</h4><pre>$ruleSql[contentsSQL]</pre><h4>results</h4>";
  $error = '';
  $rows = queryDb($ruleSql['contentsSQL'], $error);
  printBinaryTable( $rows );

  echo "<h4>Rule violations SQL</h4><pre>$ruleSql[violationsSQL]</pre><h4>results</h4>";
  $rows = queryDb($ruleSql['violationsSQL'], $error);
  printBinaryTable( $rows );
}

function timestampHtml() {
  $timestamp = getTimestamp();
  echo "<div class=Result timestamp='$timestamp'>$timestamp</div>";
}
?>
