<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require __DIR__.'/../Generics.php'; 
// defines $dbName, $isDev, $relationTableInfo, $allInterfaceObjects, $allRulesSql, $invariantRuleNames, and $allRoles

require_once __DIR__.'/DatabaseUtils.php';

// This module handles three requests: 
//
//     Database.php?getAllAtomsForConcept=..
//     Database.php?destroyAtom=..&concept=..    note: unlike delete, this removes the atom from its concept table
//                                                     it is assumed that the atom does not occur in any other tables
                                                        
//     Database.php?commands=..

if (isset($_REQUEST['getAtomsForConcept']) ) {
  listAtomsForConcept($_REQUEST['getAtomsForConcept']);
} else if (isset($_REQUEST['destroyAtom']) ) {
  removeAtomFromConcept($_REQUEST['destroyAtom'], $_REQUEST['concept']);
} else if (isset($_REQUEST['getTimestamp']) ) {
  timestampHtml();
} else if (isset($_REQUEST['testRule']) ) {
  testRule($_REQUEST['testRule']);
} else if (isset($_REQUEST['commands']) ) {
  echo '<div id="UpdateResults">';

  
  dbStartTransaction($dbName);
  emitLog('BEGIN');

  processCommands(); // update database according to edit commands

  echo '<div id="InvariantRuleResults">';
  $invariantRulesHold = checkInvariantRules();
  echo '</div>';
  
  echo '<div id="ProcessRuleResults">';
  checkRoleRules($_REQUEST['role']);
  echo '</div>';
  
  if ($invariantRulesHold) {
    setTimeStamp();
    emitLog('COMMIT');
    dbCommitTransaction($dbName);
  } else {
    emitLog('ROLLBACK');
    dbRollbackTransaction($dbName);
  }
  echo '</div>';
}

function processCommands() {  
  global $dbName; 
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
  global $dbName;
  global $relationTableInfo;
  global $tableColumnInfo;
  
  emitLog("editUpdate($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom, $parentOrChild, $originalAtom)");
  
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
    queryDb($dbName, $query);
  }
  else /* if ($tableColumnInfo[$table][$modifiedCol]['unique']) { // todo: is this ok? no, we'd also have to delete stableAtom originalAtom and check if modified atom even exists, otherwise we need an insert, not an update.
    $query = "UPDATE `$tableEsc` SET `$stableColEsc`='$stableAtomEsc' WHERE `$modifiedColEsc`='$modifiedAtomEsc'";
    emitLog ($query);
    queryDb($dbName, $query);
  }
  else */ {
    // delete only if there was an $originalAtom
    if ($originalAtom!='') {
      $query = "DELETE FROM `$tableEsc` WHERE `$stableColEsc`='$stableAtomEsc' AND `$modifiedColEsc`='$originalAtomEsc';";
      emitLog ($query);
      queryDb($dbName, $query);
    }
    $query = "INSERT INTO `$tableEsc` (`$stableColEsc`, `$modifiedColEsc`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')";
    emitLog ($query);
    queryDb($dbName, $query);
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
  global $dbName; 
  global $relationTableInfo;
  emitLog ("editDelete($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom)");
  $srcAtom = $isFlipped ? $childAtom : $parentAtom;
  $tgtAtom = $isFlipped ? $parentAtom : $childAtom;
  
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  
  $tableEsc = escapeSQL($table);
  $srcAtomEsc = escapeSQL($srcAtom);
  $tgtAtomEsc = escapeSQL($tgtAtom);
  $query = "DELETE FROM `$tableEsc` WHERE `$srcCol`='$srcAtomEsc' AND `$tgtCol`='$tgtAtomEsc';";
  emitLog ($query);
  queryDb($dbName, $query);
}

// NOTE: log messages emited here are only shown on a commit, not during normal navigation.
function checkRoleRules($roleNr) {
  global $allRoles;  
  
  if ($roleNr >= 0) {
    $role = $allRoles[$roleNr];
    emitLog("Checking rules for role $role[name]");
    return checkRules($role['ruleNames']);
  }
  else
    return true;
}

function checkInvariantRules() {
  global $invariantRuleNames;
  return checkRules($invariantRuleNames);
}

function checkRules($ruleNames) {
  global $dbName;
  global $allRulesSql;
  
  $allRulesHold = true;
  
  foreach ($ruleNames as $ruleName) {
    $ruleSql = $allRulesSql[$ruleName];
    $rows = queryDb($dbName, $ruleSql['violationsSQL'], $error);
    if ($error) error($error);
    
    if (count($rows) > 0) {
      // if the rule has an associated message, we show that instead of the name and the meaning
      $message = $ruleSql['message'] ? $ruleSql['message'] 
                                     : "Rule '$ruleSql[name]' is broken: $ruleSql[meaning]";
      emitAmpersandErr($message);
      foreach($rows as $violation){
        emitAmpersandErr("- ('$violation[src]', '$violation[tgt]')");
      }
      emitLog('Rule '.$ruleSql['name'].' is broken');
      $allRulesHold = false;
    }
    else
      emitLog('Rule '.$ruleSql['name'].' holds');
  } 
  return $allRulesHold;
}

function dbStartTransaction($dbName) {
  queryDb($dbName, 'START TRANSACTION');
}

function dbCommitTransaction($dbName) {
  queryDb($dbName, 'COMMIT');
}

function dbRollbackTransaction($dbName) {
  queryDb($dbName, 'ROLLBACK');
}

function queryDb($dbName, $querySql) {
  $result = DB_doquerErr($dbName, $querySql, $error);
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

function listAtomsForConcept($concept) {
  echo json_encode (array ( 'res' => getAllConceptAtoms($concept) ));
  // todo: pass errors back to javascript
}

function testRule($ruleName) {
  global $isDev;
  global $dbName;
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
  $ruleAdl = escapeHtmlAttrStr($ruleSql[ruleAdl]);
  echo "<b>ADL:</b>&nbsp;<tt style=\"color:blue\">$ruleAdl</tt><h4>Rule SQL</h4><pre>$ruleSql[contentsSQL]</pre><h4>results</h4>";
  $rows = queryDb($dbName, $ruleSql['contentsSQL'], $error);
  printBinaryTable( $rows );

  echo "<h4>Rule violations SQL</h4><pre>$ruleSql[violationsSQL]</pre><h4>results</h4>";
  $rows = queryDb($dbName, $ruleSql['violationsSQL'], $error);
  printBinaryTable( $rows );
}

function timestampHtml() {
  $timestamp = getTimestamp();
  echo "<div class=Result timestamp='$timestamp'>$timestamp</div>";
}
?>