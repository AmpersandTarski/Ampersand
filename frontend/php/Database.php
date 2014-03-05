<?php
error_reporting(E_ALL ^ E_DEPRECATED);
ini_set("display_errors", 1);

require __DIR__.'/../Generics.php';
require_once __DIR__.'/../dbsettings.php';
require_once __DIR__.'/DatabaseUtils.php';

if (!isset($execEngineSays))    // should have been set in 'pluginsettings.php'
   $execEngineSays = true;      // When 'true' the ExecEngine provides high level logging
if (!isset($execEngineWhispers))// should have been set in 'pluginsettings.php'
   $execEngineWhispers = false; // When 'true', the ExecEngine provides details when logging.

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
initSession();

// This module handles four requests: 
//
//     Database.php?resetSession
//     Database.php?getTimestamp
//     Database.php?testRule=..
//     Database.php?commands=..

$selectedRoleNr = isset($_REQUEST['role']) ? $_REQUEST['role'] : -1; // role=-1 (or not specified) means no role is selected

SetSMF('pause', true);

     if (isset($_REQUEST['resetSession']) ) {  resetSession(); }
else if (isset($_REQUEST['getTimestamp']) ) {  timestampHtml(); }
else if (isset($_REQUEST['testRule']) )     {  testRule($_REQUEST['testRule']); }
else if (isset($_REQUEST['ip']) )           {  echo $_SERVER['SERVER_ADDR']; }
else if (isset($_REQUEST['commands']) ) 

{ echo '<div id="UpdateResults">';

  dbStartTransaction();
  emitLog("BEGIN");

  processCommands(); // update database according to edit commands

  // Process processRules first, because the ExecEngine may execute code while processing this stuff.
  echo '<div id="ProcessRuleResults">';
  checkRoleRules($selectedRoleNr);
  echo '</div>';
  
  // Run all stored procedures in the database
  // Doing so AFTER running the ExecEngine allows any problems with stored procedures to be 'fixed'
  // 2do this: create a rule with the same ruleexpression and handle the violation with th ExecEngine
  // runAllProcedures();
  
  echo '<div id="InvariantRuleResults">';
  $invariantRulesHold = checkInvariantRules();
  echo '</div>';
  
  if ($invariantRulesHold) {
    setTimeStamp();
    emitLog("COMMIT");
    dbCommitTransaction();
  } else {
    emitLog("ROLLBACK");
    dbRollbackTransaction();
  }
  echo '</div>';    
}

SetSMF('pause', false);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function processCommands()
{ $commandsJson =$_POST['commands']; 
  if (isset($commandsJson))
  { $commandArray = json_decode($commandsJson);
    foreach ($commandArray as $command) processCommand($command);
  }
}

function processCommand($command)
{ if (!isset($command->dbCmd)) error("Malformed command, missing 'dbCmd'");
  switch ($command->dbCmd)
  { case 'addToConcept':
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
      else 
        error("Command $command->dbCmd is missing parameters");
      break;
    default:
      error("Unknown command '$command->dbCmd'");
  }
}

function editAddToConcept($atom, $concept)
{ emitLog("editAddToConcept($atom, $concept)");
  addAtomToConcept($atom, $concept);  
}

// NOTE: if $originalAtom == '', editUpdate means insert
function editUpdate($rel, $isFlipped, $parentAtom, $childAtom, $parentOrChild, $originalAtom)
{ global $relationTableInfo;
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
  if ($tableColumnInfo[$table][$stableCol]['unique']) // note: this uniqueness is not set as an SQL table attribute
  { $query = "UPDATE `$tableEsc` SET `$modifiedColEsc`='$modifiedAtomEsc' WHERE `$stableColEsc`='$stableAtomEsc'";
    emitLog($query);
    queryDb($query);
  }
  else
  { /* if ($tableColumnInfo[$table][$modifiedCol]['unique'])
       { // todo: is this ok? no, we'd also have to delete stableAtom originalAtom and check if modified atom even exists, otherwise we need an insert, not an update.
         $query = "UPDATE `$tableEsc` SET `$stableColEsc`='$stableAtomEsc' WHERE `$modifiedColEsc`='$modifiedAtomEsc'";
         emitLog ($query);
         queryDb($query);
       }
       else { */
    // delete only if there was an $originalAtom
    if ($originalAtom!='') 
    { $query = "DELETE FROM `$tableEsc` WHERE `$stableColEsc`='$stableAtomEsc' AND `$modifiedColEsc`='$originalAtomEsc';";
      emitLog($query);
      queryDb($query);
    }
    $query = "INSERT INTO `$tableEsc` (`$stableColEsc`, `$modifiedColEsc`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')";
    emitLog($query);
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

function editDelete($rel, $isFlipped, $parentAtom, $childAtom)
{ global $relationTableInfo;
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
  
  emitLog($query);
  queryDb($query);
}

// NOTE: log messages emited here are only shown on a commit, not during normal navigation.
function checkRoleRules($roleNr)
{ global $allRoles;
  $allRoleRules = array();
  
  if ($roleNr == -1) // if no role is selected, evaluate the rules for all roles
  { for ($r = 0; $r < count($allRoles); $r++)
    { // filter rules for role 'DATABASE', these will be handled by runAllProcedures()
      if($allRoles[$r]['name'] != 'DATABASE')
      { // Check every role only one time. 
        $allRoleRules = array_merge((array)$allRoleRules, $allRoles[$r]['ruleNames']); // merge process rules of all roles
      }
    }
   $allRoleRules = array_unique((array)$allRoleRules); // optimize performance by remove duplicate ruleNames
   checkRules($allRoleRules); // check every rule
  }else
    checkRoleRulesPerRole($roleNr);
}

// new function
function runAllProcedures()
{   $query = "CALL AllProcedures";
    emitLog($query);
    queryDb($query);
}

// Precondition: $roleNr >= 0
function checkRoleRulesPerRole($roleNr)
{ global $allRoles;
  
  $role = $allRoles[$roleNr];
  emitLog("Checking rules for role $role[name]");
  checkRules($role['ruleNames']);
}

function checkInvariantRules()
{ global $invariantRuleNames;
  emitLog("Checking invariant rules");
  return checkRules($invariantRuleNames);
}

function checkRules($ruleNames)
{ global $allRoles;
  global $allRulesSql;
  global $selectedRoleNr;
  global $execEningeWhispers; // set in 'pluginsettings.php'
  global $execEningeSays; // set in 'pluginsettings.php'
  global $MappingEngineRules;
  global $ExecEngineRules;
  
  $allRulesHold = true; // by default $allRulesHold = true
  $error = '';

  global $ExecEngineRules; // array met alle ruleNames van de ExecEngine 
  if (!isset($ExecEngineRules))
  { foreach ($allRoles as $role)
    { if ($role['name'] == 'ExecEngine')
      { $ExecEngineRules = $role['ruleNames'];
        break;
  } } }
  
  global $MappingEngineRules; // array met alle ruleNames van de MappingEngine 
  if (!isset($MappingEngineRules))
  { foreach ($allRoles as $role)
    { if ($role['name'] == 'MappingEngine')
      { $MappingEngineRules = $role['ruleNames'];
        break;
  } } }

  foreach ($ruleNames as $ruleName)
  { 
  
  if (is_array($MappingEngineRules) AND in_array($ruleName, $MappingEngineRules)) // Mapping rules are skipped here for performance reasons
  {  // execEngineSays("Skipped $ruleName"); 
     continue;
  }    
    $ruleSql = $allRulesSql[$ruleName];

    $rows = DB_doquerErr($ruleSql['violationsSQL'], $error); // execute violationsSQL to check for violations
      if ($error) error("While evaluating rule '$ruleName': ".$error);
    
      // if there are rows (i.e. violations)
    if (count($rows) == 0)
    { // emitLog('Rule '.$ruleSql['name'].' holds');
    } else
        { $allRulesHold = false;
      emitLog('Rule '.$ruleSql['name'].' is broken');
      
      // if the rule has an associated message, we show that instead of the name and the meaning
      $message = $ruleSql['message'] ? $ruleSql['message'] : "Rule '$ruleSql[name]' is broken: $ruleSql[meaning]";
       // however, for ExecEngine output we have the possibility to suppress some stuff
      if (isset($ExecEngineRules)) // Er is geen garantie dat de rol 'ExecEngine' altijd bestaat.
      { if(in_array($ruleName, $ExecEngineRules))
        { ExecEngineSays("[{EE} restored '$ruleName']");
          ExecEngineWhispers($message);
        } else
        { emitAmpersandLog($message);
        } 
      } else
      { emitAmpersandLog($message);
      }
     
      $srcNrOfIfcs = getNrOfInterfaces($ruleSql['srcConcept'], $selectedRoleNr);
      $tgtNrOfIfcs = getNrOfInterfaces($ruleSql['tgtConcept'], $selectedRoleNr);
      
      $pairView = $ruleSql['pairView']; // pairView contains an array with the fragments of the violations message (if specified)
     
      global $violationID;
      $violationID=0; // For optimization purposes, e.g. for computing transitive closures (see plugin_Warshall.php)
      foreach($rows as $violation)
      { $violationID++; 
        if ($pairView[0]['segmentType'] == 'Text' && strpos($pairView[0]['Text'],'{EX}') === 0) // Check for execution (or not)
        { $theMessage = execPair($violation['src'], $ruleSql['srcConcept'], $violation['tgt'], $ruleSql['tgtConcept'], $pairView);
          // emitAmpersandLog($theMessage);
          $theCleanMessage = strip_tags($theMessage);
          $theCleanMessage = substr($theCleanMessage,4); // Strip {EX} tag

          $functionsToBeCalled = explode('{EX}',$theCleanMessage); // Split off subsequent function calls
          if(count($functionsToBeCalled)>1) ExecEngineWhispers("[[START]]");
        
          foreach ($functionsToBeCalled as $functionToBeCalled) 
          { $params = explode(';',$functionToBeCalled); // Split off variables
            $cleanparams = array();
            foreach ($params as $param) $cleanparams[] = trim($param);
            $params = $cleanparams;
            unset($cleanparams);
            ExecEngineWhispers($functionToBeCalled);
            $func = array_shift($params); // First parameter is function name
            if (function_exists($func))
            { call_user_func_array($func,$params);
            } else
            { ExecEngineSHOUTS("TODO: Create function $func with " . count($params) . " parameters.");
            }
          }
          if(count($functionsToBeCalled)>1) ExecEngineWhispers("[[DONE]]");
        } else
        { $theMessage = showPair($violation['src'], $ruleSql['srcConcept'], $srcNrOfIfcs, $violation['tgt'], $ruleSql['tgtConcept'], $tgtNrOfIfcs, $pairView);
          emitAmpersandLog('- ' . $theMessage);
          }
      }
    }
  }
  return $allRulesHold;
}

function ExecuteAllMappingRules()
{ 
    global $allRulesSql; // for every rule, (almost) all relevant info
    global $allRoles; // for every role, a list of all rules that the role must maintain
    global $MappingEngineRules; // array met alle ruleNames van de MappingEngine 

    $error = '';
  
  global $MappingEngineRules; // array met alle ruleNames van de MappingEngine 
  if (!isset($MappingEngineRules))
  { foreach ($allRoles as $role)
    { if ($role['name'] == 'MappingEngine')
      { $MappingEngineRules = $role['ruleNames'];
        break;
  } } }

    foreach ((array)$MappingEngineRules as $ruleName) // Loop over all rules for the role 'MappingEngine'
    { 
        // Check if Ampersand wants to change something:
        while (GetSMF('pause')) 
        {
            echo "Value of semaphore: " . json_encode(GetPauseSMF()) . ".\n";
            echo "Interupted by high priority process, backing off for 1 second...\n";
            sleep(1);
        }

        $ruleSql = $allRulesSql[$ruleName]; // Get (almost) all relevant info about the rule

        $rows = DB_doquerErr($ruleSql['violationsSQL'], $error); // execute violationsSQL to check for violations
        if ($error) error("While evaluating rule '$ruleName': ".$error);

        // if there are rows (i.e. violations)
        if (count($rows) != 0)
        { emitLog('Rule '.$ruleSql['name'].' is repaired');
        $pairView = $ruleSql['pairView']; // pairView contains an array with the fragments of the violations message (if specified)
        foreach($rows as $violation)
        { if ($pairView[0]['segmentType'] == 'Text' && strpos($pairView[0]['Text'],'{EX}') === 0) // Check for execution (or not)
        { $theMessage = execPair($violation['src'], $ruleSql['srcConcept'], $violation['tgt'], $ruleSql['tgtConcept'], $pairView);
        $theCleanMessage = strip_tags($theMessage);
        $theCleanMessage = substr($theCleanMessage,4); // Strip {EX} tag

        $functionsToBeCalled = explode('{EX}',$theCleanMessage); // Split off subsequent function calls
        if(count($functionsToBeCalled)>1) ExecEngineWhispers("[[START]]");

        foreach ($functionsToBeCalled as $functionToBeCalled) 
        { $params = explode(';',$functionToBeCalled); // Split off variables
        $cleanparams = array();
        foreach ($params as $param) $cleanparams[] = trim($param);
        $params = $cleanparams;
        unset($cleanparams);
        ExecEngineWhispers($functionToBeCalled);
        $func = array_shift($params); // First parameter is function name
        if (function_exists($func))
        { call_user_func_array($func,$params);
        } else
        { ExecEngineSHOUTS("TODO: Create function $func with " . count($params) . " parameters.");
        }
        }
        if(count($functionsToBeCalled)>1) ExecEngineWhispers("[[DONE]]");
        }
        }
        }
    }
    return;
}

function dbStartTransaction()
{ queryDb('START TRANSACTION');
}

function dbCommitTransaction()
{ queryDb('COMMIT');
}

function dbRollbackTransaction()
{ queryDb('ROLLBACK');
}

function queryDb($querySql) {
  $result = DB_doquerErr($querySql, $error);
  if ($error)
    error($error);  
  return $result;
}

function ExecEngineWhispers($msg)
{ global $execEngineWhispers; // set in 'pluginsettings.php'
  global $execEngineSays; // set in 'pluginsettings.php'
  if ($execEngineWhispers && $execEngineSays)
     echo "<div class=\"LogItem AmpersandErr\">$msg</div>";
}

function ExecEngineSays($msg)
{ global $execEngineSays; // set in 'pluginsettings.php'
  if ($execEngineSays)
     echo "<div class=\"LogItem AmpersandErr\"><i>$msg</i></div>";
}

function ExecEngineSHOUTS($msg)
{  echo "<div class=\"LogItem AmpersandErr\" style=\"color:red\"><b>$msg</b></div>";
}

function emitAmpersandLog($msg)
{  echo "<div class=\"LogItem AmpersandErr\">$msg</div>";
}

function emitLog($msg) 
{  echo "<div class=\"LogItem LogMsg\">$msg</div>";
}

function error($err)
{  die("<div class=\"LogItem Error\">Error in Database.php: $err</div>");
} // because of this die, the top-level div is not closed, but that's better than continuing in an erroneous situtation
  // the current php session is broken off, which corresponds to a rollback. (doing an explicit roll back here is awkward
  // since it may trigger an error again, causing a loop)

function testRule($ruleName)
{ global $isDev;
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

function timestampHtml()
{ $timestamp = getTimestamp();
  echo "<div class=Result timestamp='$timestamp'>$timestamp</div>";
}
?>
