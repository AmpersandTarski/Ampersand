<?php
require __DIR__.'/../dbSettings.php';
// We need the __DIR__ because all require statements are relative to the path of the browser-requested php file.
// Otherwise, when DatabaseUtils is included by Interface.php, we would need 'dbSettings.php', but when included
// by php/Database.php, we would need '../dbSettings.php'.

// Sessions

define( "EXPIRATION_TIME", 5*60 ); // expiration time in seconds

function initSession() {
  global $dbName;
  global $conceptTableInfo;
  
  if ($conceptTableInfo['SESSION']) { // only execute session code when concept SESSION is used by adl script
    // TODO: until error handling is improved, this hack tries a dummy query and returns silently if it fails.
    //       This way, errors during initSession do not prevent the reset-database link from being visible.
    DB_doquerErr($dbName, "SELECT * FROM `__SessionTimeout__` WHERE false", $error);
    if ($error) return;
    session_start();
    cleanupExpiredSessions();
    
    $sessionAtom = $_SESSION['sessionAtom'];
    
    // create a new session if $sessionAtom is not set (browser started a new session) 
    // or $sessionAtom is not in SESSIONS (previous session expired)
    if (!isset($sessionAtom) || !isAtomInConcept($sessionAtom, 'SESSION')) {
      $sessionAtom = mkUniqueAtomByTime('SESSION');
      $_SESSION['sessionAtom']  = $sessionAtom;
      addAtomToConcept($sessionAtom, 'SESSION');
    }
    
    $timeInSeconds = time();
    DB_doquer($dbName, "INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('$_SESSION[sessionAtom]','$timeInSeconds')".
                       "ON DUPLICATE KEY UPDATE `lastAccess` = '$timeInSeconds'");
    //echo "SessionAtom is $sessionAtom access is $timeInSeconds";
  }
}

function resetSession() {
  global $conceptTableInfo;
  
  if ($conceptTableInfo['SESSION']) // only execute session code when concept SESSION is used by adl script
    deleteSession($_SESSION['sessionAtom']);
}

function deleteSession($sessionAtom) {
  global $dbName;
  
  //echo "deleting $sessionAtom<br/>";
  DB_doquer($dbName, "DELETE FROM `__SessionTimeout__` WHERE SESSION = '$sessionAtom';");
  deleteAtom($sessionAtom, 'SESSION');
  
}

// Remove expired sessions from __SessionTimeout__ and all concept tables and relations where it appears.
function cleanupExpiredSessions() {
  global $dbName;
  $expirationLimit = time() - EXPIRATION_TIME;
  
  $expiredSessions = firstCol(DB_doquer($dbName, "SELECT SESSION FROM `__SessionTimeout__` WHERE lastAccess < $expirationLimit;"));
  foreach ($expiredSessions as $sessionAtom)
    deleteSession($sessionAtom);
}


// Queries

function DB_doquer($DbName, $quer) {
  $result = DB_doquerErr($DbName, $quer, $error);
  
  if ($error)
    die("<div class=InternalError>$error</div>");
  return $result;
}

function DB_doquerErr($DbName, $quer, &$error)
{
  //Replace the special atom value _SESSION by the current sessionAtom
  $quer =  str_replace("_SESSION", $_SESSION['sessionAtom'], $quer);
  
  global $DB_link,$DB_errs;
  $DB_slct = mysql_select_db($DbName,$DB_link);
    
  $result=mysql_query($quer,$DB_link);
  if(!$result){
    $error = 'Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'": '.mysql_error();
    return false;
  }
  if($result===true) return true; // success.. but no contents..
  $rows=Array();
  while (($row = @mysql_fetch_array($result))!==false) {
    $rows[]=$row;
    unset($row);
  }
  return $rows;
}

function getSpecializations($concept) {
  global $allSpecializations;

  return isset( $allSpecializations[$concept]) ? $allSpecializations[$concept] : array ();
}

function getKey($concept) {
  global $allKeys;

  foreach ($allKeys as $key)
    if ($concept == $key['concept'] || in_array($concept, getSpecializations($key['concept'])))
      return $key;

  return null;
}

function showKeyAtom($atom, $concept) {
  global $db;
  
  $keyDef = getKey($concept);

  if (!$keyDef || $atom == '') {
    return $atom;
  }
  else {
    $keyStrs = array ();
    foreach ($keyDef['segments'] as $keySegment) 
      if ($keySegment['segmentType'] == 'Text')
        $keyStrs[] = $keySegment['Text'];
      else {
        $r = getCoDomainAtoms($db, $atom, $keySegment['expSQL']);
        $keyStrs[] = $r[0];
      }
    return implode($keyStrs);
  }
}


// return an atom "Concept_<n>" that is not in $existingAtoms (make sure that $existingAtoms covers all concept tables)
function mkUniqueAtom($existingAtoms, $concept) {
  $generatedAtomNrs = array();
  foreach (array_unique($existingAtoms) as $atom) {
    preg_match('/\A'.$concept.'\_(?P<number>[123456789]\d*)\z/', $atom, $matches);
    // don't match nrs with leading 0's since we don't generate those
    $generatedAtomNrs[] = $matches['number'];
  }

  $generatedAtomNrs = array_filter($generatedAtomNrs); // filter out all the non-numbers (which are null)
  sort($generatedAtomNrs);
  foreach ($generatedAtomNrs as $i=>&$nr) {
    if ($nr != $i+1) // as soon as $generatedAtomNrs[i] != i+1, we arrived at a gap in the sorted number sequence and we can use i+1
    return $concept.'_'.($i+1);
  }
  return $concept.'_'.(count($generatedAtomNrs)+1);
}

function mkUniqueAtomByTime($concept) {
  $time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
  return $concept.'_'.$time[1]."_".substr($time[0], 2,6);  // we drop the leading "0." and trailing "00"  from the microseconds  
}

function addAtomToConcept($newAtom, $concept) {
  global $dbName;
  global $conceptTableInfo;

  foreach ($conceptTableInfo[$concept] as $conceptTableCol) { // $conceptTableInfo[$concept] is an array of tables with arrays of columns 
    $conceptTable = $conceptTableCol['table'];                // maintaining $concept. (we have an array rather than a single column because of generalizations)
    $conceptCols = $conceptTableCol['cols'];                  // We insert the new atom in each of them.

    
    $conceptTableEsc = escapeSQL($conceptTable);
    $newAtomEsc = escapeSQL($newAtom); 
    
    // invariant: all concept tables (which are columns) are maintained properly, so we can query an arbitrary one for checking the existence of a concept
    $firstConceptColEsc = escapeSQL($conceptCols[0]);
    
    $existingAtoms = firstCol(DB_doquer($dbName, "SELECT `$firstConceptColEsc` FROM `$conceptTableEsc`")); // no need to filter duplicates and NULLs
    
    if (!in_array($newAtom, $existingAtoms)) {
      $allConceptColsEsc = '`'.implode('`, `', $conceptCols).'`';
      $newAtomsEsc = array_fill(0, count($conceptCols), $newAtomEsc);
      $allValuesEsc = "'".implode("', '", $newAtomsEsc)."'";
            
      DB_doquer($dbName, "INSERT INTO `$conceptTableEsc` ($allConceptColsEsc) VALUES ($allValuesEsc)");
    }
  }
}

// Remove all occurrences of $atom in the database (all concept tables and all relations)
// In tables where the atom may not be null, the entire row is removed. 
// TODO: If all relation fields in a wide table are null, the entire row could be deleted, but this doesn't
//       happen now. As a result, relation queries may return some nulls, but these are filtered out anyway.
function deleteAtom($atom, $concept) {
  global $dbName;
  global $tableColumnInfo;


  foreach ($tableColumnInfo as $table => $tableInfo)
  foreach ($tableInfo as $column => $fieldInfo) {
    // TODO: could be optimized by doing one query per table. But deleting per column yields the same result.
    //       (unlike adding)
    if ($fieldInfo['concept']==$concept) {
      $tableEsc = escapeSQL($table);
      $columnEsc = escapeSQL($column);
      $atomEsc = escapeSQL($atom);

      if ($fieldInfo['null'])  // if the field can be null, we set all occurrences to null
        $query = "UPDATE `$tableEsc` SET `$columnEsc`=NULL WHERE `$columnEsc`='$atomEsc';";
      else // otherwise, we remove the entire row for each occurrence
        $query = "DELETE FROM `$tableEsc` WHERE `$columnEsc` = '$atomEsc';";
      //echo $query;
      DB_doquer($dbName, $query);
    }
  }
}

// Currently not used. Javascript creates a unique name and index.php adds to to the concept in a temporary transaction.
function createNewAtom($concept) {
  $newAtom = mkUniqueAtomByTime($concept);
  
  addAtomToConcept($newAtom, $concept);
  return $newAtom;
}

/* invariant: all concept tables (which are columns) are maintained properly, so we can query an arbitrary one to obtain the list of atoms */
function getAllConceptAtoms($concept) {
  global $dbName;
  global $conceptTableInfo;
  $conceptTable = $conceptTableInfo[$concept][0]['table']; // $conceptTableInfo[$concept] is an array of tables with arrays of columns maintaining $concept
  $conceptCol = $conceptTableInfo[$concept][0]['cols'][0]; // for lookup, we just take the first table and its first column
  $conceptTableEsc = escapeSQL($conceptTable);
  $conceptColEsc = escapeSQL($conceptCol);
  
  // need to do array_unique and array_filter, since concept table may contain duplicates and NULLs
  return array_unique(array_filter(firstCol(DB_doquer($dbName, "SELECT `$conceptColEsc` FROM `$conceptTableEsc`")),notNull));  
}

function notNull($atom) { // need a type-based comparison, otherwise 0 is also null
  return $atom !== null;
}

function isAtomInConcept($atom, $concept) {
  return in_array( $atom, getAllConceptAtoms($concept) );
}

function isInterfaceForRole($interface, $roleNr, $roleName) {
  return in_array($roleName, $interface['interfaceRoles']) || count($interface['interfaceRoles'])==0 || $roleNr == -1;
  // an interface is visible if: the interface roles contain $role; the interface does not specify roles; or no role is selected   
} 

function getTopLevelInterfacesForConcept($concept, $roleNr, $roleName) {
  global $allInterfaceObjects;
  $interfacesForConcept = array();
  foreach($allInterfaceObjects as $interface) {
    if (($interface['srcConcept']==$concept || in_array($concept, getSpecializations($interface['srcConcept']))) 
       && isInterfaceForRole($interface, $roleNr, $roleName))
    $interfacesForConcept[] = $interface;
  }
  return $interfacesForConcept;
}


// Misc utils

function firstRow($rows) {
  return $rows[0];
}

function firstCol($rows) {
  foreach ($rows as $i=>&$v)
  $v=$v[0];
  return $rows;
}

function targetCol($rows) {
  foreach ($rows as $i=>&$v)
  $v=$v['tgt'];
  return $rows;
}

function getCoDomainAtoms($db, $atom, $selectRel) {
  return targetCol(DB_doquer($db, selectCoDomain($atom, $selectRel)));
}

function selectCoDomain($atom, $selectRel) {
  return 'SELECT DISTINCT `tgt` FROM ('.$selectRel.') AS results WHERE src=\''.escapeSQL($atom).'\'';
}


// Timestamps

// return the most recent modification time for the database (only Ampersand edit operations are recorded)
function getTimestamp(&$error) {
  global $dbName;
  $timestampRow = DB_doquerErr($dbName, "SELECT MAX(`Seconds`) FROM `__History__`", $error);

  if ($error)
    return '0';
  else
    return $timestampRow[0][0];
}

// set modification timestamp to the current time
function setTimestamp() {
  global $dbName;
  
  $time = explode(' ', microTime()); // yields [seconds,microseconds] both in seconds, e.g. ["1322761879", "0.85629400"]
  $microseconds = substr($time[0], 2,6); // we drop the leading "0." and trailing "00"  from the microseconds
  $seconds =$time[1].$microseconds;  
  $date = date("j-M-Y, H:i:s.").$microseconds; 
  DB_doquer($dbName, "INSERT INTO `__History__` (`Seconds`,`Date`) VALUES ('$seconds','$date')");
  // TODO: add error checking
}


// Html generation utils

function printBinaryTable($table) {
  echo '<table border=solid>';
  foreach ($table as $row)
  echo '<tr><td>'.$row['src'].'&nbsp;</td><td>'.$row['tgt'].'&nbsp;</td></tr>';
  echo '</table>';
}

function echoLn($str) {
  echo $str.'<br/>';
}

function emit(&$lines,$line) {
  $lines.=$line."\n";
}

// for use in specifiying values for attributes to html elements (eg. <div attr=VALUE>)
// " -> &quot,  
function showHtmlAttrStr($str) {
  return '"'.escapeHtmlAttrStr($str).'"';
}

function showHtmlAttrBool($b) {
  return $b ? '"true"' : '"false"';
}

function escapeHtmlAttrStr($str) {
  return str_replace(array('"'), array('&quot;'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
}

function showJsStr($str) {
  return "'".escapeJsStr($str)."'";
}

function escapeJsStr($str) {
  return escapeSQL($str);
}

// This is needed for non-javascript urls, where javascript would call encodeURIComponent
// We only handle the &, the browser takes care of the rest.
function escapeURI($str) {
    return str_replace(array('&'), array('%26'), $str); // replace & by %26
}

function escapeSQL($str) {
    return addslashes($str);
}
?>