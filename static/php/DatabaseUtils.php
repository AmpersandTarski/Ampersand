<?php
require __DIR__.'/../dbSettings.php';
// We need the __DIR__ because all require statements are relative to the path of the browser-requested php file.
// Otherwise, when DatabaseUtils is included by Interface.php, we would need 'dbSettings.php', but when included
// by php/Database.php, we would need '../dbSettings.php'.
  

function DB_doquer($DbName, $quer) {
  $result = DB_doquerErr($DbName, $quer, $error);
  
  if ($error)
    die("<div class=InternalError>$error</div>");
  return $result;
}

function DB_doquerErr($DbName, $quer, &$error)
{
  global $DB_link,$DB_errs;
  $DB_slct = mysql_select_db($DbName,$DB_link);
    
  $result=mysql_query($quer,$DB_link);
  if(!$result){
    $error = 'Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'": '.mysql_error();
    return false;
  }
  if($result===true) return true; // succes.. but no contents..
  $rows=Array();
  while (($row = @mysql_fetch_array($result))!==false) {
    $rows[]=$row;
    unset($row);
  }
  return $rows;
}

function getGeneralizations($concept) {
  global $allGeneralizations;

  return isset( $allGeneralizations[$concept]) ? $allGeneralizations[$concept] : array ();
}

function getSpecializations($concept) {
  global $allSpecializations;

  return isset( $allSpecializations[$concept]) ? $allSpecializations[$concept] : array ();
}

function keyForConcept($concept) {
  global $allKeys;

  foreach ($allKeys as $key)
    if (in_array($concept, $key['conceptAndSpecs']))
      return $key;

  return null;
}

function showKeyAtom($atom, $concept) {
  global $db;
  
  $keyDef = keyForConcept($concept);

  if (!$keyDef || $atom == '') {
    return $atom;
  }
  else {
    $keyStrs = array ();
    foreach ($keyDef['exps'] as $label => $expSql) {
      $r = getCoDomainAtoms($db, $atom, $expSql);
      $keyStrs[] = $r[0];
    }
    return implode(', ', $keyStrs);
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

function escapeHtmlAttrStr($str) {
  return str_replace(array('"'), array('&quot;'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
}

function showJsStr($str) {
  return "'".escapeJsStr($str)."'";
}

function escapeJsStr($str) {
  return escapeSQL($str);
}

function jsBool($b) {
	return $b ? 'true' : 'false';
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