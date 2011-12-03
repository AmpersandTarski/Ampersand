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

  foreach ($conceptTableInfo[$concept] as $conceptTableCol) { // $conceptTableInfo[$concept] is an array of tables&columns 
    $conceptTable = $conceptTableCol['table'];                // maintaining $concept.
    $conceptCol = $conceptTableCol['col'];                    // We insert the new atom in each of them

    $conceptTableEsc = addSlashes($conceptTable);
    $conceptColEsc = addSlashes($conceptCol);
    $newAtomEsc = addSlashes($newAtom);

    $existingAtoms = firstCol(DB_doquer($dbName, "SELECT `$conceptColEsc` FROM `$conceptTableEsc`"));
    
    if (!in_array($newAtom, $existingAtoms))
      DB_doquer($dbName, "INSERT INTO `$conceptTableEsc` (`$conceptColEsc`) VALUES ('$newAtomEsc')");
  }
}

function createNewAtom($concept) {
  $newAtom = mkUniqueAtomByTime($concept);
  
  addAtomToConcept($newAtom, $concept);
  return $newAtom;
}

/* NOTE: this only  a single concept table, so it assumes that all concept tables for $concept are up to date (or at least the first one) */
function getAllConceptAtoms($concept) {
  global $dbName;
  global $conceptTableInfo;
  
  $conceptTable = $conceptTableInfo[$concept][0]['table']; // $conceptTableInfo[$concept] is an array of tables&columns maintaining $concept
  $conceptCol = $conceptTableInfo[$concept][0]['col'];     // for lookup, we just take the first
  
  $conceptTableEsc = addSlashes($conceptTable);
  $conceptColEsc = addSlashes($conceptCol);
  
  return firstCol(DB_doquer($dbName, "SELECT `$conceptColEsc` FROM `$conceptTableEsc`"));  
}

function getTopLevelInterfacesForConcept($concept) {
  global $allInterfaceObjects;
  $interfacesForConcept = array();
  foreach($allInterfaceObjects as $interface) {
    if ($interface['srcConcept']==$concept)
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
  return 'SELECT DISTINCT `tgt` FROM ('.addSlashes($selectRel).') AS results WHERE src=\''.addSlashes($atom).'\'';
}


// Html generation utils

function printBinaryTable($table) {
  echo '<table>';
  foreach ($table as $row)
  echo '<tr><td>'.$row['src'].'</td><td>'.$row['tgt'].'</td></tr>';
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
  return str_replace(array('"'), array('&quot;'), $str); // we do addSlashes and replace \" by &quot; and \' by '
}

function showJsStr($str) {
  return "'".escapeJsStr($str)."'";
}

function escapeJsStr($str) {
  return addSlashes($str);
}

function jsBool($b) {
	return $b ? 'true' : 'false';
}
// This is needed for non-javascript urls, where javascript would call encodeURIComponent
// We only handle the &, the browser takes care of the rest.
function escapeURI($str) {
    return str_replace(array('&'), array('%26'), $str); // replace & by %26
}
?>