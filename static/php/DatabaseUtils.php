<?php

/* TODO

-debug just puts elements in the page, should be in a specific place
-errsors are in nasty globarl $DB_errss
-fix require "dbsettings.php". just declare a constant that returns the settings, and do the connection somewhere else
-check why DB_doquer creates 0 and tgt fields for each row (even when there is only 1 column)
-rewrite DB_doquer
-Do we want objects instead of arrays? do they add anything?
- escape correctly (also need to use htmlSpecialChars?)
- use an array for getAllAtomsFor
*/

require "dbsettings.php";
  
$DB_errs = array();
    
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

function printBinaryTable($table) {
  echo '<table>';
  foreach ($table as $row)
    echo '<tr><td>'.$row['src'].'</td><td>'.$row['tgt'].'</td></tr>';
  echo '</table>';
}

function dbStartTransaction($dbName) {
  DB_doquer($dbName, 'START TRANSACTION');
}

function dbCommitTransaction($dbName) {
  DB_doquer($dbName, 'COMMIT');
}

function dbRollbackTransaction($dbName) {
  DB_doquer($dbName, 'ROLLBACK');
}

function DB_debug($txt,$lvl=0){
  global $DB_debug;
  if ($lvl<=$DB_debug) {
    echo "<i title=\"debug level $lvl\">$txt</i>\n<P />\n";
    return true;
  }
  else
    return false;
}
  
  
function DB_doquer($DbName, $quer,$debug=5)
{
  global $DB_link,$DB_errs;
  $DB_slct = mysql_select_db($DbName,$DB_link);
    
  DB_debug($quer,$debug);
  $result=mysql_query($quer,$DB_link);
  if(!$result){
    DB_debug('Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'": '.mysql_error(),2);
    $DB_errs[]='Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'"';
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


///////// Interface stuff (does not belong here) /////////

function topLevelInterfaceLinks($interfaces) {
  foreach($interfaces as $interface) {
    if ($interface['concept']=='ONE')
      echo '<a href="Interface.php?interface='.escapeHtmlAttrStr(escapeURI($interface['name'])).'&atom=1">'.htmlSpecialChars($interface['name']).'</a><br>';
  }
}

function generateInterfaceMap($interfaces) {
  echo 'function getInterfacesMap() {';
  echo '  var interfacesMap = new Array();';
  foreach($interfaces as $interface) {
    echo '  mapInsert(interfacesMap, '.showHtmlAttrStr($interface['concept']).', '.showHtmlAttrStr($interface['name']).');';
  }
  echo '  return interfacesMap;';
  echo '}';
}

function generateInterface($db, $interface, $srcAtom) {
  $html = "";
  emit($html, withClass('Label', htmlSpecialChars($interface['name'])));
  
  if ($srcAtom == null)
    $codomainAtoms = array (); // in case the table would contain (null, some atom)  
  else
    $codomainAtoms = array_filter(getCoDomainAtoms($db, $srcAtom, $interface['sqlQuery'])); // filter, in case table contains ($srcAtom, null)

  // todo: cleanup, rename concept/srcConcept. maybe just srcConcept and tgtConcept 
  // todo: maybe Container should be called Relation?
  // todo: probably don't want different classes AtomList and Atomic, but just an attr list/singleton or something
  $isUni = false; // temporarily disabled the univalence check (so everything is a list) $interface['isUnivalent'];   
  if (!$isUni) $codomainAtoms[] = null; // the null is presented as a NewAtomTemplate (which is cloned when inserting a new atom)
  
  $relationAttrs = $interface['relation']=='' ? '' : ' relation='.showHtmlAttrStr($interface['relation']).' relationIsFlipped='.showHtmlAttrStr(jsBool($interface['relationIsFlipped']));
  if (!$isUni) emit($html, '<table class="AtomList Container" concept='.showHtmlAttrStr($interface['concept']).$relationAttrs.'><tbody>'); // todo: change name, these things are not necessarily atoms
  else         emit($html, '<div class="Atomic Container" concept='.showHtmlAttrStr($interface['concept']).$relationAttrs.'>'); // tbody is inserted automatically, but we do it explicitly to make the structure more clear
  foreach($codomainAtoms as $tgtAtom) {  // srcColumn needs to be in div because its is used by js code
    if (!$isUni) emit($html, '<tr '.($tgtAtom==null?' class=NewAtomTemplate':'').'><td class=DeleteStub>&nbsp;</td><td class=AtomListElt>');
    emit($html, generateInterfaceList($db, $interface, $tgtAtom));         // &nbsp; is to prevent empty strings from having height 1
    if (!$isUni) emit($html,'</td></tr>'); 
  }
  
  if (!$isUni) emit($html, '<tr><td></td><td class=InsertStub>Insert new '.htmlSpecialChars($interface['concept']).'</td></tr><tbody></table>');
  else         emit($html, '</div>');
  return $html;
}

function generateInterfaceList($db, $parentInterface, $atom) {
  $html = "";
  $interfaces = $parentInterface['subInterfaces'];

  // if $atom is null, we are presenting a template

  emit($html, '<div class=Atom atom='.showHtmlAttrStr($atom).' status='.($atom?'unchanged':'new').'>');
  // the old prototype did not show the atom when there are subinterfaces
    
  emit($html, '<div class=AtomName>'.htmlSpecialChars($atom).'</div>');
  if (count($interfaces) > 0) {
    emit($html, '<div class=Interface>');
    foreach($interfaces as $interface) {
      emit($html, generateInterface($db, $interface, $atom));
    }
    emit($html, '</div>'); // div class=Interface
  }
  emit($html, '</div>'); // div class=Atom
  return $html;
}

function echoLn($str) {
  echo $str.'<br/>';
}

function getCoDomainAtoms($db, $atom, $selectRel) {
  return targetCol(DB_doquer($db, selectCoDomain($atom, $selectRel)));
}

function selectCoDomain($atom, $selectRel) {
  return 'SELECT DISTINCT `tgt` FROM ('.addSlashes($selectRel).') as results where src=\''.addSlashes($atom).'\'';
}


// utils

function withClass($class, $elt) {
  return "<div class=$class>$elt</div>";
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