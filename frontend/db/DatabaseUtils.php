<?php
require_once __DIR__.'/../dbSettings.php';

// let PHP also report undefined variable references
function terminate_missing_variables($errno, $errstr, $errfile, $errline)
{ if (($errno == E_NOTICE) and (strstr($errstr, "Undefined variable")))
  echo ("$errstr in $errfile line $errline");

  return false; // Let the PHP error handler handle all the rest
}
set_error_handler("terminate_missing_variables");

// Sessions
define("EXPIRATION_TIME", 60*60 ); // expiration time in seconds

function initSession()
{

	$database = Database::singleton();
// when using $_SESSION, we get a nonsense warning if not declared global, however here
// we only do isset, so no need for global
  global $conceptTableInfo;

  session_start(); // Start a new, or resume the existing, PHP session

// only execute session code when concept SESSION is used by adl script
  if (isset($conceptTableInfo['SESSION']))
  {	// TODO: until error handling is improved, this hack tries a dummy query and returns silently if it fails.
    //       This way, errors during initSession do not prevent the reset-database link from being visible.
	try {
		$database->Exe("SELECT * FROM `__SessionTimeout__` WHERE false");
	} catch (Exception $e) {
		return;
	}
    

// Remove expired Ampersand-sessions from __SessionTimeout__ and all concept tables and relations where it appears.
    $expirationLimit = time() - EXPIRATION_TIME; 
    $expiredSessions = array_column($database->Exe("SELECT SESSION FROM `__SessionTimeout__` WHERE lastAccess < $expirationLimit"), 1);
    foreach ($expiredSessions as $expiredSessionAtom)
      deleteSession($expiredSessionAtom);
    
// If the PHP session has the Ampersand sessionAtom, retrieve it. 
// Note that it may still refer to an Ampersand session that has expired and therefore no longer exists in the Ampersand administration
    $sessionAtom = $_SESSION['sessionAtom']; 
// create a new session if $sessionAtom is not set (browser started a new session) 
// or $sessionAtom is not in SESSIONS (previous session expired)
    if (!isset($sessionAtom) || !Concept::isAtomInConcept($sessionAtom, 'SESSION'))
    { $sessionAtom = $database->createNewAtom('SESSION');
      $_SESSION['sessionAtom']  = $sessionAtom;
      
    }
// echo "sessionAtom = [$sessionAtom]<br>";
    
    $timeInSeconds = time();
    $database->Exe("INSERT INTO `__SessionTimeout__` (`SESSION`,`lastAccess`) VALUES ('$_SESSION[sessionAtom]','$timeInSeconds')".
              "ON DUPLICATE KEY UPDATE `lastAccess` = '$timeInSeconds'");
    //echo "SessionAtom is $sessionAtom access is $timeInSeconds";
  }
}

function resetSession()
{ global $conceptTableInfo;
  
  if ($conceptTableInfo['SESSION']) // only execute session code when concept SESSION is used by adl script
  { deleteSession($_SESSION['sessionAtom']);
  }
}

function deleteSession($sessionAtom)
{ //echo "deleting $sessionAtom<br/>";
	$database = Database::singleton();
	$database->Exe("DELETE FROM `__SessionTimeout__` WHERE SESSION = '$sessionAtom'");
	$database->deleteAtom($sessionAtom, 'SESSION');
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


function notNull($atom) { // need a type-based comparison, otherwise 0 is also null
  return $atom !== null;
}




function getTopLevelInterfacesForConcept($concept, $roleNr) {
  global $allInterfaceObjects;
  $role = new Role($roleNr);
  
  $interfacesForConcept = array();
  foreach($allInterfaceObjects as $interface) {
    if (($interface['srcConcept']==$concept || in_array($concept, Concept::getSpecializations($interface['srcConcept']))) 
       && $role->isInterfaceForRole($interface['name']))
    $interfacesForConcept[] = $interface;
  }
  return $interfacesForConcept;
}


// Misc utils


function getCoDomainAtoms($atom, $selectRel) {
	$database = Database::singleton();
	return array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$selectRel.") AS results WHERE src='".addslashes($atom)."'"), 'tgt');
}


// Html generation utils


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
  return str_replace(array('"', '&'), array('&quot;', '%26'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
}


?>
