<?php // Functions for easier logging/debugging in Ampersand script

// VIOLATION (TXT "{EX} LogText;" all sorts of texts, including contents of relations etc. can be logged
function LogText($logtext, $logclass='Log')
{	\Ampersand\Logger::getLogger($logclass)->debug($logtext);
}

// VIOLATION (TXT "{EX} LogViolation;", SRC I, TXT ";", TGT I
function LogViolation($srcAtom,$tgtAtom)
{	\Ampersand\Logger::getLogger('EXECENGINE')->debug("Violation: ({$srcAtom},{$tgtAtom})");
}

?>