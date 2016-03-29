<?php // Functions for easier logging/debugging in Ampersand script

// VIOLATION (TXT "{EX} LogText;" all sorts of texts, including contents of relations etc. can be logged
function LogText($logtext, $logclass='Log')
{	Notifications::addLog($logtext, $logclass);
	return;
}

// VIOLATION (TXT "{EX} LogViolation;", SRC I, TXT ";", TGT I
function LogViolation($srcAtom,$tgtAtom)
{	Notifications::addLog("Violation: ($srcAtom,$tgtAtom)", 'VIOLATION');
	return;
}

?>