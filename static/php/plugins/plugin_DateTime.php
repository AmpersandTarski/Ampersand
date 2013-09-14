<?php 
/* This file defines a limited number of functions for comparing dates and times.
   Whenever you need to compare dates/times, you must define your own relation(s)
   to do that, and consequently also a concept for the date/time.
   The functions provided in this file allow you to fill such relations.
   
   You can use almost arbitrary date/time formats; a precise description is given at:
   http://www.php.net/manual/en/datetime.formats.date.php
   http://www.php.net/manual/en/datetime.formats.time.php

>> EXAMPLE OF USE:
   -- First, we define the relations for comparing dates/times that we need, e.g.:
   
    eqlDateTime :: DateTime * DateTime PRAGMA "" " occurred simultaneously "
    neqDateTime :: DateTime * DateTime PRAGMA "" " occurred either before or after "
     ltDateTime :: DateTime * DateTime PRAGMA "" " has occurred before "
     gtDateTime :: DateTime * DateTime PRAGMA "" " has occurred after "
   
   -- Then, we populate these relations automatically using the following RULE:
   ROLE DATABASE {-ExecEngine-} MAINTAINS "insltDateTime"
   RULE "insltDateTime": V[DateTime] |- eqlDateTime \/ neqDateTime
   VIOLATION (TXT "{EX} datimeEQL;DateTime;", SRC I, TXT ";", TGT I
             ,TXT "{EX} datimeNEQ;DateTime;", SRC I, TXT ";", TGT I
             ,TXT "{EX} datimeLT;DateTime;", SRC I, TXT ";", TGT I
             ,TXT "{EX} datimeGT;DateTime;", SRC I, TXT ";", TGT I
             )
             
>> LIMITATIONS OF USE:
   Stored procedures such as in the example (i.e. assigning the violation to DATABASE)
   only look for violations after a transaction is completed (Edit - Save). Assigning
   the rule to 'ExecEngine' also checks for violations when the initial screen is setup.
   Note that if you use many atoms in DateTime, this will take increasingly more time
   to check for violations. So do not use that many...
*/

// VIOLATION (TXT "{EX} datimeEQL;DateTime;" SRC I, TXT ";", TGT I)
function datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom)
{ 	emitAmpersandExecEngine("datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom)");
  	emitLog("datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom)");
   $dt1 = strtotime($srcAtom);
   $dt2 = strtotime($tgtAtom);
   if ($dt1 == $dt2) 
   { InsPair($neqRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
// Accommodate for different representations of the same time:
     if ($srcAtom != $tgtAtom)
       InsPair($neqRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
   }
   return;
}

// VIOLATION (TXT "{EX} datimeNEQ;DateTime;" SRC I, TXT ";", TGT I)
function datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom)
{ 	emitAmpersandExecEngine("datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom)");
  	emitLog("datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom)");
   $dt1 = strtotime($srcAtom);
   $dt2 = strtotime($tgtAtom);
   if ($dt1 != $dt2) 
   { InsPair($neqRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
     InsPair($neqRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
   }
   return;
}

// VIOLATION (TXT "{EX} datimeLT;DateTime;" SRC I, TXT ";", TGT I)
function datimeLT($ltRelation,$DateConcept,$srcAtom,$tgtAtom)
{ 	emitAmpersandExecEngine("datimeLT($ltRelation,$DateConcept,$srcAtom,$tgtAtom)");
  	emitLog("datimeLT($ltRelation,$DateConcept,$srcAtom,$tgtAtom)");
   $dt1 = strtotime($srcAtom);
   $dt2 = strtotime($tgtAtom);
   if ($dt1 == $dt2) return;
   if ($dt1 < $dt2)
   { InsPair($ltRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
   } else
   { InsPair($ltRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
   }
   return;
}

// VIOLATION (TXT "{EX} datimeGT;DateTime;" SRC I, TXT ";", TGT I)
function datimeGT($gtRelation,$DateConcept,$srcAtom,$tgtAtom)
{ 	emitAmpersandExecEngine("datimeGT($gtRelation,$DateConcept,$srcAtom,$tgtAtom)");
  	emitLog("datimeGT($gtRelation,$DateConcept,$srcAtom,$tgtAtom)");
   $dt1 = strtotime($srcAtom);
   $dt2 = strtotime($tgtAtom);
   if ($dt1 == $dt2) return;
   if ($dt1 > $dt2)
   { InsPair($gtRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
   } else
   { InsPair($gtRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
   }
   return;
}

?>