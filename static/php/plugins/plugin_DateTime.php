<?php 
/* This file defines a limited number of functions for comparing dates and times.
   Whenever you need to compare dates/times, you must define your own relation(s)
   to do that, and consequently also a concept for the date/time.
   The functions provided in this file allow you to fill such relations.
   
   You can use almost arbitrary date/time formats; a precise description is given at:
   http://www.php.net/manual/en/datetime.formats.date.php
   http://www.php.net/manual/en/datetime.formats.time.php

>> EXAMPLES OF USE:
   
   stdDateTime :: DateTime * DateTimeStdFormat [UNI] PRAGMA "Standard output format for " " is " 

   ROLE ExecEngine MAINTAINS "compute DateTime std values"
   RULE "compute DateTime std values": I[DateTime] |- stdDateTime;stdDateTime~
   VIOLATION (TXT "{EX} datimeStdFormat;stdDateTime;DateTime;", SRC I, TXT ";DateTimeStdFormat;Y-m-d")

   eqlDateTime :: DateTime * DateTime PRAGMA "" " occurred simultaneously "
   neqDateTime :: DateTime * DateTime PRAGMA "" " occurred either before or after "
    ltDateTime :: DateTime * DateTime PRAGMA "" " has occurred before "
    gtDateTime :: DateTime * DateTime PRAGMA "" " has occurred after "
   
   ROLE ExecEngine MAINTAINS "compute DateTime comparison relations"
   RULE "compute DateTime comparison relations": V[DateTime] |- eqlDateTime \/ neqDateTime
   VIOLATION (TXT "{EX} datimeEQL;eqlDateTime;DateTime;", SRC I, TXT ";", TGT I
             ,TXT "{EX} datimeNEQ;neqDateTime;DateTime;", SRC I, TXT ";", TGT I
             ,TXT "{EX} datimeLT;ltDateTime;DateTime;", SRC I, TXT ";", TGT I
             ,TXT "{EX} datimeGT;gtDateTime;DateTime;", SRC I, TXT ";", TGT I
             )
            
>> LIMITATIONS OF USE:
   Note that if you use many atoms in DateTime, this will take increasingly more time
   to check for violations. So do not use that many...
*/

// VIOLATION (TXT "{EX} datimeStdFormat;standardizeDateTime;DateTime;", SRC I, TXT ";DateTimeStdFormat;", TGT I)
function datimeStdFormat($stdFormatRelation,$DateConcept,$srcAtom,$StdFormatConcept,$formatSpec)
{ 	emitLog("datimeStdFormat($stdFormatRelation,$DateConcept,$srcAtom,$StdFormatConcept,$formatSpec)");
   $date = new DateTime($srcAtom);
   InsPair($stdFormatRelation,$DateConcept,$srcAtom,$StdFormatConcept,$date->format($formatSpec));
   return;
}

// VIOLATION (TXT "{EX} datimeEQL;DateTime;" SRC I, TXT ";", TGT I)
function datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom)
{ 	emitLog("datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom)");
   $dt1 = strtotime($srcAtom);
   $dt2 = strtotime($tgtAtom);
   if ($dt1 == $dt2) 
   { InsPair($eqlRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
// Accommodate for different representations of the same time:
     if ($srcAtom != $tgtAtom)
     { InsPair($eqlRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
   } }
   return;
}

// VIOLATION (TXT "{EX} datimeNEQ;DateTime;" SRC I, TXT ";", TGT I)
function datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom)
{ 	emitLog("datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom)");
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
{ 	emitLog("datimeLT($ltRelation,$DateConcept,$srcAtom,$tgtAtom)");
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
{ 	emitLog("datimeGT($gtRelation,$DateConcept,$srcAtom,$tgtAtom)");
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