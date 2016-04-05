<?php 
/* This file defines a limited number of functions that deal with dates and times. They include functions for comparing dates and times (equal, less than etc.), for setting today's date, and more. This file may be extended, but only with functions that can be used generically. 

The date and time formats that can be used are pretty much arbitrary. A precise description is given at:
   http://www.php.net/manual/en/datetime.formats.date.php
   http://www.php.net/manual/en/datetime.formats.time.php
*/

use Ampersand\Log\Logger; 

/* sessionToday :: SESSION * Date -- or whatever the DateTime concept is called
   ROLE ExecEngine MAINTAINS "Initialize today's date"
   RULE "Initialize today's date": I[SESSION] |- sessionToday;sessionToday~
   VIOLATION (TXT "{EX} SetToday;sessionToday;SESSION;", SRC I, TXT ";Date;", TGT sessionToday)
   
   For $formatSpec see http://php.net/manual/en/function.date.php
   Default is 'd-m-Y' -> e.g: "01-01-2015", other examples include time, like 'd-m-Y G:i:s' -> e.g.: "01-01-2015 1:00:00"
*/
function SetToday($relation,$srcConcept,$srcAtom,$dateConcept,$formatSpec='d-m-Y'){
	Logger::getLogger('EXECENGINE')->debug("SetToday($relation,$srcConcept,$srcAtom,$dateConcept,$formatSpec)");
	$curdate = date($formatSpec);
	InsPair($relation,$srcConcept,$srcAtom,$dateConcept,$curdate);
	
	Logger::getLogger('EXECENGINE')->debug("Today's date set to {$curdate}");
}


// VIOLATION (TXT "{EX} datimeStdFormat;standardizeDateTime;DateTime;", SRC I, TXT ";DateTimeStdFormat;", TGT I)
function datimeStdFormat($relation,$DateConcept,$srcAtom,$StdFormatConcept,$formatSpec){
	Logger::getLogger('EXECENGINE')->debug("datimeStdFormat($relation,$DateConcept,$srcAtom,$StdFormatConcept,$formatSpec)");
	$date = new DateTime($srcAtom);
	InsPair($relation,$DateConcept,$srcAtom,$StdFormatConcept,$date->format($formatSpec));
	
	Logger::getLogger('EXECENGINE')->debug("Date format $srcAtom changed to {$date->format($formatSpec)}");
}


/* (Example taken from EURent):
VIOLATION (TXT "{EX} DateDifferencePlusOne" -- Result = 1 + MAX(0, (RentalEndDate - RentalStartDate))
               , TXT ";computedRentalPeriod;DateDifferencePlusOne;", SRC I, TXT ";Integer"
               , TXT ";", SRC earliestDate -- = Rental start date
               , TXT ";", SRC latestDate   -- = Rental end date
          )
*/
function DateDifferencePlusOne($relation,$srcConcept,$srcAtom,$integerConcept,$earliestDate,$latestDate){
	Logger::getLogger('EXECENGINE')->debug("DateDifferencePlusOne($relation,$srcConcept,$srcAtom,$integerConcept,$earliestDate,$latestDate)");
	$datediff = strtotime($latestDate) - strtotime($earliestDate);
	if ($datediff < 0) Logger::getUserLogger()->error("DateDifferencePlusOne: first arg (earliestDate) must be smaller than second arg (latestDate).");
	
	$result = 1 + max(0, floor($datediff/(60*60*24)));
	InsPair($relation,$srcConcept,$srcAtom,$integerConcept,$result);
	
	Logger::getLogger('EXECENGINE')->debug("Date difference + 1 calculated for $latestDate - $earliestDate");
}


/* (Example taken from EURent):
VIOLATION (TXT "{EX} DateDifference"
               , TXT ";compExcessPeriod;DateDifference;", SRC I, TXT ";Integer"
               , TXT ";", SRC firstDate
               , TXT ";", SRC lastDate
          )
*/
function DateDifference($relation,$srcConcept,$srcAtom,$integerConcept,$firstDate,$lastDate){
	Logger::getLogger('EXECENGINE')->debug("DateDifference($relation,$srcConcept,$srcAtom,$integerConcept,$firstDate,$lastDate)");
	$datediff = strtotime($lastDate) - strtotime($firstDate);
	if ($datediff < 0) Logger::getUserLogger()->error("DateDifference: first arg (earliestDate) must be smaller than second arg (latestDate).");
	
	$result = max(0, floor($datediff/(60*60*24)));
	InsPair($relation,$srcConcept,$srcAtom,$integerConcept,$result);
	
	Logger::getLogger('EXECENGINE')->debug("Date difference calculated for $lastDate - $firstDate");
}


/* COMPARING DATES AND TIMES (used e.g. in Arbeidsduur)
   Whenever you need to compare dates/times, you must define your own relation(s) for that, and consequently also a concept for the date/time.
   The functions provided in this file allow you to fill such relations.

>> EXAMPLES OF USE:
   stdDateTime :: DateTime * DateTimeStdFormat [UNI] PRAGMA "Standard output format for " " is " 

   ROLE ExecEngine MAINTAINS "compute DateTime std values"
   RULE "compute DateTime std values": I[DateTime] |- stdDateTime;stdDateTime~
   VIOLATION (TXT "{EX} datimeStdFormat;stdDateTime;DateTime;", SRC I, TXT ";DateTimeStdFormat;Y-m-d") -- The text 'Y-m-d' may be replaced by any other format specification, see the 'Parameters' section on http://www.php.net/manual/en/function.date.php

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
   If you use many atoms in DateTime, this will take increasingly more time
   to check for violations. So do not use that many...
*/
// VIOLATION (TXT "{EX} datimeEQL;DateTime;" SRC I, TXT ";", TGT I)
function datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom){
	Logger::getLogger('EXECENGINE')->debug("datimeEQL($eqlRelation,$DateConcept,$srcAtom,$tgtAtom)");
	if (($dt1 = strtotime($srcAtom)) === false) Logger::getUserLogger()->error("datimeEQL: Illegal date $dt1 specified in srcAtom (3rd arg): $srcAtom");
	if (($dt2 = strtotime($tgtAtom)) === false) Logger::getUserLogger()->error("datimeEQL: Illegal date $dt2 specified in tgtAtom (4th arg): $tgtAtom");
	
	if ($dt1 == $dt2){
		InsPair($eqlRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
		
		// Accommodate for different representations of the same time:
		if ($srcAtom != $tgtAtom){
			InsPair($eqlRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
		}
	}
}


// VIOLATION (TXT "{EX} datimeNEQ;DateTime;" SRC I, TXT ";", TGT I)
function datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom){ 	
	Logger::getLogger('EXECENGINE')->debug("datimeNEQ($neqRelation,$DateConcept,$srcAtom,$tgtAtom)");
	if (($dt1 = strtotime($srcAtom)) === false) Logger::getUserLogger()->error("datimeNEQ: Illegal date $dt1 specified in srcAtom (3rd arg): $srcAtom");
	if (($dt2 = strtotime($tgtAtom)) === false) Logger::getUserLogger()->error("datimeNEQ: Illegal date $dt2 specified in tgtAtom (4th arg): $tgtAtom");
	
	if ($dt1 != $dt2){
		InsPair($neqRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
		InsPair($neqRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
	}
}


// VIOLATION (TXT "{EX} datimeLT;DateTime;" SRC I, TXT ";", TGT I)
function datimeLT($ltRelation,$DateConcept,$srcAtom,$tgtAtom){
	Logger::getLogger('EXECENGINE')->debug("datimeLT($ltRelation,$DateConcept,$srcAtom,$tgtAtom)");
	if (($dt1 = strtotime($srcAtom)) === false) Logger::getUserLogger()->error("datimeLT: Illegal date $dt1 specified in srcAtom (3rd arg): $srcAtom");
	if (($dt2 = strtotime($tgtAtom)) === false) Logger::getUserLogger()->error("datimeLT: Illegal date $dt2 specified in tgtAtom (4th arg): $tgtAtom");
	if ($dt1 == $dt2) return;
	
	if ($dt1 < $dt2){
		InsPair($ltRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
	}else{
		InsPair($ltRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
	}
}


// VIOLATION (TXT "{EX} datimeGT;DateTime;" SRC I, TXT ";", TGT I)
function datimeGT($gtRelation,$DateConcept,$srcAtom,$tgtAtom){
	Logger::getLogger('EXECENGINE')->debug("datimeGT($gtRelation,$DateConcept,$srcAtom,$tgtAtom)");
	if (($dt1 = strtotime($srcAtom)) === false) Logger::getUserLogger()->error("datimeGT: Illegal date $dt1 specified in srcAtom (3rd arg): $srcAtom");
	if (($dt2 = strtotime($tgtAtom)) === false) Logger::getUserLogger()->error("datimeGT: Illegal date $dt2 specified in tgtAtom (4th arg): $tgtAtom");
	if ($dt1 == $dt2) return;
	
	if ($dt1 > $dt2){
		InsPair($gtRelation,$DateConcept,$srcAtom,$DateConcept,$tgtAtom);
	}else{
		InsPair($gtRelation,$DateConcept,$tgtAtom,$DateConcept,$srcAtom);
	}
}

?>