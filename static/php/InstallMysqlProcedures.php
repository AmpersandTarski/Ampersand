<?php
/* Please forward any comments to the author: michiel.stornebrink@tno.nl
This file loops over the rules in Generics.php, filters out the rules that are to become stored procedures, and creates the stored procedures. Any previously existing stored procedures of the same name are overwritten.
Not all rules can be transformed into stored procedures - please see the documentation in the code below for the constraints.
*/
set_time_limit(60);
error_reporting(E_ALL ^ E_NOTICE);
ini_set("display_errors", 1);

// require __DIR__.'/../Generics.php'; // wordt al geladen, alleen nodig voor stand-alone executie
require_once 'Database.php'; 
require_once 'DatabaseUtils.php';
require_once 'loadplugins.php';

global $allRoles;
global $allRulesSql;
global $dbName;
global $storedProcedureCreated;

// $link = mysqli_connect('localhost', 'root', '', $dbName); // mysqli needed because of multiple statements execution support
$link = mysqli_connect($DB_host, $DB_user, $DB_pass, $dbName); // mysqli needed because of multiple statements execution support


// loop over all roles
for ($r = 0; $r < count($allRoles); $r++)
{
	// only handle role 'DATABASE'
	if($allRoles[$r]['name'] == 'DATABASE')
	{
		// foreach rule that is assigned to 'DATABASE'
		foreach($allRoles[$r]['ruleNames'] as $ruleName)
		{
			$storedProcedureCreated = false; // if stored procedure remains uncreated, we MUST warn the developer/user!!
			procDBGecho("RuleName: $ruleName");
			procDBGecho("--Rule: ".$allRulesSql[$ruleName]['ruleAdl']);
			
			// if "|-" exists in ADL expression
			if(strpos($allRulesSql[$ruleName]['ruleAdl'], "|-") !== false)
			{
				$symbol = "|-";
				procDBGecho("--Rule contains '|-'");
				// Break up rule into expressions
				$expressions = explode("|-", $allRulesSql[$ruleName]['ruleAdl']);
				
			// elseif "-|" exists in ADL expression
			}elseif(strpos($allRulesSql[$ruleName]['ruleAdl'], "-|") !== false)
			{
				$symbol = "-|";
				procDBGecho("--Rule contains '-|'");
				// Break up rule into expressions
				$expressions = explode("-|", $allRulesSql[$ruleName]['ruleAdl']);
				
			}else
			{
				procDBGecho("--Rule contains no |- or -|. No stored procedure created");
				echo("</br>--ERROR-- ROLE DATABASE MAINTAINS $rulename does not lead to a stored procedure.</br>");
				die;
// Om de zaak te redden kunnen we deze regel toekennen aan de rol 'ExecEngine' i.p.v. aan DATABASE
// We kunnen overigens NIET het omgekeerde doen (alle ExecEngine regels aan DATABASE toekennen)
				$expressions = array(); // empty arry
			}
				
			foreach($expressions as $key => $expression)
			{
				$expression = trim($expression);
				procDBGecho("* Expression: $expression");
				
				// check if $expression starts with '-' (e.g. -bfGebruiktARM20_DefProp)
				if(substr($expression, 0, 1) == "-") {
					$expressionIsNegative = true;
					procDBGecho(" --Expression is negative");
					$expression = substr($expression, 1); // remove '-' before expression
				}else{
					$expressionIsNegative = false;
					procDBGecho(" --Expression is <u>non</u> negative");
				}
				
				// check if $expression is flipped (e.g. relation~)
				if(substr($expression, -1) == "~"){
					$expressionIsFlipped = true;
					procDBGecho(" --Expression is flipped");
					$expression = substr($expression, 0, -1); // remove '~' after expression
				}else{
					$expressionIsFlipped = false;
					procDBGecho(" --Expression is <u>not</u> flipped");
				}
				
				if(array_key_exists($expression, $relationTableInfo))
				{
					procDBGecho(" --Expression exists in relationTableInfo as $expression");
					$relation = $expression; // $expression exists in $relationTableInfo, so we call it a $relation
					
					// if $expressionIsFlipped provide ERROR and stop script
					if($expressionIsFlipped)
					{
						echo " --<b>ERROR: Relation must not be flipped</b>";
						die;
					}											
					/* Decision table for deciding whether it must be an 'delpair' or 'inspair' procedure
						|************|********|***********|*********************|**********|
						|			         |    			 | Negative 	| Relation left/right	|		      		|
						|     	     	| Symbol	| relation 	|	of symbol          	|	Outcome		|
						|************|********|***********|*********************|**********|
						|  r  |- exp	|	  |- 	 |   false 		|  left  ($key == 0)		| delpair		|
						| exp |-  r 	|  	|-	  |   false 		|  right ($key == 1)		| inspair		|
						| -r  |- exp	|  	|-	  |   true  		|  left  ($key == 0)		| inspair		|
						| exp |- -r	 |  	|-	 	|   true	  	|  right ($key == 1)		| delpair		|
						|  r  -| exp	|	  -|	 	|   false 		|  left  ($key == 0)		| inspair		|
						| exp -|  r 	|  	-|	 	|   false 		|  right ($key == 1)		| delpair		|
						| -r  -| exp	|	  -|	 	|   true	 	 |  left  ($key == 0)		| delpair		|
						| exp -| -r	 |	  -|	 	|   true	 	 |  right ($key == 1)		| inspair		|
						|************|********|***********|*********************|**********|
					*/
					switch($symbol)
					{
						case "|-":
							switch($expressionIsNegative)
							{
								case false:
									switch ($key)
									{
										case 0:
											// delpair
											$query = makeDelpairSqlProcedure($ruleName, $relation);
											break;
										case 1:
											// inspair
											$query = makeInspairSqlProcedure($ruleName, $relation);
											break;
									}
									break;
								case true:
									switch ($key)
									{
										case 0:
											// inspair
											$query = makeInspairSqlProcedure($ruleName, $relation);
											break;
										case 1:
											// delpair
											$query = makeDelpairSqlProcedure($ruleName, $relation);
											break;
									}
									break;
							}
							break;
						case "-|":
							switch($expressionIsNegative)
							{
								case false:
									switch ($key)
									{
										case 0:
											// inspair
											$query = makeInspairSqlProcedure($ruleName, $relation);
											break;
										case 1:
											// delpair
											$query = makeDelpairSqlProcedure($ruleName, $relation);
											break;
									}
									break;
								case true:
									switch ($key)
									{
										case 0:
											// delpair
											$query = makeDelpairSqlProcedure($ruleName, $relation);
											break;
										case 1:
											// inspair
											$query = makeInspairSqlProcedure($ruleName, $relation);
											break;
									}
									break;
							}
							break;					
					}
										
					// query uitvoeren
					mysqli_query($link, "DROP PROCEDURE IF EXISTS ".escapeSQL(makeDBFunctionName($allRulesSql[$ruleName]['name'])));
					mysqli_query($link, $query); 
					echo(mysqli_error($link));
					
					// reporting
//			procDBGecho($query);
					procDBGecho(" --<b>Stored procedure aangemaakt voor $ruleName</b>");
				}else
				{
					procDBGecho(" --Expression not in relationTableInfo</br>");
				}
			}
				
   if (!$storedProcedureCreated)  // Since stored procedure is uncreated, we MUST warn the developer/user!!
   { 
     echo("Rule $ruleName could not be created as a stored procedure!");
     die;
   }
			procDBGecho("<hr/>");
		}
		
	}
}

// AllProcedures PROCEDURE aanmaken
$query = makeAllProceduresProcedure(); // Dit werkt niet als er geen stored procedures zijn
mysqli_query($link, "DROP PROCEDURE IF EXISTS AllProcedures");
mysqli_query($link, $query); 
procDBGecho("InstallMysqlProcdures - make all procedures". mysqli_error($link));
procDBGecho($query . "");
procDBGecho("AllProcedures PROCEDURE aangemaakt</br/>");

function makeInspairSqlProcedure($ruleName, $relation){
	global $allRulesSql;
	global $relationTableInfo;
	global $tableColumnInfo;
	global $procedures;
	global $storedProcedureCreated;
	
	// get table, srcCol and tgtCol for $relation
	$table = $relationTableInfo[$relation]['table'];
	$srcCol = $relationTableInfo[$relation]['srcCol'];
	$tgtCol = $relationTableInfo[$relation]['tgtCol'];
	
	// get table column properties for $srcCol and $tgtCol
	$srcColUnique = $tableColumnInfo[$table][$srcCol]['unique'];
	$tgtColUnique = $tableColumnInfo[$table][$tgtCol]['unique'];
	
	// SQL escape table and column names
	$tableEsc = escapeSQL($table);
	$srcColEsc = escapeSQL($srcCol);
	$tgtColEsc = escapeSQL($tgtCol);
	$srcAtomEsc = 'srcAtom'; // This is correct! without $, because 'srcAtom' is the column name of query result in the procedure
	$tgtAtomEsc = 'tgtAtom'; // This is correct! without $, because 'tgtAtom' is the column name of query result in the procedure
	
	// build database query
	if($srcColUnique || $tgtColUnique) // srcCol, tgtCol or both are unique ==> update query
	{
		if($srcColUnique){
			$query = "UPDATE `$tableEsc` SET `$srcColEsc`= $srcAtomEsc, `$tgtColEsc`= $tgtAtomEsc WHERE `$srcColEsc`= $srcAtomEsc";
		}else{
			$query = "UPDATE `$tableEsc` SET `$srcColEsc`= $srcAtomEsc, `$tgtColEsc`= $tgtAtomEsc WHERE `$tgtColEsc`= $tgtAtomEsc";
		}
	}else{ // neither srcCol nor tgtCol is unique ==> insert query
		$query = "INSERT INTO `$tableEsc` (`$srcColEsc`, `$tgtColEsc`) VALUES ($srcAtomEsc, $tgtAtomEsc)";
	}
	
	// build procedure
	$query = "CREATE PROCEDURE ".escapeSQL(makeDBFunctionName($allRulesSql[$ruleName]['name']))."() 
			BEGIN 
			DECLARE done INT DEFAULT 0; 
			DECLARE srcAtom varchar(4000); 
			DECLARE tgtAtom varchar(4000); 
			DECLARE cur_1 CURSOR FOR ".$allRulesSql[$ruleName]['violationsSQL']."; 
			DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = 1; 
			OPEN cur_1; 
			REPEAT 
				FETCH cur_1 INTO srcAtom, tgtAtom; 
				IF NOT done THEN 
					$query;
				END IF; 
			UNTIL done END REPEAT; 
			CLOSE cur_1; 
			END "; 
	
	procDBGecho(" --Inspair PROCEDURE aanmaken");
	addProcedureToAllProcedures(makeDBFunctionName($allRulesSql[$ruleName]['name'])); // add this procedure to AllProcedures in order to call all procedures at once
	$storedProcedureCreated = TRUE;

	return $query;
}

function makeDelpairSqlProcedure($ruleName, $relation){
	global $allRulesSql;
	global $relationTableInfo;
	global $tableColumnInfo;
	global $procedures;
	global $storedProcedureCreated;
	
	// get table, srcCol and tgtCol for $relation
	$table = $relationTableInfo[$relation]['table'];
	$srcCol = $relationTableInfo[$relation]['srcCol'];
	$tgtCol = $relationTableInfo[$relation]['tgtCol'];
	
	// get table column properties for $srcCol and $tgtCol
	$srcColNull = $tableColumnInfo[$table][$srcCol]['null'];
	$tgtColNull = $tableColumnInfo[$table][$tgtCol]['null'];
	
	// SQL escape table and column names
	$tableEsc = escapeSQL($table);
	$srcColEsc = escapeSQL($srcCol);
	$tgtColEsc = escapeSQL($tgtCol);
	
	// build database query
	if($srcColNull xor $tgtColNull) // srcCol xor tgtCol can be null ==> update query
	{
		if($srcColNull){
			$query = "UPDATE `$tableEsc` SET `$srcColEsc`= NULL WHERE `$srcColEsc`= srcAtom AND `$tgtColEsc`= tgtAtom";
		}else{
			$query = "UPDATE `$tableEsc` SET `$tgtColEsc`= NULL WHERE `$srcColEsc`= srcAtom AND `$tgtColEsc`= tgtAtom";
		}
	}elseif($srcColNull and $tgtColNull) // both srcCol and tgtCol can be null ==> delete query 	-- REMARK: maybe this should be an update instead of delete query
	{
		$query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`= srcAtom AND `$tgtColEsc`= tgtAtom";
	}else{ // neither srcCol nor tgtCol can be null ==> delete query
		$query = "DELETE FROM `$tableEsc` WHERE `$srcColEsc`= srcAtom AND `$tgtColEsc`= tgtAtom";
	}
	// build procedure
	$sql = "CREATE PROCEDURE ".escapeSQL(makeDBFunctionName($allRulesSql[$ruleName]['name']))."() 
			BEGIN 
			DECLARE done INT DEFAULT 0; 
			DECLARE srcAtom varchar(4000); 
			DECLARE tgtAtom varchar(4000); 
			DECLARE cur_1 CURSOR FOR ".$allRulesSql[$ruleName]['violationsSQL']."; 
			DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = 1; 
			OPEN cur_1; 
			REPEAT 
				FETCH cur_1 INTO srcAtom, tgtAtom; 
				IF NOT done THEN 
					$query;
				END IF; 
			UNTIL done END REPEAT; 
			CLOSE cur_1; 
			END";
			
	procDBGecho(" --Delpair PROCEDURE aanmaken");
	addProcedureToAllProcedures(makeDBFunctionName($allRulesSql[$ruleName]['name'])); // add this procedure to AllProcedures in order to call all procedures at once
	$storedProcedureCreated = TRUE;
	
	return $sql;						
}

// add $procedureName to AllProcedures in order to call all procedures at once
function addProcedureToAllProcedures($procedureName){
	global $procedures;
	$procedures[] = escapeSQL($procedureName);
}

// build procedure AllProcedures()
function makeAllProceduresProcedure(){
	global $procedures;
	
	$query = "CREATE PROCEDURE AllProcedures() BEGIN ";
	foreach ((array)$procedures as $procedure)
	{
		$query .= "CALL $procedure;";
	}
	$query .= "END ";
	procDBGecho("AllProcedures PROCEDURE aanmaken");
	return $query;
}


function procDBGecho ($text)
{ 
  //echo(" ".$text."<br/>");
  
} 

function makeDBFunctionName($functionName){
 $search = array(' ', '-');
 return str_replace($search, '_', $functionName);
}

?>
