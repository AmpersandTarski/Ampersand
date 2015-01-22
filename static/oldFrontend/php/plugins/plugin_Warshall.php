<?php 
/* This file defines the function 'TransitiveClosure', that computes the transitive closure of a relation using Washall's algorithm.
   There are no guarantees with respect to its 100% functioning. Have fun...

   Suppose you need the transitive closure r* of a relation r :: C * C
   This pattern tells you how to define a relation rStar that contains the same population as r*
   (which you need, but isn't provided in the prototype generator, so this is the workaround).
   Maintaining the population of rStar correctly is not trivial, particularly when r is depopulated.
   The easiest way around this is to compute rStar from scratch.
   However, you then need to know that r is being (de)populated, so we need a copy of r.
   This leads to the following pattern:
   
   relation :: Concept*Concept
   relationCopy :: Concept*Concept -- copied value of 'relation' allows for detecting modifcation events
   relationStar :: Concept*Concept -- transitive closure of relation
   
   ROLE ExecEngine MAINTAINS "relationCompTransitiveClosure"
   RULE "relationCompTransitiveClosure": relation = relationCopy
   VIOLATION (TXT "{EX} TransitiveClosure;relation;Concept;relationCopy;relationStar")

   NOTES:
   1) The above example is made for ease of use. This is what you need to do:
      a) copy and paste the above example into your own ADL script;
      b) replace the names of 'relation' and 'Concept' (cases sensitive, also as part of a word) with what you need
      c) make sure you define an INTERFACE that contains both 'relationCopy' and 'relationStar'
         (this is necessary for interfacing with such relations using PHP).
   2) Of course, there are all sorts of alternative ways in which 'TransitiveClosure' can be used.
   3) There are ways to optimize the below code, e.g. by splitting the function into an 'InsTransitiveClosure'
      and a 'DelTransitiveClosure'
*/

function TransitiveClosure($r,$C,$rCopy,$rStar)
{ ExecEngineWhispers("Exeucte TransitiveClosure($r,$C,$rCopy,$rStar)");
  global $violationID; // 'array-index' of violations that are being processed; 
  if ($violationID > 1) // If we are not dealing with the first violation of a rule:
  { ExecEngineWhispers("Skipping TransitiveClosure($r,$C,$rCopy,$rStar)");
    return;  // this is the case if we have executed this function already in this transaction
  }
  
  // Compute transitive closure following Warshall's algorithm
  $closure = RetrievePopulation($r, $C); // get adjacency matrix
  OverwritePopulation($closure, $rCopy, $C); // store it in the 'rCopy' relation  

  // Get all unique atoms from this population
  $atoms = array_keys($closure); // 'Src' (left) atoms of pairs in $closure 
  foreach ($closure as $tgtAtomsList) // Loop to add 'Tgt' atoms that not yet exist
  { $tgtAtoms = array_keys($tgtAtomsList);
    foreach ($tgtAtoms as $tgtAtom)
    { if (!in_array($tgtAtom, $atoms)) 
      {  $atoms[] = $tgtAtom;
  } } }

  foreach ($atoms as $k)
  { foreach ($atoms as $i)
    { if ($closure[$i][$k])
      { foreach ($atoms as $j)
         { $closure[$i][$j] = $closure[$i][$j] || $closure[$k][$j];
   } } } }

  OverwritePopulation($closure, $rStar, $C);
}

function RetrievePopulation($relation, $concept)
{ global $relationTableInfo; // For comments on this function see plugin_InsDelPairAtom.php
  global $tableColumnInfo;

  $found = false; // check if $relation appears in $relationTableInfo
  foreach($relationTableInfo as $key => $arr)
    {   if($key == $relation && $arr['srcConcept'] == $concept && $arr['tgtConcept'] == $concept)
        { $found = true;
          $table = $arr['table'];
            $srcCol = $arr['srcCol'];
            $tgtCol = $arr['tgtCol'];
        }
    }
    if (!$found)
    { // Errors in ADL script may corrupt the database, so we die (leaving a suicide note)
      ExecEngineSHOUTS("ERROR in RetrievePopulation: Cannot find $relation\[$concept\*$concept\] signature.");
      ExecEngineSays("If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration.");
      die;
    }

  $tableEsc = escapeSQL($table);
  $query = "SELECT * FROM $tableEsc";
  $result = mysql_query($query);
  if (!$result) die ('Error: '.mysql_error());
  while($row = mysql_fetch_array($result, MYSQL_NUM))
  { $array[$row[0]][$row[1]] = true;
  }
  return (array) $array;
}

// Overwrite contents of &-relation $r with contents of php array $rArray
function OverwritePopulation($rArray, $relation, $concept)
{ global $relationTableInfo; // For comments on this function see plugin_InsDelPairAtom.php
  global $tableColumnInfo;

  $found = false; // check if $relation appears in $relationTableInfo
  foreach($relationTableInfo as $key => $arr)
    {   if($key == $relation && $arr['srcConcept'] == $concept && $arr['tgtConcept'] == $concept)
        { $found = true;
          $table = $arr['table'];
            $srcCol = $arr['srcCol'];
            $tgtCol = $arr['tgtCol'];
        }
    }
    if (!$found)
    { // Errors in ADL script may corrupt the database, so we die (leaving a suicide note)
      ExecEngineSHOUTS("ERROR in OverwritePopulation: Cannot find $relation\[$concept\*$concept\] signature.");
      ExecEngineSays("If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration.");
      die;
    }

  $tableEsc = escapeSQL($table);
  $srcColEsc = escapeSQL($srcCol);
  $tgtColEsc = escapeSQL($tgtCol);
  
  $query = "TRUNCATE TABLE $tableEsc";
  DB_doquer($query);
  
  foreach($rArray as $src => $tgtArray)
  { foreach($tgtArray as $tgt => $bool)
    { if($bool)
      { $query = "INSERT INTO $tableEsc (`$srcColEsc`, `$tgtColEsc`) VALUES ('$src','$tgt')";
        DB_doquer($query);
} } } } 

?>