<?php

error_reporting(E_ALL);
date_default_timezone_set('Europe/London');

require_once '../../../../dbSettings.php';
require_once '../../../Database.php';
require_once './Classes/PHPExcel.php';

define('DEBUG', true);

if (DEBUG)
{
        echo "<pre>";
        echo date('H:i:s') . " Loading from Excel2007 file\n";
}

$file = "CSA_demo.xlsx";
//$file = "/var/www/CyberSA/NW/CSA_demo.xlsx";
$objPHPExcel = PHPExcel_IOFactory::load($file);

if (DEBUG) echo date('H:i:s') . " Loading complete, starting parsing\n";

// Format is as follows:
// (gray bg)    [ <description of data> ], <relation1>,    <relationN>  
//              <srcConcept>,              <tgtConcept1>,  <tgtConceptN>
//              <srcAtomA>,                <tgtAtom1A>,    <tgtAtomNA>
//              <srcAtomB>,                <tgtAtom1B>,    <tgtAtomNB>
//              <srcAtomC>,                <tgtAtom1C>,    <tgtAtomNC>

// Output is function call: 
// InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)



// Loop through all rows
$worksheet = $objPHPExcel->getActiveSheet();
$highestrow = $worksheet->getHighestRow();
$highestcolumn = $worksheet->getHighestColumn();
$highestcolumnnr = PHPExcel_Cell::columnIndexFromString($highestcolumn);

$line = array();

for ($row = 1; $row <= $highestrow; $row++)
{
	// Read this line as an array of values
	$values = array();
	
	for ($columnnr = 0; $columnnr < $highestcolumnnr; $columnnr++)
	{
		$columnletter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
		$content = $objPHPExcel->getActiveSheet()->getCell($columnletter . $row)->getValue();
		// Empty? Then we skip further columns.
		if (empty($content)) { break; } else { $values[] = $content; }
	}

	$line[] = $values;

	// Is this relation table done? Then we parse the current values into function calls and reset it
	// Check if next line starts with a [ or if we're on the last row:
	$nextrowfirstcolumn = $objPHPExcel->getActiveSheet()->getCell('A' . ($row+1))->getValue();
	if (strpos($nextrowfirstcolumn,'[') === 0 OR $row == $highestrow)
	{
		// New relation table!
		ParseLines($line);
		$line = array();
		$values = array();
	}
}

// Format is as follows:
// (gray bg)    [ <description of data> ], <relation1>,    <relationN>  
//              <srcConcept>,              <tgtConcept1>,  <tgtConceptN>
//              <srcAtomA>,                <tgtAtom1A>,    <tgtAtomNA>
//              <srcAtomB>,                <tgtAtom1B>,    <tgtAtomNB>
//              <srcAtomC>,                <tgtAtom1C>,    <tgtAtomNC>

// Output is function call: 
// InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)

function ParseLines($data)
{
	$relation = $concept = $atom = array();

	foreach ($data as $linenr => $values)
	{
		$totalcolumns = count($values);
		if ($linenr == 0)
		{
			// Relations:
			for ($col = 0; $col < $totalcolumns; $col++)
				$relation[$col] = $values[$col];
		} 
		else if ($linenr == 1)
		{
			// Concepts:
			for ($col = 0; $col < $totalcolumns; $col++)
				$concept[$col] = $values[$col];
		}
		else
		{
			// Atoms:
			for ($col = 0; $col < $totalcolumns; $col++)
				$atom[$col] = $values[$col];
			// Now we transform the data info function calls:
			for ($col = 1; $col < $totalcolumns; $col++)
			{
				$bla = "\n" . 'InsPair( RELATION:"'. $relation[$col] . '", SRCCONCEPT:"' . $concept[0] . '", SRCATOM:"' . $atom[0] . '", TGTCONCEPT:"' . $concept[$col] . '", TGTATOM:"' . $atom[$col] . '" );';
				echo $bla . "\n";
				InsPair($relation[$col], $concept[0], $atom[0], $concept[$col], $atom[$col]);
			}
			$atom = array();
		}
	}
}


// Echo memory peak usage
if (DEBUG) echo date('H:i:s') . " Peak memory usage: " . (memory_get_peak_usage(true) / 1024 / 1024) . " MB\r\n";


?>
