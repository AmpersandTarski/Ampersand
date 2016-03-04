<?php
require_once (__DIR__ . '/lib/Classes/PHPExcel.php');

// UI
$GLOBALS['navBar']['appMenu'][] = array ( 'url' => 'extensions/ExcelImport/ui/views/MenuItem.html');
AngularApp::addCSS('extensions/ExcelImport/ui/css/style.css');
AngularApp::addJS('extensions/ExcelImport/ui/js/ExcelImport.js');

// Config
Config::set('allowedMimeTypes', 'excelImport', array('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'application/excel'));

class ImportExcel {
	public $file;
	private $db;

	function __construct($filename){
		$this->file = $filename;
		$this->db = Database::singleton();
	}

	public function ParseFile(){
		
		Notifications::addLog('------------------------- EXCEL IMPORT STARTED -------------------------', 'ExcelImport');
				
		$this->ProcessFileContent();
		
		Notifications::addLog('------------------------- END OF EXCEL IMPORT -------------------------', 'ExcelImport');
		
		// Close transaction => ROLLBACK or COMMIT.
		$this->db->closeTransaction('File uploaded', false, true, false);
		
		return Notifications::getAll();
		
	}
	
	public function ProcessFileContent(){
		$objPHPExcel = PHPExcel_IOFactory::load($this->file);

		// Format is as follows:
		// (gray bg)    [ <description of data> ], <relation1>,    <relationN>  
		//              <srcConcept>,              <tgtConcept1>,  <tgtConceptN>
		//              <srcAtomA>,                <tgtAtom1A>,    <tgtAtomNA>
		//              <srcAtomB>,                <tgtAtom1B>,    <tgtAtomNB>
		//              <srcAtomC>,                <tgtAtom1C>,    <tgtAtomNC>

		// Output is function call: 
		// InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)

		// Loop over all worksheets
		foreach($objPHPExcel->getWorksheetIterator() as $worksheet){
			// Loop through all rows
			$highestrow = $worksheet->getHighestRow();
			$highestcolumn = $worksheet->getHighestColumn();
			$highestcolumnnr = PHPExcel_Cell::columnIndexFromString($highestcolumn);
	
			$row = 1; // Go to the first row where a table starts. 
			for ($i = $row; $i <= $highestrow; $i++){
				$row = $i;
				if (substr($worksheet->getCell('A' . $row)->getValue(), 0, 1) === '[') break;
			} // We are now at the beginning of a table or at the end of the file.
	
			$line = array(); // Line is a buffer of one or more related (subsequent) excel rows
	
			while ($row <= $highestrow){
				// Read this line as an array of values
				$values = array(); // values is a buffer containing the cells in a single excel row 
				for ($columnnr = 0; $columnnr < $highestcolumnnr; $columnnr++){ 
					$columnletter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
					$cell = $worksheet->getCell($columnletter . $row);
					
					$cellvalue = (string)$cell->getCalculatedValue();
					
					// overwrite $cellvalue in case of datetime
					// the @ is a php indicator for a unix timestamp (http://php.net/manual/en/datetime.formats.compound.php), later used for typeConversion
					if(PHPExcel_Shared_Date::isDateTime($cell) && !empty($cellvalue)) $cellvalue = '@'.(string)PHPExcel_Shared_Date::ExcelToPHP($cellvalue); 
					
					$values[] = $cellvalue;
				}
				$line[] = $values; // add line (array of values) to the line buffer
	
				$row++;
				// Is this relation table done? Then we parse the current values into function calls and reset it
				$firstCellInRow = (string)$worksheet->getCell('A' . $row)->getCalculatedValue();
				if (substr($firstCellInRow, 0, 1) === '['){ 
					// Relation table is complete, so it can be processed.
					$this->ParseLines($line);
					$line = array();
				}
			}
			
			// Last relation table remains to be processed.
			$this->ParseLines($line);
			$line = array();
		}
	}

	// Format is as follows:
	//		      [ <description of data> ], <relation1>,    <relationN>  
	//              <srcConcept>,              <tgtConcept1>,  <tgtConceptN>
	//              <srcAtomA>,                <tgtAtom1A>,    <tgtAtomNA>
	//              <srcAtomB>,                <tgtAtom1B>,    <tgtAtomNB>
	//              <srcAtomC>,                <tgtAtom1C>,    <tgtAtomNC>

	// Output is function call: 
	// InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
	private function ParseLines($data)
	{	$relation = $concept = $separator = $atom = array();
		
		foreach ($data as $linenr => $values)
		{	$totalcolumns = count($values);
			
			if ($linenr == 0) // First line specifies relation names:
			{	for ($col = 0; $col < $totalcolumns; $col++)
				{	$relation[$col] = $values[$col]; // Next: check to see if there is a trailing 'flip' ('~')
					$flipped[$col] = ( strpos($relation[$col],'~') === (strlen($relation[$col])-1) );
					if ($flipped[$col]) $relation[$col] = substr($relation[$col],0,strlen($relation[$col])-1);
				}
			}elseif ($linenr == 1) // Second line specifies concepts (and optionally: separators):
// In the Haskell importer, separators are the last character before the ']' if the concept is surrounded by such block quotes. Alternatively, you could specify a separator following such block-quotes, allowing for multiple-character separators.
			{	for ($col = 0; $col < $totalcolumns; $col++)
// The cell contains either 'Concept' or '[Conceptx]' where x is a separator character (e.g. ';', ',', ...)
				{	if (  (strpos($values[$col], '[') === 0) // if we have a '[' at the first position
					   && (strrpos($values[$col], ']') === (strlen($values[$col])-1)) // and a ']' at the last position
					   )
					{ // then we have a concept AND separator specification
//						 0123456789   positions/indexes
//						 [Concept,]   length=10, separatorpos=length-2
						$concept[$col] = substr($values[$col], 1, strlen($values[$col])-3);
						$separator[$col] = substr($values[$col], strlen($values[$col])-2, 1);
						Notifications::addLog("Multiple '$concept[$col]'-atoms allowed: separator='$separator[$col]'.", 'ExcelImport');
					}else
					{	$concept[$col] = $values[$col];
						$separator[$col] = false;
					}
				}
			}else // All other lines specify atoms:
			{	for ($col = 0; $col < $totalcolumns; $col++) $atom[$col] = $values[$col];

				// Don't process lines that start with an empty first cell
				if ($atom[0] == '' OR empty($atom[0])) continue;

//				// Check if this is an atom-create line, syntax = &atomname
//				if (strpos('&', $atom[0]) === 0){ 
				// Check if this is an atom-create line, syntax = _NEW
				if ($atom[0] === '_NEW')
  				{	$atom[0] = Concept::createNewAtomId($concept[0]); // Create a unique atom name
					Notifications::addLog("_NEW atom created: '$atom[0]'.", 'ExcelImport');
				}
				
				// Insert $atom[0] into the DB if it does not yet exist
				$this->addAtomToConcept($atom[0], $concept[0]);

				for ($col = 1; $col < $totalcolumns; $col++) // Now we transform the data info function calls:
				{	if ($atom[$col] == '' OR empty($atom[$col])) continue; // Empty cells are allowed but shouldn't do anything
					if ($concept[$col] == '' OR empty($concept[$col])) continue; // if no concept is specified, the contents of the cell should be ignored.
					if ($relation[$col] == '' OR empty($relation[$col])) continue; // if no relation is specified, the contents of the cell should be ignored.
//					if (strpos('&', $tgtatom) === 0){ // Check if this is an atom-create line, syntax = &atomname
					if ($atom[$col] === '_NEW') // Check if this is an atom-create line, syntax = &atomname
					{	$atom[$col] = $atom[0]; // '_NEW' copies the atom-value; useful for property-relations.
					}

					$atoms = array();
					if ($separator[$col])
					{	$atoms = explode($separator[$col],$atom[$col]);
					}else
					{	$atoms[] = $atom[$col];
					}
					Notifications::addLog("atoms-array has length ".count($atoms), 'ExcelImport');
					for ($i=0; $i < count($atoms); $i++)
					{	$tgtatom = $atoms[$i];
						$tgtatom = trim($tgtatom); // remove leading and trailing spaces, tabs, newlines, etc.
						Notifications::addLog("i='$i', tgtatom='$atoms[$i]', trimmed='$tgtatom'", 'ExcelImport');
						
						if ($flipped[$col])
						{	$this->insertRel($relation[$col],$tgtatom,$atom[0],$concept[$col],$concept[0]);
			Notifications::addLog("flippdRel($relation[$col],$tgtatom,$atom[0],$concept[$col],$concept[0])", 'ExcelImport');
						}else
						{	$this->insertRel($relation[$col],$atom[0],$tgtatom,$concept[0],$concept[$col]);
			Notifications::addLog("normalRel($relation[$col],$atom[0],$tgtatom,$concept[0],$concept[$col])", 'ExcelImport');
						}
					}
				}
				$atom = array();
			}
		}
	}
	
	private function addAtomToConcept($atom, $concept){
		$this->db->addAtomToConcept($atom, $concept);
		
	}
	
	private function insertRel($relationName, $srcAtom, $tgtAtom, $srcConcept, $tgtConcept){
		$this->db->editUpdate($relationName, false, $srcAtom, $srcConcept, $tgtAtom, $tgtConcept);
	}
}

?>