<?php
date_default_timezone_set('Europe/London');
require_once (__DIR__ . '/lib/Classes/PHPExcel.php');

// Zet extension in applications menu
$GLOBALS['navBar']['appMenu'][] = array ( 'url' => 'extensions/ExcelImport/ui/views/MenuItem.html');

// UI
$GLOBALS['hooks']['after_Viewer_load_cssFiles'][] = 'extensions/ExcelImport/ui/css/style.css';
$GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/ExcelImport/ui/js/ExcelImport.js';

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
	private function ParseLines($data){
		$relation = $concept = $atom = array();
		
		foreach ($data as $linenr => $values){ 
			$totalcolumns = count($values);
			
			if ($linenr == 0){ // Relations:
				for ($col = 0; $col < $totalcolumns; $col++) $relation[$col] = $values[$col];
			}elseif ($linenr == 1){ // Concepts:
				for ($col = 0; $col < $totalcolumns; $col++) $concept[$col] = $values[$col];
			}else{ // Atoms:
				for ($col = 0; $col < $totalcolumns; $col++) $atom[$col] = $values[$col];

				// Don't process lines that start with an empty first cell
				if ($atom[0] == '' OR empty($atom[0])) continue;

				// Check if this is an atom-create line, syntax = &atomname
				if (strpos('&', $atom[0]) === 0){ 
					$atom[0] = Concept::createNewAtom($concept[0]); // Create a unique atom name
				}
				
				// Insert $atom[0] into the DB if it does not yet exist
				$this->addAtomToConcept($atom[0], $concept[0]);
								
				for ($col = 1; $col < $totalcolumns; $col++){ // Now we transform the data info function calls:
					if ($atom[$col] == '') continue; // Empty cells are allowed but shouldn't do anything
					if ($concept[$col] == '' OR empty($concept[$col])) continue; // if no concept is specified, the contents of the cell should be ignored.
					if ($relation[$col] == '' OR empty($relation[$col])) continue; // if no relation is specified, the contents of the cell should be ignored.
					
					if (strpos('&', $atom[$col]) === 0){ // Check if this is an atom-create line, syntax = &atomname
						$atom[$col] = $atom[0]; // '&' copies the atom-value; useful for property-relations.
					}
					
					$this->insertRel($relation[$col], $atom[0], $atom[$col], $concept[0], $concept[$col]);
					
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
