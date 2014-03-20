<?php
date_default_timezone_set('Europe/London');
require_once (__DIR__ . '/lib/Classes/PHPExcel.php');

$apps['ExcelImport'] = array('name' => 'Excel import', 'link' => IP . '/extensions/ExcelImport/', 'icon' => 'glyphicon glyphicon-upload'); // activeer app extension in framework

class ImportExcel
{
	public $file;
	private $commandArray = array();

	function __construct($filename){
		$this->file = $filename;
	}

	public function ParseFile(){
		$database = Database::singleton();
		
		$this->ProcessFileContent(); // fills $this->commandArray
		
		$database->transaction($this->commandArray);
		
		return ErrorHandling::getAll();
		
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

		// Loop through all rows
		$worksheet = $objPHPExcel->getActiveSheet();
		$highestrow = $worksheet->getHighestRow();
		$highestcolumn = $worksheet->getHighestColumn();
		$highestcolumnnr = PHPExcel_Cell::columnIndexFromString($highestcolumn);

		$row = 1; // Go to the first row where a table starts. 
		for ($i = $row; $i <= $highestrow; $i++){
			$row = $i;
			if (substr($objPHPExcel->getActiveSheet()->getCell('A' . $row)->getValue(), 0, 1) === '[') break;
		} // We are now at the beginning of a table or at the end of the file.

		$line = array(); // Line is a buffer of one or more related (subsequent) excel rows

		while ($row <= $highestrow){
			// Read this line as an array of values
			$values = array(); // values is a buffer containing the cells in a single excel row 
			for ($columnnr = 0; $columnnr < $highestcolumnnr; $columnnr++){ 
				$columnletter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
				$values[] = $objPHPExcel->getActiveSheet()->getCell($columnletter . $row)->getValue();
			}
			// var_dump($values);
			$line[] = $values; // add line (array of values) to the line buffer

			$row++;
			// Is this relation table done? Then we parse the current values into function calls and reset it
			$firstCellInRow = $objPHPExcel->getActiveSheet()->getCell('A' . $row)->getValue();
			// emitLog("First cell in row $row is: $firstCellInRow");
			if (substr($firstCellInRow, 0, 1) === '['){ 
				// Relation table is complete, so it can be processed.
				// emitLog( "<<< BLOK\n");
				// print_r($line);
				// emitLog( "\n/BLOK >>>\n\n");
				$this->ParseLines($line);
				$line = array();
			}
		}
		
		// Last relation table remains to be processed.
		// emitLog( "<<< BLOK\n");
		// print_r($line);
		// emitLog( "\n/BLOK >>>\n\n");
		$this->ParseLines($line);
		$line = array();
	}

	// Format is as follows:
	// (gray bg)    [ <description of data> ], <relation1>,    <relationN>  
	//              <srcConcept>,              <tgtConcept1>,  <tgtConceptN>
	//              <srcAtomA>,                <tgtAtom1A>,    <tgtAtomNA>
	//              <srcAtomB>,                <tgtAtom1B>,    <tgtAtomNB>
	//              <srcAtomC>,                <tgtAtom1C>,    <tgtAtomNC>

	// Output is function call: 
	// InsPair($relation,$srcConcept,$srcAtom,$tgtConcept,$tgtAtom)
	private function ParseLines($data){
		$relation = $concept = $atom = array();
		
		// echo "hello earth!\n";
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
					// $bla = "\n" . 'InsPair( RELATION:"'. $relation[$col] . '", SRCCONCEPT:"' . $concept[0];
					// $bla .= '", SRCATOM:"' . $atom[0] . '", TGTCONCEPT:"' . $concept[$col] . '", TGTATOM:"' . $atom[$col] . '" );';
					// echo $bla . "\n";
					if ($atom[$col] == '' OR empty($atom[$col])) continue; // Empty cells are allowed but shouldn't do anything
					
					if (strpos('&', $atom[$col]) === 0){ // Check if this is an atom-create line, syntax = &atomname
						$atom[$col] = $atom[0]; // '&' copies the atom-value; useful for property-relations.
					}
					
					$this->addAtomToConcept($atom[$col], $concept[$col]); // Try if this fixes the bug....
					$this->insertRel($relation[$col], $atom[0], $atom[$col]);
					
				}
				$atom = array();
			}
			// var_dump ($values);
		}
	}
	
	private function addAtomToConcept($atom, $concept){
		$this->commandArray[] = json_decode(json_encode(array('dbCmd' => 'addToConcept', 'atom' => $atom, 'concept' => $concept)), false); // commands need to be objects. TODO: create command class for this.
	}
	
	private function insertRel($relation, $srcAtom, $tgtAtom){
		$this->commandArray[] = json_decode(json_encode(array('dbCmd' => 'update'
									, 'relation' => $relation
									, 'isFlipped' => false
									, 'parentAtom' => $srcAtom
									, 'childAtom' => $tgtAtom
									, 'parentOrChild' => 'child'
									, 'originalAtom' => ''
									)), false);
	}
}

?>
