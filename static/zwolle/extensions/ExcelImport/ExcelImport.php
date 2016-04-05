<?php

namespace Ampersand\Extension\ExcelImport;

use Exception;
use Ampersand\AngularApp;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Config;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Relation;
use Ampersand\Log\Logger;
use PHPExcel_Cell;
use PHPExcel_Shared_Date;
use PHPExcel_IOFactory;

require_once (__DIR__ . '/lib/Classes/PHPExcel.php');

// UI
$GLOBALS['navBar']['appMenu'][] = array ( 'url' => 'extensions/ExcelImport/ui/views/MenuItem.html'); 
AngularApp::addCSS('extensions/ExcelImport/ui/css/style.css');
AngularApp::addJS('extensions/ExcelImport/ui/js/ExcelImport.js');

// API
$GLOBALS['api']['files'][] = __DIR__ . DIRECTORY_SEPARATOR . 'api' . DIRECTORY_SEPARATOR . 'import.php';

// Config
Config::set('allowedMimeTypes', 'excelImport', array('application/vnd.ms-excel', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'application/excel'));

class ExcelImport {
	/**
	 * 
	 * @var string
	 */
    public $file;
    
	/**
	 *
	 * @var \Psr\Log\LoggerInterface
	 */
	private $logger;

	/**
	 * Constructor
	 */
	function __construct(){
	    $this->logger = Logger::getLogger('EXCELIMPORT');
	}
	
	/**
	 * @param string $filename
	 * @return void
	 */
	public function ParseFile($filename){
	    $this->file = $filename;
		
		$this->logger->info("Excel import started: parsing file {$this->file}");
				
		$this->ProcessFileContent();
		
		$this->logger->info("Excel import completed");
		
	}
	
	/**
	 * 
	 * @return void
	 */
	private function ProcessFileContent(){
		$objPHPExcel = PHPExcel_IOFactory::load($this->file);

		// Loop over all worksheets
		foreach($objPHPExcel->getWorksheetIterator() as $worksheet){
		    try {
		        // First check if there is an interface with the same id as the worksheet
		        $ifc = InterfaceObject::getInterfaceByLabel($worksheet->getTitle());
		        $parseWithIfc = true;
		    }catch (Exception $e){
		        $this->logger->warning("No interface found with name as title of worksheet '{$worksheet->getTitle()}'. Parsing file without interface");
		        $parseWithIfc = false;
		    }
		    
		    if($parseWithIfc) $this->ParseWorksheetWithIfc($worksheet, $ifc);
		    else $this->ParseWorksheet($worksheet);
		}
	}
	
	/**
	 * 
	 * @param PHPExcel_Worksheet $worksheet
	 * @param InterfaceObject $ifc
	 * @return void
	 */
	private function ParseWorksheetWithIfc($worksheet, $ifc){
	    /* Use interface name as worksheet name. Format for content is as follows:
	    #1 <srcConcept> | <ifc label x> | <ifc label y> | <etc>
	    #2 <srcAtomA>   | <tgtAtom1>    | <tgtAtom2>    | <etc>
	    #3 <srcAtomB>   | <tgtAtom3>    | <tgtAtom4>    | <etc>
	    */
	    
	    $highestrow = $worksheet->getHighestRow();
	    $highestcolumn = $worksheet->getHighestColumn();
	    $highestcolumnnr = PHPExcel_Cell::columnIndexFromString($highestcolumn);
	    
	    $leftConcept = Concept::getConcept((string)$worksheet->getCell('A1'));
	    if($leftConcept != $ifc->tgtConcept) throw new Exception("Target concept of interface '{$ifc->path}' does not match concept specified in cell {$worksheet->getTitle()}:A1", 500);
	    
	    // Parse other columns of first row
	    $header = array();
	    for ($columnnr = 1; $columnnr < $highestcolumnnr; $columnnr++){
	        $columnletter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
	        $cell = $worksheet->getCell($columnletter . '1');
	        
	        $cellvalue = (string)$cell->getCalculatedValue();
	        
	        if($cellvalue == '') $header[$columnletter] = null; // will be skipped
	        else{
	            $subIfc = $ifc->getSubinterfaceByLabel($cellvalue);
	            if(!$subIfc->crudU || !$subIfc->relation) throw new Exception ("Update not allowed/possible for {$subIfc->label} as specified in cell {$columnletter}1", 403);
	            $header[$columnletter] = $subIfc;
	            
	        }
	    }
	    
	    for ($row = 2; $row <= $highestrow; $row++){
	        $firstCol = (string)$worksheet->getCell('A'.$row)->getCalculatedValue();
	        if($firstCol == '') continue; // Skip this row
	        elseif($firstCol == '_NEW'){
	            if(!$ifc->crudC) throw new Exception("Trying to create new atom in cell A{$row}. This is not allowed.", 403);
	            $leftAtom = $leftConcept->createNewAtom()->addAtom();
	        }else{
	            $leftAtom = new Atom($firstCol, $leftConcept->name);
	            if(!$leftAtom->atomExists() && !$ifc->crudC) throw new Exception("Trying to create new {$leftConcept->name} in cell A{$row}. This is not allowed.", 403);
	            $leftAtom->addAtom();
	        }
	        
	        for ($columnnr = 1; $columnnr < $highestcolumnnr; $columnnr++){
	            $columnletter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
	            
	            if(is_null($header[$columnletter])) continue; // skip this column.
	            
	            $cell = $worksheet->getCell($columnletter . $row); 
	            $cellvalue = (string)$cell->getCalculatedValue();
	            
	            if($cellvalue == '') continue; // skip if not value provided	            
	             
	            // overwrite $cellvalue in case of datetime
	            // the @ is a php indicator for a unix timestamp (http://php.net/manual/en/datetime.formats.compound.php), later used for typeConversion
	            if(PHPExcel_Shared_Date::isDateTime($cell) && !empty($cellvalue)) $cellvalue = '@'.(string)PHPExcel_Shared_Date::ExcelToPHP($cellvalue);
	            
	            $rightAtom = new Atom($cellvalue, $header[$columnletter]->tgtConcept->name);
	            if(!$rightAtom->atomExists() && !$header[$columnletter]->crudC) throw new Exception("Trying to create new {$header[$columnletter]->tgtConcept->name} in cell {$columnletter}{$row}. This is not allowed.", 403);
	            
	            $header[$columnletter]->relation->addLink($leftAtom, $rightAtom, $header[$columnletter]->relationIsFlipped, 'ExcelImport');
	        }
	    }
	}
	
	/**
	 * 
	 * @param PHPExcel_Worksheet $worksheet
	 * @return void
	 */
	private function ParseWorksheet($worksheet){
	    // Format is as follows:
	    // (gray bg)    [ <description of data> ], <relation1>,    <relationN>
	    //              <srcConcept>,              <tgtConcept1>,  <tgtConceptN>
	    //              <srcAtomA>,                <tgtAtom1A>,    <tgtAtomNA>
	    //              <srcAtomB>,                <tgtAtom1B>,    <tgtAtomNB>
	    //              <srcAtomC>,                <tgtAtom1C>,    <tgtAtomNC>
	    
	    // Loop through all rows
	    $highestrow = $worksheet->getHighestRow();
	    $highestcolumn = $worksheet->getHighestColumn();
	    $highestcolumnnr = PHPExcel_Cell::columnIndexFromString($highestcolumn);
	    
	    $row = 1; // Go to the first row where a table starts.
	    for ($i = $row; $i <= $highestrow; $i++){
	        $row = $i;
	        if (substr($worksheet->getCell('A' . $row)->getValue(), 0, 1) === '[') break;
	    } // We are now at the beginning of a table or at the end of the file.
	    
	    $lines = array(); // Line is a buffer of one or more related (subsequent) excel rows
	    
	    while ($row <= $highestrow){
	        // Read this line as an array of values
	    
	        $line = array(); // values is a buffer containing the cells in a single excel row
	        for ($columnnr = 0; $columnnr < $highestcolumnnr; $columnnr++){
	            $columnletter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
	            $cell = $worksheet->getCell($columnletter . $row);
	            	
	            $cellvalue = (string)$cell->getCalculatedValue();
	            	
	            // overwrite $cellvalue in case of datetime
	            // the @ is a php indicator for a unix timestamp (http://php.net/manual/en/datetime.formats.compound.php), later used for typeConversion
	            if(PHPExcel_Shared_Date::isDateTime($cell) && !empty($cellvalue)) $cellvalue = '@'.(string)PHPExcel_Shared_Date::ExcelToPHP($cellvalue);
	            	
	            $line[] = $cellvalue;
	        }
	        $lines[] = $line; // add line (array of values) to the line buffer
	    
	        $row++;
	        // Is this relation table done? Then we parse the current values into function calls and reset it
	        $firstCellInRow = (string)$worksheet->getCell('A' . $row)->getCalculatedValue();
	        if (substr($firstCellInRow, 0, 1) === '['){
	            // Relation table is complete, so it can be processed.
	            $this->ParseLines($lines);
	            $lines = array();
	        }
	    }
	    	
	    // Last relation table remains to be processed.
	    $this->ParseLines($lines);
	    $lines = array();
	}
	
	/**
	 * 
	 * @param array $lines
	 */
	private function ParseLines($lines){
	    $relations = $concept = $separator = $flipped = array();
		
		foreach ($lines as $linenr => $line){
		    $totalcolumns = count($line);
			
		    // First line specifies relation names
			if ($linenr == 0){
			    for ($col = 0; $col < $totalcolumns; $col++){
			        $relations[$col] = $line[$col];
				}
			
			// Second line specifies concepts (and optionally: separators)
			}elseif ($linenr == 1){
			    // In the Haskell importer, separators are the last character before the ']' if the concept is surrounded by such block quotes. Alternatively, you could specify a separator following such block-quotes, allowing for multiple-character separators.
				for ($col = 0; $col < $totalcolumns; $col++){
				    
				    if($line[$col] == ''){
				        $concept[$col] = null;
				    // The cell contains either 'Concept' or '[Conceptx]' where x is a separator character (e.g. ';', ',', ...)
				    }elseif ((substr($line[$col], 0, 1) == '[') && (substr($line[$col], -1) == ']') ){
				        if($col == 0) throw new Exception ("Seperator character not allowed for first column of excel import. Specified '{$line[$col]}'", 500);
				        $concept[$col] = Concept::getConcept(substr($line[$col], 1, -2));
						$separator[$col] = substr($line[$col], -2, 1);
					}else{
					    $concept[$col] = Concept::getConcept($line[$col]);
						$separator[$col] = false;
					}
					
					// Determine relations for all cols except col 0
					if($col > 0){
    					if($relations[$col] == '' || $concept[$col] == ''){
    					    $relations[$col] = null;
    					}elseif(substr($relations[$col], -1) == '~'){ // Relation is flipped is last character is a tilde (~)
    					    $relations[$col] = Relation::getRelation(substr($relations[$col], 0, -1), $concept[$col]->name, $concept[0]->name);
    					    $flipped[$col] = true;
    					}else{
    					    $relations[$col] = Relation::getRelation($relations[$col], $concept[0]->name, $concept[$col]->name);
    					    $flipped[$col] = false;
    					}
					}
				}
			
			// All other lines specify atoms
			}else{			    
			    $line[0] = trim($line[0]); // Trim cell content
			    
			    // Determine left atom (column 0) of line
				if ($line[0] == '') continue; // Don't process lines that start with an empty first cell
				elseif ($line[0] == '_NEW') $leftAtom = $concept[0]->createNewAtom(); // Create a unique atom name
				else $leftAtom = new Atom($line[0], $concept[0]->name);
				
				// Insert $leftAtom into the DB if it does not yet exist
				$leftAtom->addAtom();
				
				// Process other columns of line
				for ($col = 1; $col < $totalcolumns; $col++){
				    
					if (is_null($concept[$col])) continue; // If no concept is specified, the cell is skipped
					if (is_null($relations[$col])) continue; // If no relation is specified, the cell is skipped
					
					// Determine right atom(s)
					$rightAtoms = array();
					
					$cell = trim($line[$col]); // Trim cell content
					
					if ($cell == '') continue; // If cell is empty, it is skipped
					elseif ($cell == '_NEW') $rightAtoms[] = $leftAtom; // If the cell contains '_NEW', the same atom as the $leftAtom is used. Useful for property-relations
					elseif($separator[$col]){
					    $atomsIds = explode($separator[$col],$cell);
					    foreach($atomsIds as $atomId) $rightAtoms[] = new Atom($atomId, $concept[$col]->name);
					}else{
					    $rightAtoms[] = new Atom($line[$col], $concept[$col]->name);
					}
					
					foreach ($rightAtoms as $rightAtom){
					    $relations[$col]->addLink($leftAtom, $rightAtom, $flipped[$col], 'ExcelImport');
					}
				}
			}
		}
	}
}

?>