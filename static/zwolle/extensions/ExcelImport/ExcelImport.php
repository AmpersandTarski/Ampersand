<?php

namespace Ampersand\Extension\ExcelImport;

use Exception;
use Ampersand\AngularApp;
use Ampersand\Core\Link;
use Ampersand\Core\Atom;
use Ampersand\Core\Concept;
use Ampersand\Misc\Config;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Relation;
use Ampersand\Log\Logger;
use PHPExcel_Cell;
use PHPExcel_Shared_Date;
use PHPExcel_IOFactory;
use PHPExcel_Worksheet;

// UI
AngularApp::addMenuItem('ext', 'extensions/ExcelImport/ui/views/MenuItem.html', 
    function(\Ampersand\AmpersandApp $app){
        $roles = Config::get('allowedRolesForExcelImport','excelImport');
        return $app->hasActiveRole($roles);
    });
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
                $this->ParseWorksheetWithIfc($worksheet, $ifc);
            }catch (Exception $e){
                $this->logger->warning("No interface found with name as title of worksheet '{$worksheet->getTitle()}'. Parsing file without interface");
                $this->ParseWorksheet($worksheet);
            }
        }
    }
    
    /**
     * Parse worksheet according to an Ampersand interface definition.
     * 
     * @param \PHPExcel_Worksheet $worksheet
     * @param \Ampersand\Interfacing\InterfaceObject $ifc
     * @return void
     * 
     * Use interface name as worksheet name. Format for content is as follows:
     *          | Column A        | Column B        | Column C        | etc
     * Row 1    | <srcConcept>    | <ifc label x>   | <ifc label y>   | etc
     * Row 2    | <srcAtom a1>    | <tgtAtom b1>    | <tgtAtom c1>    | etc
     * Row 3    | <srcAtom a2>    | <tgtAtom b2>    | <tgtAtom c2>    | etc
     * etc
     */
    private function ParseWorksheetWithIfc(PHPExcel_Worksheet $worksheet, InterfaceObject $ifc) 
    {
        // Determine $leftConcept from cell A1
        $leftConcept = Concept::getConceptByLabel((string)$worksheet->getCell('A1'));
        if ($leftConcept != $ifc->tgtConcept) throw new Exception("Target concept of interface '{$ifc->getPath()}' does not match concept specified in cell {$worksheet->getTitle()}:A1", 500);
        
        // Parse other columns of first row
        $header = [];
        foreach ($worksheet->getColumnIterator('B') as $column) {
            /** @var \PHPExcel_Worksheet_Column $column */

            $columnLetter = $column->getColumnIndex();            
            $cellvalue = (string)$worksheet->getCell($columnLetter . '1')->getCalculatedValue();
            
            if ($cellvalue != '') {
                $subIfc = $ifc->getSubinterfaceByLabel($cellvalue);
                if(!$subIfc->crudU() || !$subIfc->relation) throw new Exception ("Use of {$subIfc->label} in cell {$columnLetter}1 is not allowed. No update rights specified in interface", 403);
                $header[$columnLetter] = $subIfc;
            } else {
                $this->logger->warning("Skipping column {$columnLetter} in sheet {$worksheet->getTitle()}, because header is not provided");
            }
        }
        
        // Parse other rows
        foreach ($worksheet->getRowIterator(2) as $row) {
            /** @var \PHPExcel_Worksheet_Row $row */

            $rowNr = $row->getRowIndex();

            $firstCol = (string)$worksheet->getCell('A'.$rowNr)->getCalculatedValue();
            
            // If cell Ax is empty, skip complete row
            if ($firstCol == ''){
                $this->logger->warning("Skipping row {$rowNr} in sheet {$worksheet->getTitle()}, because column A is empty");
                continue;
            }

            // If cell Ax contains '_NEW', this means to automatically create a new atom
            elseif ($firstCol == '_NEW') {
                if(!$ifc->crudC()) throw new Exception("Trying to create new atom in cell A{$rowNr}. This is not allowed.", 403);
                $leftAtom = $leftConcept->createNewAtom()->add();
            }
            
            // Else instantiate atom with given atom identifier
            else {
                $leftAtom = new Atom($firstCol, $leftConcept);
                if(!$leftAtom->exists()){
                    if (!$ifc->crudC()) {
                        throw new Exception("Trying to create new {$leftConcept} in cell A{$rowNr}. This is not allowed.", 403);
                    } else {
                        $leftAtom->add();
                    }
                } else {
                    // $leftAtom already exists..do nothing
                }
            }
            
            // Process other columns of this row
            foreach ($header as $columnLetter => $ifc) {                
                $cell = $worksheet->getCell($columnLetter . $rowNr); 
                $cellvalue = (string)$cell->getCalculatedValue();
                
                if ($cellvalue == '') continue; // skip if not value provided                
                 
                // Overwrite $cellvalue in case of datetime
                // The @ is a php indicator for a unix timestamp (http://php.net/manual/en/datetime.formats.compound.php), later used for typeConversion
                if (PHPExcel_Shared_Date::isDateTime($cell) && !empty($cellvalue)) $cellvalue = '@'.(string)PHPExcel_Shared_Date::ExcelToPHP($cellvalue);
                
                $rightAtom = new Atom($cellvalue, $ifc->tgtConcept);
                if (!$rightAtom->exists()) {
                    if ($ifc->tgtConcept->isObject() && !$ifc->crudC()) {
                        throw new Exception("Trying to create new {$ifc->tgtConcept} in cell {$columnLetter}{$rowNr}. This is not allowed.", 403);
                    } else {
                        $rightAtom->add();
                    }
                }
                
                $leftAtom->link($rightAtom, $ifc->relation(), $ifc->relationIsFlipped)->add();
            }
        }
    }
    
    /**
     * Parse worksheet according to the 2-row header information.
     * Row 1 contains the relation names, Row 2 the corresponding concept names
     * Multiple block of imports can be specified on a single sheet. 
     * The parser looks for the brackets '[ ]' to start a new block
     * 
     * @param \PHPExcel_Worksheet $worksheet
     * @return void
     * 
     * Format of sheet:
     *           | Column A        | Column B        | Column C        | etc
     * Row 1     | [ block label ] | <relation name> | <relation name> | etc
     * Row 2     | <srcConcept>    | <tgtConcept1>   | <tgtConcept2>   | etc
     * Row 3     | <srcAtom a1>    | <tgtAtom b1>    | <tgtAtomN c1>   | etc
     * Row 4     | <srcAtom a2>    | <tgtAtom b2>    | <tgtAtomN c2>   | etc
     * etc
     * 
     */
    private function ParseWorksheet(PHPExcel_Worksheet $worksheet){
        // Loop through all rows
        $highestrow = $worksheet->getHighestRow();
        $highestcolumn = $worksheet->getHighestColumn();
        $highestcolumnnr = PHPExcel_Cell::columnIndexFromString($highestcolumn);
        
        $row = 1; // Go to the first row where a table starts.
        for ($i = $row; $i <= $highestrow; $i++){
            $row = $i;
            $cellvalue = $worksheet->getCell('A' . $row)->getValue();
            if (substr(trim($cellvalue), 0, 1) === '[') break;
        } // We are now at the beginning of a table or at the end of the file.
        
        $lines = array(); // Line is a buffer of one or more related (subsequent) excel rows
        
        while ($row <= $highestrow){
            // Read this line as an array of values

            $line = array(); // values is a buffer containing the cells in a single excel row
            for ($columnnr = 0; $columnnr < $highestcolumnnr; $columnnr++){
                $columnLetter = PHPExcel_Cell::stringFromColumnIndex($columnnr);
                $cell = $worksheet->getCell($columnLetter . $row);
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
            if (substr(trim($firstCellInRow), 0, 1) === '['){
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
                    $relations[$col] = trim($line[$col]); // No leading/trailing spaces around relation names.
                }
            
            // Second line specifies concepts (and optionally: separators)
            }elseif ($linenr == 1){
                // In the Haskell importer, separators are the last character before the ']' if the concept is surrounded by such block quotes. Alternatively, you could specify a separator following such block-quotes, allowing for multiple-character separators.
                for ($col = 0; $col < $totalcolumns; $col++){
                    
                    $cellvalue = trim($line[$col]); // No leading/trailing spaces around cell values in second line
                    if($cellvalue == ''){
                        $concept[$col] = null;
                    // The cell contains either 'Concept' or '[Conceptx]' where x is a separator character (e.g. ';', ',', ...)
                    }elseif ((substr($cellvalue, 0, 1) == '[') && (substr($cellvalue, -1) == ']') ){
                        if($col == 0) throw new Exception ("Seperator character not allowed for first column of excel import. Specified '{$line[$col]}'", 500);
                        $concept[$col] = Concept::getConceptByLabel(substr($cellvalue, 1, -2));
                        $separator[$col] = substr($cellvalue, -2, 1);
                    }else{
                        $concept[$col] = Concept::getConceptByLabel($cellvalue);
                        $separator[$col] = false;
                    }
                    
                    // Determine relations for all cols except col 0
                    if($col > 0){
                        if($relations[$col] == '' || $concept[$col] == ''){
                            $relations[$col] = null;
                        }elseif(substr($relations[$col], -1) == '~'){ // Relation is flipped is last character is a tilde (~)
                            $relations[$col] = Relation::getRelation(substr($relations[$col], 0, -1), $concept[$col], $concept[0]);
                            $flipped[$col] = true;
                        }else{
                            $relations[$col] = Relation::getRelation($relations[$col], $concept[0], $concept[$col]);
                            $flipped[$col] = false;
                        }
                    }
                }
            
            // All other lines specify atoms
            }else{                
                $line[0] = trim($line[0]); // Trim cell content (= dirty identifier)
                
                // Determine left atom (column 0) of line
                if ($line[0] == '') continue; // Don't process lines that start with an empty first cell
                elseif ($line[0] == '_NEW') $leftAtom = $concept[0]->createNewAtom(); // Create a unique atom name
                else $leftAtom = new Atom($line[0], $concept[0]);
                
                // Insert $leftAtom into the DB if it does not yet exist
                $leftAtom->add();
                
                // Process other columns of line
                for ($col = 1; $col < $totalcolumns; $col++){
                    
                    if (is_null($concept[$col])) continue; // If no concept is specified, the cell is skipped
                    if (is_null($relations[$col])) continue; // If no relation is specified, the cell is skipped
                    
                    // Determine right atom(s)
                    $rightAtoms = array();
                    
                    $cell = trim($line[$col]); // Start of check for multiple atoms in single cell
                    if ($cell == '') continue; // If cell is empty, it is skipped
                    elseif ($cell == '_NEW') $rightAtoms[] = $leftAtom; // If the cell contains '_NEW', the same atom as the $leftAtom is used. Useful for property-relations
                    elseif($separator[$col]){
                        $atomsIds = explode($separator[$col],$cell); // atomnames may have surrounding whitespace
                        foreach($atomsIds as $atomId) $rightAtoms[] = new Atom(trim($atomId), $concept[$col]);
                    }else{
                        $rightAtoms[] = new Atom($line[$col], $concept[$col]); // DO NOT TRIM THIS CELL CONTENTS as it contains an atom that may need leading/trailing spaces
                    }
                    
                    foreach ($rightAtoms as $rightAtom){
                        if($flipped[$col]) (new Link($relations[$col], $rightAtom, $leftAtom))->add();
                        else (new Link($relations[$col], $leftAtom, $rightAtom))->add();
                    }
                }
            }
        }
    }
}
