<?php

namespace Ampersand\IO;

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
use PHPExcel_Worksheet_Row;

class ExcelImporter {
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
     * Parse excelsheet and import population
     * 
     * @param string $filename
     * @return void
     */
    public function parseFile($filename){
        $file = PHPExcel_IOFactory::load($filename);

        $this->logger->info("Excel import started: parsing file {$filename}");
        
        // Loop over all worksheets
        foreach($file->getWorksheetIterator() as $worksheet){
            /** @var \PHPExcel_Worksheet $worksheet */
            try {
                // First check if there is an interface with the same id as the worksheet
                $ifc = InterfaceObject::getInterfaceByLabel($worksheet->getTitle());
                $this->parseWorksheetWithIfc($worksheet, $ifc);
            }catch (Exception $e){
                $this->logger->warning("No interface found with name as title of worksheet '{$worksheet->getTitle()}'. Parsing file without interface");
                $this->parseWorksheet($worksheet);
            }
        }
        
        $this->logger->info("Excel import completed");
        
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
    protected function parseWorksheetWithIfc(PHPExcel_Worksheet $worksheet, InterfaceObject $ifc) 
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
    protected function parseWorksheet(PHPExcel_Worksheet $worksheet)
    {
        // Find import blocks
        $blockStartRowNrs = [];
        foreach ($worksheet->getRowIterator() as $row) {
            /** @var \PHPExcel_Worksheet_Row $row */
            $rowNr = $row->getRowIndex();
            $cellvalue = $worksheet->getCell('A'. $rowNr)->getValue();

            // Import block is indicated by '[]' brackets in cell Ax
            if (substr(trim($cellvalue), 0, 1) === '['){
                $blockStartRowNrs[] = $rowNr;
            }
        }

        // Process import blocks
        foreach ($blockStartRowNrs as $key => $startRowNr) {
            $endRowNr = isset($blockStartRowNrs[$key + 1]) ? $blockStartRowNrs[$key + 1] : null;
            $this->parseBlock($worksheet, $startRowNr, $endRowNr);
        }
    }

    /**
     * Undocumented function
     *
     * @param PHPExcel_Worksheet $worksheet
     * @param int $startRowNr
     * @param int $endRowNr
     * @return void
     */
    protected function parseBlock(PHPExcel_Worksheet $worksheet, int $startRowNr, int $endRowNr = null)
    {
        $line1 = [];
        $line2 = [];
        $header = [];

        $i = 0; // row counter
        foreach($worksheet->getRowIterator($startRowNr, $endRowNr) as $row) {
            /** @var \PHPExcel_Worksheet_Row $row */

            $i++; // increment row counter

            // Header line 1 specifies relation names
            if ($i === 1) {
                foreach ($row->getCellIterator() as $cell) {
                    /** @var \PHPExcel_Cell $cell */
                    $line1[$cell->getColumn()] = trim((string) $cell->getValue()); // no leading/trailing spaces allowed
                }
            }
            
            // Header line 2 specifies concept names
            elseif ($i === 2) {
                foreach ($row->getCellIterator() as $cell) {
                    /** @var \PHPExcel_Cell $cell */
                    
                    $col = $cell->getColumn();
                    $line2[$col] = trim((string) $cell->getValue()); // no leading/trailing spaces allowed
                
                    // Import header can be determined now using line 1 and line 2
                    if ($col === 'A') {
                        $leftConcept = Concept::getConceptByLabel($line2[$col]);
                        $header[$col] = ['concept' => $leftConcept, 'relation' => null, 'flipped' => null];
                    } else {
                        if ($line1[$col] == '' || $line2[$col] == '') {
                            // Skipping column
                            $this->logger->warning("Skipping column {$col} in sheet {$worksheet->getTitle()}, because header is not complete");
                        }
                        // Relation is flipped when last character is a tilde (~)
                        elseif (substr($line1[$col], -1) == '~') {
                            $rightConcept = Concept::getConceptByLabel($line2[$col]);
                            
                            $header[$col] = ['concept' => $rightConcept
                                            ,'relation' => Relation::getRelation(substr($line1[$col], 0, -1), $rightConcept, $leftConcept)
                                            ,'flipped' => true
                                            ];
                        } else {
                            $rightConcept = Concept::getConceptByLabel($line2[$col]);
                            $header[$col] = ['concept' => $rightConcept
                                            ,'relation' => Relation::getRelation($line1[$col], $leftConcept, $rightConcept)
                                            ,'flipped' => false
                                            ];
                        }
                    }
                }
            }
            
            // Data lines
            else {
                $this->processDataRow($row, $header);
            }
        }
    }
    
    /**
     * Undocumented function
     *
     * @param PHPExcel_Worksheet_Row $row
     * @param array $headerInfo
     * @return void
     */
    private function processDataRow(PHPExcel_Worksheet_Row $row, array $headerInfo)
    {
        foreach ($row->getCellIterator() as $cell) {
            /** @var \PHPExcel_Cell $cell */

            $col = $cell->getColumn();

            // Skip cell if column must not be imported
            if(!array_key_exists($col, $headerInfo)) continue; // continue to next cell

            $cellvalue = (string) $cell->getCalculatedValue(); // !Do NOT trim this cellvalue, because atoms may have leading/trailing whitespace

            // Overwrite $cellvalue in case of datetime
            // the @ is a php indicator for a unix timestamp (http://php.net/manual/en/datetime.formats.compound.php), later used for typeConversion
            if(PHPExcel_Shared_Date::isDateTime($cell) && !empty($cellvalue)) $cellvalue = '@'.(string)PHPExcel_Shared_Date::ExcelToPHP($cellvalue);

            // Determine $leftAtom using column A
            if ($col == 'A') {
                // If cell Ax is empty, skip complete row
                if ($cellvalue == '') {
                    $this->logger->warning("Skipping row {$row->getRowIndex()}, because column A is empty");
                    return; // stop processing complete row
                }

                // If cell Ax contains '_NEW', this means to automatically create a new atom
                elseif ($cellvalue == '_NEW') {
                    $leftAtom = $headerInfo[$col]['concept']->createNewAtom()->add();
                }

                // Else instantiate atom with given atom identifier
                else {
                    $leftAtom = (new Atom($cellvalue, $headerInfo[$col]['concept']))->add();
                }
            
            // Process other columns
            } else {
                // If cell is empty, skip column
                if ($cellvalue == '') {
                    continue; // continue to next cell
                } elseif ($cellvalue == '_NEW') {
                    $rightAtom = $leftAtom;
                } else {
                    $rightAtom = (new Atom($cellvalue, $headerInfo[$col]['concept']))->add();
                }

                $leftAtom->link($rightAtom, $headerInfo[$col]['relation'], $headerInfo[$col]['flipped'])->add();
            }
        }
    }
}
