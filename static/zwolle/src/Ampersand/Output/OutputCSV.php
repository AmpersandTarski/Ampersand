<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Output;
use Exception;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class OutputCSV {
    
    /**
     * @var array $headers Contains http response headers.
     */
    private $headers = array();
    
    /**
     * @var array $columns Contains csv column defintions. Index of array determines the order of the columns
     */
    private $columns = array();
    
    /**
     * @var array $rows Contains csv rows with content of fields. Multidimensional array. Index of array determines order of the rows.
     */
    private $rows = array();
    
    /**
     * @param array $options Configuration options
     */
    public function __construct($options = array()){
        $this->headers[] = 'Content-Type: text/csv; charset=utf-8';
    }
    
    /**
     * Add csv columns
     * @param array $colNames
     * @return void
     */
    public function addColumns(array $colNames){
        foreach($colNames as $colName) $this->addColumn($colName);
    }
    
    /**
     * Add csv column
     * @param string $colName
     */
    public function addColumn($colName){
        if(!is_string("$colName")) throw new Exception("Error in colname for csv");
        if(!in_array($colName, $this->columns)) $this->columns[] = $colName;
    }
    
    public function addRow(array $row){
        $this->rows[] = $row;
    }
    
    /**
     * Add additional http response headers
     * @param string $header
     */
    public function addHeader($header){
        $this->headers[] = $header;
    }
    
    public function render($filename, $delimiter = ";"){
        $this->addHeader("Content-Disposition: attachment; filename={$filename}");
        
        // set reponse headers
        foreach($this->headers as $header) header($header);
        
        // create a file pointer connected to the output stream
        $stream = fopen('php://output', 'w');
        
        // output the column headings
        fputcsv($stream, array_values($this->columns), $delimiter);
        
        // output rows
        foreach ($this->rows as $row) {
            $fields = array();
            foreach ($this->columns as $col) {
                if(isset($row[$col])) $fields[] = $row[$col];
                else $fields[] = null;
            }
            
            fputcsv($stream, $fields, $delimiter);
        }
        
        fclose($stream);
    }
}