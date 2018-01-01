<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

use Ampersand\IO\AbstractWriter;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class CSVWriter extends AbstractWriter {

    public function write($data){
        $delimiter = ';';

        // Output the column headings (use first row to get column heading)
        $columns = array_keys($data[0]);
        fputcsv($this->stream, $columns, $delimiter);

        // Output rows
        foreach ($data as $row) {
            $fields = [];
            foreach ($columns as $col) {
                if(isset($row[$col])) $fields[] = $row[$col];
                else $fields[] = null;
            }
            
            fputcsv($this->stream, $fields, $delimiter);
        }
    }
}
