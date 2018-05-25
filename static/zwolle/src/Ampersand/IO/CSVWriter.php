<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\IO;

use Ampersand\IO\AbstractWriter;
use Exception;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class CSVWriter extends AbstractWriter
{

    public function write($data)
    {
        $delimiter = ';';

        // Output the column headings (use first row to get column heading)
        $columns = array_keys($data[0]);
        $this->stream->write($this->formatCSVLine($columns, $delimiter));

        // Output rows
        foreach ($data as $row) {
            $fields = [];
            foreach ($columns as $col) {
                if (isset($row[$col])) {
                    switch (gettype($row[$col])) {
                        case "array":
                            throw new Exception("Cannot output multi layer arrays to CSV", 500); // TODO, replace with functionality to comma separate content one level deeper
                        case "boolean":
                        case "integer":
                        case "double": // = float
                        case "string":
                        case "NULL":
                            $fields[] = $row[$col];
                            break;
                        case "object":
                            $fields[] = (string) $row[$col]; // cast to string (uses __toString function)
                            break;
                        case "resource":
                        case "resource (closed)": //as of PHP 7.2.0
                        case "unknown type":
                            throw new Exception("Cannot output variable of type '" . gettype($row[$col] . "' to CSV"), 500);
                    }
                } else {
                    $fields[] = null;
                }
            }
            
            $this->stream->write($this->formatCSVLine($fields, $delimiter));
        }
    }
    
    protected function formatCSVLine(array $fields, string $delimiter = ';')
    {
        return implode($delimiter, array_map(function ($field){
            return '"' . addslashes($field) . '"';
        }, $fields)) . PHP_EOL;
    }
}
