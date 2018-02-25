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
class CSVWriter extends AbstractWriter
{

    public function write($data)
    {
        $delimiter = ';';

        // Output the column headings (use first row to get column heading)
        $columns = array_keys($data[0]);
        fputcsv($this->stream, $columns, $delimiter);

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
            
            fputcsv($this->stream, $fields, $delimiter);
        }
    }
}
