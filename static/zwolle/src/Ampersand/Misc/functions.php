<?php

namespace Ampersand\Misc;

/**
 * Get the filesnames from a folder
 * @param $dirPath
 * @return array
 */
function getDirectoryList($dirPath)
{   
    $results = array(); // create an array to hold directory list
    $handler = opendir($dirPath); // create a handler for the directory
    
    while ($file = readdir($handler)){ // open directory and walk through the filenames
        // if file isn't this directory or its parent, add it to the results
        if ($file != "." && $file != ".."){
            $results[] = $file;
        }
    }
    
    closedir($handler); // tidy up: close the handler
    
    return $results;
}

/**
 * Check if array is sequential (i.e. numeric keys, starting with 0, without gaps)
 * @param array $arr the array to check
 * @return boolean
 */
function isSequential(array $arr){
    return array_keys($arr) === range(0, count($arr) - 1);
}

?>