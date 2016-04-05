<?php

//namespace Ampersand\Helper;
// TODO: add namespace. From php 5.6+ functions can be imported with 'use function' statement

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
 * Finds whether an array is associative (true) or numeric (false)
 * @param array $arr
 * @return boolean
 */
function isAssoc($arr)
{
    if(is_array($arr) && empty($arr)) return false; // empty array is  considered not associative

    return array_keys($arr) !== range(0, count($arr) -1);
}

?>