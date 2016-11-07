<?php

namespace Ampersand\Helper;

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

?>