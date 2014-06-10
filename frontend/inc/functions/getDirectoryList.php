<?php
// get the files from a folder
function getDirectoryList ($directory) 
{   // create an array to hold directory list
    $results = array();
    // create a handler for the directory
    $handler = opendir($directory);
    // open directory and walk through the filenames
    while ($file = readdir($handler))
    {   // if file isn't this directory or its parent, add it to the results
        if ($file != "." && $file != "..")
        {   $results[] = $file;
        }
    }
    // tidy up: close the handler
    closedir($handler);
    // done!
    return $results;
}

?>