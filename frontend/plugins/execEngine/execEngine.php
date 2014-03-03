<?php
require_once __DIR__.'/../../db/Database.php';
require_once(__DIR__.'/config.php'); // configuration for ExecEngine and functions
// We need the __DIR__ because all require statements are relative to the path of the browser-requested php file.

// Load the functions from the functions folder:
// (security hazard :P)
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

$files = getDirectoryList(__DIR__.'/functions');
foreach ($files as $file)
{ if (substr($file,-3) !== 'php') continue;
  require_once __DIR__.'/functions/'.$file;
  //echo "Included file: " . __DIR__ . '/functions/' . $file . "\n<br/>";
}

?>