<?php
require_once(__DIR__.'/../pluginsettings.php'); // configuration for ExecEngine and plugins
// We need the __DIR__ because all require statements are relative to the path of the browser-requested php file.
// Otherwise, when 'loadplugins' is included e.g. by Interface.php, we would need 'pluginsettings.php', but when included
// by php/Database.php, we would need '../pluginssettings.php'.

// Load the plugins from the plugins folder:
// (security hazard :P)
function getDirectoryList ($directory) 
{	// create an array to hold directory list
 	$results = array();
 	// create a handler for the directory
 	$handler = opendir($directory);
 	// open directory and walk through the filenames
 	while ($file = readdir($handler))
 	{	// if file isn't this directory or its parent, add it to the results
  		if ($file != "." && $file != "..")
  		{	$results[] = $file;
  		}
 	}
 	// tidy up: close the handler
 	closedir($handler);
 	// done!
 	return $results;
}

$files = getDirectoryList(__DIR__.'/plugins');
foreach ($files as $file)
{ if (substr($file,-3) !== 'php') continue;
  require_once __DIR__.'/plugins/'.$file;
  //echo "Included file: " . __DIR__ . '/plugins/' . $file . "\n<br/>";
}

?>