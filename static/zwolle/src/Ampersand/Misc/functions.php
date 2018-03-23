<?php

namespace Ampersand\Misc;

/**
 * Get the filesnames from a folder
 * @param $dirPath
 * @return array
 */
function getDirectoryList($dirPath)
{
    $results = []; // create an array to hold directory list
    $handler = opendir($dirPath); // create a handler for the directory
    
    while ($file = readdir($handler)) { // open directory and walk through the filenames
        // if file isn't this directory or its parent, add it to the results
        if ($file != "." && $file != "..") {
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
function isSequential(array $arr)
{
    return array_keys($arr) === range(0, count($arr) - 1);
}

/**
 * Returns a filename (including path) that does not exists yet
 * Filename is appended with '_i' just before the extension (e.g. dir/file_1.txt)
 *
 * @param string $absolutePath
 * @return string
 */
function getSafeFileName(string $absolutePath): string
{
    if (!file_exists($absolutePath)) {
        return $absolutePath;
    }

    $dir = pathinfo($absolutePath, PATHINFO_DIRNAME);
    $filename = pathinfo($absolutePath, PATHINFO_FILENAME);
    $ext = pathinfo($absolutePath, PATHINFO_EXTENSION);

    $i = 1;
    while (file_exists($dir . DIRECTORY_SEPARATOR . "{$filename}_{$i}.{$ext}")) {
        $i++;
    }
    return $dir . DIRECTORY_SEPARATOR . "{$filename}_{$i}.{$ext}";
}

function stackTrace(Throwable $throwable): string
{
    $html = sprintf('<div><strong>Type:</strong> %s</div>', get_class($throwable));
    
    if (($code = $throwable->getCode())) {
        $html .= sprintf('<div><strong>Code:</strong> %s</div>', $code);
    }

    if (($message = $throwable->getMessage())) {
        $html .= sprintf('<div><strong>Message:</strong> %s</div>', htmlentities($message));
    }

    if (($file = $throwable->getFile())) {
        $html .= sprintf('<div><strong>File:</strong> %s</div>', $file);
    }

    if (($line = $throwable->getLine())) {
        $html .= sprintf('<div><strong>Line:</strong> %s</div>', $line);
    }

    if (($trace = $throwable->getTraceAsString())) {
        $html .= '<h2>Trace</h2>';
        $html .= sprintf('<pre>%s</pre>', htmlentities($trace));
    }
    return $html;
}
