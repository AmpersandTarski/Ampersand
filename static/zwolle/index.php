<?php

$html = file_get_contents('app' . DIRECTORY_SEPARATOR . 'index.html');

if (strpos($html, '<!--[PROJECT_PLACEHOLDER]-->')) {
    $replace = '';

    // Project style sheet
    if (file_exists('app/dist/project.min.css')) {
        $replace .= "<link href=\"app/dist/project.min.css\" rel=\"stylesheet\" media=\"screen\" type=\"text/css\"/>" . PHP_EOL;
    }

    // Project javascript files
    if (file_exists('app/dist/project.min.js')) {
        $replace .= "<script src=\"app/dist/project.min.js\"></script>" . PHP_EOL;
    } else {
        if ($files = glob('app/project/*.js')) {
            foreach ($files as $filepath) {
                $replace .= "<script src=\"{$filepath}\"></script>" . PHP_EOL;
            }
        }
    }
    $html = str_replace('<!--[PROJECT_PLACEHOLDER]-->', $replace, $html);
}

echo $html;
