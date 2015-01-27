<?php
// UI
$GLOBALS['hooks']['after_Viewer_load_cssFiles'][] = 'extensions/DndTree/ui/css/style.css';
$GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/DndTree/ui/js/DndTreeController.js';
$GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'http://d3js.org/d3.v3.min.js';
$GLOBALS['hooks']['after_Viewer_load_angularScripts'][] = 'extensions/DndTree/lib/dndTree.js';
