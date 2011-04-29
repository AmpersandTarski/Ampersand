<?php

$commandstr = array(
     1 => './adl --verbose -p'.COMPILATIONS_PATH.' --theme=student --import="'.$fullfile.'" --namespace='.USER.' --importformat=adl atlas',
     2 => './adl --verbose -o'.FILEPATH.' --export='.$file.' --namespace='.USER.' atlas',
     3 => './adl --verbose -o'.FILEPATH.' --export='.$file.' --namespace='.USER.' atlas',
     4 => './amp --verbose -o'.COMPILATIONS_PATH.' -f Latex "'.$fullfile.'"',
     5 => './adl --verbose -p'.COMPILATIONS_PATH.'proto/ --theme=student "'.$fullfile.'"',
     6 => './amp --verbose -o'.COMPILATIONS_PATH.' -f Latex --diagnosis "'.$fullfile.'"',
     99 => './adl --verbose "'.$fullfile.'"'
     );
$compileurl = array(
     1 => COMPILATIONS_PATH.'ctxAtlas.php?content=Contextoverzicht',
     2 => 'index.php?file='.$fullfile,
     3 => 'index.php?file='.$fullfile.'&operation=1',
     4 => COMPILATIONS_PATH.REQ_OUTPUT,
     5 => COMPILATIONS_PATH.'proto/index.htm',
     6 => COMPILATIONS_PATH.REQ_OUTPUT,
     99 => ''
     );

?>
