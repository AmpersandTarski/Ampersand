<?php
require_once('config.php');

error_reporting(E_ALL ^ E_DEPRECATED ^ E_NOTICE);

$db = Database::singleton();

// Get all the information for the edges:
$qry = "SELECT * FROM Edge LEFT JOIN viewEdges ON Edge.Edge=viewEdges.Edge WHERE viewEdges.View = '$_VIEW' AND (Edge.edgeNaar is NOT null AND Edge.edgeVan is NOT null)";
// Or, if we should focus on what's around 1 element:
if ($_FOCUS !== false) $qry = "SELECT * FROM Edge LEFT JOIN viewEdges ON Edge.Edge=viewEdges.Edge WHERE viewEdges.View = '$_VIEW' AND (Edge.edgeNaar = '$_FOCUS' OR Edge.edgeVan = '$_FOCUS') AND (Edge.edgeNaar is NOT null AND Edge.edgeVan is NOT null)";

// Edge: basic Edge info including source nodes, edgeNaar: contains target nodes, viewEdges: links edges to a certain view

$rows = $db->Exe($qry);

$edge = array();
$existingnodes = array();

foreach((array)$rows as $row)
{
	// Just throw everything in this row into the edge (yes this means the ID is given twice, doesn't matter.)
	$src = $row['edgeVan'];
	$dst = $row['edgeNaar'];

    $existingnodes[$src] = '';
    $existingnodes[$dst] = '';
	// Enrich data with the configData from config.php:
	AddConfigData($row,'edgeimage',$row['edgeSymbool']);
	AddConfigData($row,'linewidth',$row['edgeType']);
	AddConfigData($row,'statuscolour',$row['edgeStatus']);

	// A source node may have multiple destination nodes:
	if (isset($edge[$src])){
		$edge[$src][$dst] = $row; 
		// $edge[$src] = array_merge($edge[$src],array($dst=>$row)); 
	}else{
		$edge[$src] = array($dst=>$row);
	}
}

// Get all the information for the nodes:
$node = array();
$qry = "SELECT * FROM Node LEFT JOIN viewNodes ON Node.Node = viewNodes.Node WHERE View = '$_VIEW'";
if ($_FOCUS !== false)
{
    $nodelist = '(';
    foreach ($existingnodes as $node => $nothing) $nodelist .= "'$node',";
    $nodelist = rtrim($nodelist,',');
    $nodelist .= ')'; 
    // If the node is a single element, the list will be empty. Fix this by just showing the element:
    if ($nodelist == '()') $nodelist = "('$_FOCUS')";
    $qry = "SELECT * FROM Node LEFT JOIN viewNodes ON Node.Node = viewNodes.Node WHERE View = '$_VIEW' AND Node.nodeAtom in $nodelist";
}
$rows = $db->Exe($qry);

if (!is_array($rows)) $rows = array();
$node = array();
foreach ($rows as $row)
{
	// Enrich data with the configData from config.php:
	AddConfigData($row,'nodeimage',$row['nodeSymbool']);
	AddConfigData($row,'statuscolour',$row['nodeStatus']);
	AddConfigData($row,'url',$row['nodeType']); // nodeType

	$thisnode = $row['nodeAtom'];
	$node[$thisnode] = $row;
}

class export
{
	public $nodes;
    public $edges;
    public $config;
}

$export = new Export();
$export->nodes = $node;
$export->edges = $edge;
$export->config = isset($configData['config']) ? $configData['config'] : array();

// export:
echo json_encode($export,JSON_FORCE_OBJECT);



// This is the right format:
//$export->edges = array('Brazil'=>array('Argentina'=>array(),'Venezuela'=>array(),'North Africa'=>array()), 'Afghanistan'=>array('Ukraine'=>array(),'China'=>array(),'India'=>array(),'Middle East'=>array()));
//echo json_encode($export,JSON_FORCE_OBJECT);

function AddConfigData(&$row, $field, $value)
{
	global $configData;
	if (isset($configData[$field][$value])) $row[$field] = $configData[$field][$value];
	return false;
}

?>
