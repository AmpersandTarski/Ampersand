<?PHP
require_once __DIR__.'/../config.php';
require_once __DIR__.'/../../../Installer.php';

$db = Database::singleton();

function inMultiArray($needle, $haystack){

  foreach($haystack as $key => $arr){
        if(in_array($needle, $arr)) return true;        
  }
  return false;
}

global $relationTableInfo, $conceptTableInfo, $tableColumnInfo;

$queries = array();


// Tabelstructuur voor tabel `viewNodes`
$queries[] = "DROP TABLE IF EXISTS `viewNodes`";
$queries[] = "CREATE TABLE IF NOT EXISTS `viewNodes` (
  `View` varchar(255) DEFAULT NULL,
  `Node` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8";

// Tabelstructuur voor tabel `Edge`
$queries[] = "DROP TABLE IF EXISTS `Edge`";
$queries[] = "CREATE TABLE IF NOT EXISTS `Edge` (
  `Edge` int(11) NOT NULL AUTO_INCREMENT,
  `edgeType` varchar(255) DEFAULT NULL,
  `edgeTitel` varchar(255) DEFAULT NULL,
  `edgeSymbool` varchar(255) DEFAULT NULL,
  `edgeStatus` varchar(255) DEFAULT NULL,
  `edgeVan` varchar(255) DEFAULT NULL,
  `edgeNaar` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`Edge`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ";

// Triggers `Edge`
$queries[] = "DROP TRIGGER IF EXISTS `delete_viewEdges`";
$queries[] = "CREATE TRIGGER `delete_viewEdges` BEFORE DELETE ON `Edge`
 FOR EACH ROW BEGIN DELETE FROM viewEdges WHERE Edge = OLD.Edge; END";
 
// Tabelstructuur voor tabel `Node`
$queries[] = "DROP TABLE IF EXISTS `Node`";
$queries[] = "CREATE TABLE IF NOT EXISTS `Node` (
  `Node` int(11) NOT NULL AUTO_INCREMENT,
  `nodeType` varchar(255) DEFAULT NULL,
  `nodeAtom` varchar(255) DEFAULT NULL,
  `nodeTitel` varchar(255) DEFAULT NULL,
  `nodeSymbool` varchar(255) DEFAULT NULL,
  `nodeStatus` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`Node`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1 ";

// Triggers `Node`
$queries[] = "DROP TRIGGER IF EXISTS `delete_viewNodes`";
$queries[] = "CREATE TRIGGER `delete_viewNodes` BEFORE DELETE ON `Node`
 FOR EACH ROW BEGIN DELETE FROM viewNodes WHERE Node = OLD.Node; END";

// Tabelstructuur voor tabel `viewEdges`
$queries[] = "DROP TABLE IF EXISTS `viewEdges`";
$queries[] = "CREATE TABLE IF NOT EXISTS `viewEdges` (
  `View` varchar(255) DEFAULT NULL,
  `Edge` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8";


// CREATE TRIGGERS FOR EDGES
foreach ($relationTableInfo as $relation => $value){

    if(inMultiArray($relation, $mappingViewRelation)){
    $table = $value['table'];
    $srcCol = $value['srcCol'];
    $tgtCol = $value['tgtCol'];
  
$queries[] = "DROP TRIGGER IF EXISTS insert_".$relation;
$query = "
CREATE TRIGGER insert_".$relation." 
AFTER INSERT ON ".$table."  
FOR EACH ROW 
BEGIN
 DECLARE lastid INT DEFAULT 0; 
 
	INSERT INTO Edge 
	(edgeType, edgeSymbool, edgeStatus, edgeVan, edgeNaar) 
	VALUES ('".$relation."', '".$relation."Symbool', '".$relation."Status', NEW.".$srcCol.", NEW.".$tgtCol.");
	
	SET lastid = LAST_INSERT_ID();";
	
	foreach ($mappingViewRelation as $view => $relations){
	
	  foreach ($relations as $value){
	    if($value == $relation){
	    
	       $query .= "INSERT INTO viewEdges
                	(View, Edge)
                	VALUES ('".$view."', lastid); ";
	    }
	  } 
 }

$query .= "END";
$queries[] = $query;

$queries[] = "DROP TRIGGER IF EXISTS delete_".$relation;

$queries[] = "CREATE TRIGGER delete_".$relation." 
BEFORE DELETE ON ".$table." 
FOR EACH ROW 
BEGIN
	DELETE FROM Edge 
	WHERE edgeType = '".$relation."' 
	AND edgeVan = OLD.".$srcCol." 
	AND edgeNaar = OLD.".$tgtCol.";

END";

$queries[] = "DROP TRIGGER IF EXISTS update_".$relation;

$queries[] = "CREATE TRIGGER update_".$relation." 
AFTER UPDATE ON ".$table." 
FOR EACH ROW 
BEGIN
	UPDATE Edge SET edgeVan = NEW.".$srcCol.", edgeNaar = NEW.".$tgtCol."
	WHERE edgeType = '".$relation."' 
	AND edgeVan = OLD.".$srcCol." 
	AND edgeNaar = OLD.".$tgtCol." 
	LIMIT 1;

END";

  }
}
$queries[] = "DROP TRIGGER IF EXISTS delete_viewEdges";

$queries[] = "CREATE TRIGGER delete_viewEdges 
BEFORE DELETE ON Edge 
FOR EACH ROW 
BEGIN
	DELETE FROM viewEdges
	WHERE Edge = OLD.Edge;

END"; 

// CREATE TRIGGERS FOR NODES
foreach ($conceptTableInfo as $concept => $value){

    if(inMultiArray($concept, $mappingViewConcepts)){
    
    
    $table = $value[0]['table'];
    $idCol = $value[0]['cols'][0];
    $cols = array_keys($tableColumnInfo[$table]);
    
    $statusCol = "NULL";
	$nameCol = "NULL";
    foreach ($cols as $col){
      if( substr($col, -6) == "Status")  { 
          $statusCol = "NEW.".$col;
          continue;
         }
	  if( substr($col, -4) == "Name") 	{
		  $nameCol = "NEW.".$col;
		  continue;	  
	  }
    }
  
$queries[] = "DROP TRIGGER IF EXISTS insert_".$concept;

$query = "CREATE TRIGGER insert_".$concept." 
AFTER INSERT ON ".$table."  
FOR EACH ROW 
BEGIN
 DECLARE lastid INT DEFAULT 0; 
 
	INSERT INTO Node 
	(nodeType, nodeSymbool, nodeStatus, nodeTitel, nodeAtom ) 
	VALUES ('".$concept."', '".$concept."Symbool', ".$statusCol.", ".$nameCol.", NEW.".$idCol.");
	
	SET lastid = LAST_INSERT_ID();";
	
	foreach ($mappingViewConcepts as $view => $concepts){
	
	  foreach ($concepts as $value){
	    if($value == $concept){
	    
	       $query .= "INSERT INTO viewNodes
                	(View, Node)
                	VALUES ('".$view."', lastid);";
	    }
	  } 
 }

$query .= "END";

$queries[] = $query;

$queries[] = "DROP TRIGGER IF EXISTS delete_".$concept;

$queries[] = "CREATE TRIGGER delete_".$concept." 
BEFORE DELETE ON ".$table." 
FOR EACH ROW 
BEGIN
	DELETE FROM Node 
	WHERE nodeType = '".$concept."' 
	AND nodeAtom = OLD.".$idCol.";

END";

$queries[] = "DROP TRIGGER IF EXISTS update_".$concept;

$queries[] = "CREATE TRIGGER update_".$concept." 
AFTER UPDATE ON ".$table." 
FOR EACH ROW 
BEGIN
	UPDATE Node SET nodeStatus = ".$statusCol.", nodeTitel = ".$nameCol." 
	WHERE nodeType = '".$concept."' 
	AND nodeAtom = OLD.".$idCol." 
	LIMIT 1;

END";

  }
}

$queries[] = "DROP TRIGGER IF EXISTS delete_viewNodes";

$queries[] = "CREATE TRIGGER delete_viewNodes 
BEFORE DELETE ON Node 
FOR EACH ROW 
BEGIN
	DELETE FROM viewNodes
	WHERE Node = OLD.Node;

END"; 


// do query
foreach ($queries as $query){
 print $query."<br/>";
 $db->Exe($query); 
 print($db->error());

}


?>
