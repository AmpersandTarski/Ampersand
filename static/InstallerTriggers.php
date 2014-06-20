<?php


// Array for trigger queries that need to be installed
$queries = array();


// Trigger for DELETE Atom or Pair in function in Concept table
$queries['delete_<table name>'] = "CREATE TRIGGER `delete_<table name>` BEFORE DELETE ON `<table name>`
				FOR EACH ROW 
					BEGIN 
						DELETE FROM <other table> WHERE <other table>.<column name> = OLD.<column name>; 
						
					END";
					
$queries[] = "CREATE TRIGGER etc";




// Execute queries
foreach ($queries as $query){
 print $query."<br/>";
 // $db->Exe($query); 
 // print($db->error());

}

?>