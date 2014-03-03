<?php

// Classes

class Database
{
	
	private $dblink;

	function __construct()
	{
		global $DB_link;
		$this->dblink = $DB_link;
		mysql_select_db('VP_Projects', $this->dblink); // $dbName from Generics.php
	}

	public function Exe($query)
	{
		$result = mysql_query($query,$this->dblink) OR die ("<br />\n<span style=\"color:red\">Query: $query UNsuccessful :</span> " . mysql_error() . "\n<br />");

		if ($result === false) return false;
		if ($result === true) return true;

		$resultarray = array();
		while(($resultarray[] = mysql_fetch_assoc($result)) || array_pop($resultarray));
		return $resultarray;
	}

	public function Escape($item)
	{
		if (is_object($item) OR is_array($item)) die("Escape item is not a variable but an object or array.");
		return mysql_escape_string($item);
	}

}


?>

