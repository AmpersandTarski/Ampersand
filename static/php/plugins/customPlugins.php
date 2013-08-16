<?
define('DEBUGMSG', true);

// These are some examples of what you can do with execution-engine plugins
// This was the first attempt of execution-engine plugins.
// Note that they 'know' about the internals of the database.
// Look at the generic plugins file for 'second generationn' stuff.

function SetComputerStatus($id, $status)
{

	// First check if this is a Computer:
	$query = "SELECT * FROM Computer WHERE compID = \"$id\"";
	if (DB_doquerErr($query,$error) !== 0)
	{
		$query = "UPDATE Computer SET compStatus = \"$status\" WHERE compID = \"$id\"";
		DB_doquerErr($query,$error);
		if (DEBUGMSG) emitAmpersandErr("- Automatically set status of '$id' to '$status'.");
	}else{
		if (DEBUGMSG) emitAmpersandErr("- $id is not a computer. Error.");
	}
}

function SetNetworkStatus($id, $status)
{
	// First check if this is a Network:
	$query = "SELECT * FROM Network WHERE nwID = \"$id\"";
	if (DB_doquerErr($query,$error) !== 0)
	{
		$query = "UPDATE Network SET nwStatus = \"$status\" WHERE nwID = \"$id\"";
		DB_doquerErr($query,$error);
		echo "- Automatically set status of '$id' to '$status'.";
	}else{
		echo "$id is not a network. Error.";
	}
}

function SetRouterStatus($id, $status)
{
	// First check if this is a Router:
	$query = "SELECT * FROM Router WHERE routerID = \"$id\"";
	if (DB_doquerErr($query,$error) !== 0)
	{
		$query = "UPDATE Router SET routerStatus = \"$status\" WHERE routerID = \"$id\"";
		DB_doquerErr($query,$error);
		echo "- Automatically set status of '$id' to '$status'.";
	}else{
		echo "$id is not a router. Error.";
	}
}

function SetSvcComponentStatus($id, $status)
{
	// First check if this is a SvcComponent:
	$query = "SELECT * FROM SvcComponent1 WHERE scID = \"$id\"";
	if (DB_doquerErr($query,$error) !== 0)
	{
		$query = "UPDATE SvcComponent1 SET scStatus = \"$status\" WHERE scID = \"$id\"";
		DB_doquerErr($query,$error);
		echo "- Automatically set status of '$id' to '$status'.";
	}else{
		echo "$id is not a servicecomponent. Error.";
	}
}

function SetServiceStatus($id, $status)
{
	// First check if this is a Service:
	$query = "SELECT * FROM Service WHERE svcID = \"$id\"";
	if (DB_doquerErr($query,$error) !== 0)
	{
		$query = "UPDATE Service SET svcStatus = \"$status\" WHERE svcID = \"$id\"";
		DB_doquerErr($query,$error);
		echo "- Automatically set status of '$id' to '$status'.";
	}else{
		echo "$id is not a service. Error.";
	}
}

function RemoveRoute($src, $tgt)
{
	$query = "SELECT Router FROM Router WHERE routerID = \"$src\"";
	$data = DB_doquerErr($query,$error);
	$srcid = $data[0]['Router'];
	
	$query = "SELECT Router FROM Router WHERE routerID = \"$tgt\"";
	$data = DB_doquerErr($query,$error);
	$tgtid = $data[0]['Router'];

	$query = "DELETE FROM routerRoutesTo WHERE sRouter = \"$srcid\" AND tRouter = \"$tgtid\"";
	DB_doquerErr($query,$error);
	echo "- Automatically removed route between '$src' ($srcid) and '$tgt' ($tgtid).";
}


function DisconnectComputerFromRouter($src, $tgt)
{
	$query = "SELECT Computer FROM Computer WHERE compID = \"$src\"";
	$data = DB_doquerErr($query,$error);
	$srcid = $data[0]['Computer'];

	$query = "SELECT Router FROM Router WHERE routerID = \"$tgt\"";
	$data = DB_doquerErr($query,$error);
	$tgtid = $data[0]['Router'];

	$query = "DELETE FROM compRouter WHERE Computer = \"$srcid\" AND Router = \"$tgtid\"";
	DB_doquerErr($query,$error);
	echo "- Automatically removed computer '$src' ($srcid) from router '$tgt' ($tgtid).";
}


?>
