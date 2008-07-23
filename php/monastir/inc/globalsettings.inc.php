<?

// this file contains all code that is shared between both applications and views

$DB_debug = 3; // 0 = show nothing except failures, 5 = show everything
$DB_user = 'root';
$DB_pass = ''; // if you wish, you can use $DB_pass = base64_decode('your_base64_encoded_password_here'); to decrease readability
$DB_host = 'localhost';
// If you wish to use other settings for each database, copy these lines to the localsettings.inc.php file just below the globalsettings.inc.php include

$imageDirName = '../infeez/';
function DB_debug($txt,$lvl=0){
	global $DB_debug;
	if($lvl<=$DB_debug) {
		echo "<i title=\"debug level $lvl\">$txt</i>\n<P />\n";
		return true;
	}else return false;
}

$DB_errs = array();
// wrapper function for MySQL
function DB_doquer($quer,$debug=5)
{
	global $DB_link,$DB_errs;
	DB_debug($quer,$debug);
	$result=mysql_query($quer,$DB_link);
	if(!$result){
	  DB_debug('Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'"',2);
	  $DB_errs[]='Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'"';
	  return false;
	}
	$rows=Array();
	while (($row = @mysql_fetch_array($result))!==false) {
	  $rows[]=$row;
	  //print_r($row);
	  //echo '<br />';
	  unset($row);
	}
	return $rows;
}
function DB_plainquer($quer,&$errno,$debug=5)
{
	global $DB_link,$DB_errs,$DB_lastquer;
	$DB_lastquer=$quer;
	DB_debug($quer,$debug);
	$result=mysql_query($quer,$DB_link);
	if(!$result){
	 // DB_debug('Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'"',2);
	 // $DB_errs[]='Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'"';
	  $errno=mysql_errno($DB_link);
	  return false;
	}else{
		if(($p=stripos($quer,'INSERT'))!==false
		   && (($q=stripos($quer,'UPDATE'))==false || $p<$q)
		   && (($q=stripos($quer,'DELETE'))==false || $p<$q)
		  )
		{
			return mysql_insert_id();
		} else return mysql_affected_rows();
	}
}

// Code from php.net, for those who have get_magic_quotes_gpc turned ON...
if ( get_magic_quotes_gpc() ) {
   function stripslashes_deep($value) {
	   $value = is_array($value) ? array_map('stripslashes_deep', $value) : stripslashes($value);
	   return $value;
   }
   $_POST = stripslashes_deep($_POST);
   $_REQUEST = stripslashes_deep($_REQUEST);
   $_GET = stripslashes_deep($_GET);
   $_COOKIE = stripslashes_deep($_COOKIE);
}
// End of php.net-code


// object definition
class object {
	var $name;
	var $page;
	var $containing;
	function object($name,array $containing=array(),$page=NULL){
		$this->name=$name;
		$this->page=$page;
		$this->containing=$containing;
	}
	function containsObject(oRef $container){
		$this->containing[]=$container;
	}
}
class oRef {
	var $mult;
	var $type;
	function oRef(oMulti $multiplicity,object $type = NULL){
		$this->mult=$multiplicity;
		$this->type=$type;
	}
}
class oMulti {
	var $inj;var $sur;var $uni;var $tot;
	function oMulti
		($inj // each TARGET is used at most once -> only select from existing if it is not used
		,$uni // there is at most one TARGET -> no double values allowed
		,$sur // each TARGET is used at least once -> removing it may fail
		,$tot // there must be a TARGET
		){
			$this->inj=$inj;$this->sur=$sur;$this->uni=$uni;$this->tot=$tot;
	}
}

?>