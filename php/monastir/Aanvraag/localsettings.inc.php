<?
	
	// selecting the monastir view
	
	$incPath = "../inc/";
	// globalsettings will yield DB_debug(), $DB_debug, $DB_host, $DB_user, $DB_pass, $imageDirName, $DB_errs, DB_doquer()
	require $incPath."globalsettings.inc.php";
	
	$dbName = "Aanvraag"; // mysql internal name
	$appName = "Aanvraag"; // full utf-8 name
	
	// include default VIEW elements
	require $incPath."monastir.inc.php";

	$DB_link = @mysql_connect($DB_host,$DB_user,$DB_pass) or die('Could not connect to MySql.');
	$DB_slct = mysql_select_db($dbName,$DB_link);
	
	require "maintainDataBase.php";
	
	
	class view Extends monastir {
		function view(){
			global $imageDirName;
			global $appName;
			// the menu is part of the view
			
			$menu = array(); // menuItem(url, title, class, link-caption)
			$menu[] = array
			  (new menuItem('behandelaar.php','Show all behandelaar objects','menuItem','behandelaar')
			  ,new menuItem('permissions.php','Show all permissions objects','menuItem','permissions')
			  ,new menuItem('person.php','Show all person objects','menuItem','person')
			  ,new menuItem('application.php','Show all application objects','menuItem','application')
			  ,new menuItem('decision.php','Show all decision objects','menuItem','decision')
			  );
			$menu[] = new menuItem('index.php','Show the index object','menuItem','index');
			// the menu is the same for all pages
			$this->assign('menu',$menu);
			$this->assign("iDir",$imageDirName);
			$this->assign("appname",$appName);
		}
	}
		
?>