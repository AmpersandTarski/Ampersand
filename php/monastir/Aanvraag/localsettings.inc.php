<?
	// selecting the monastir view
	$incPath = "../inc/";
	$dbName = "Aanvraag"; // mysql internal name
	$appName = "Aanvraag"; // full text name
	require $incPath."globalsettings.inc.php";
	require "connectToDataBase.inc.php";
	require $incPath."monastir.inc.php";
	
	class view Extends monastir {
		function view($obj,$object){
			global $imageDirName;
			global $appName;
			global $action;
			global $actionValue;
			$this->action=$action;
			$menu = array(); // menuItem(url, title, class, link-caption)
			$menu[] = array
			  (new menuItem('behandelaar.php','Show all behandelaar objects','menuItem','behandelaar')
			  ,new menuItem('permissions.php','Show all permissions objects','menuItem','permissions')
			  ,new menuItem('person.php','Show all person objects','menuItem','person')
			  ,new menuItem('application.php','Show all application objects','menuItem','application')
			  ,new menuItem('decision.php','Show all decision objects','menuItem','decision')
			  );
			$menu[] = new menuItem('index.php','Show the index object','menuItem','index');
			parent::monastir($menu,$obj,$object,ucfirst($object->name));

		}
	}	
?>