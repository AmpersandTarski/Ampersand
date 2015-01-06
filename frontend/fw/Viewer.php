<?php

class Viewer {
	
	private $html;
	
	public function __construct(){		
		
	}
	
	public function __tostring(){
		global $dbName;
		
		$this->addHtmlLine("<!doctype html>"."\n");  //TODO: "\n" op een of andere manier vervangen
		$this->addHtmlLine('<html ng-app="AmpersandApp">');
		$this->addHtmlLine('<head>');
				
		$this->addHtmlLine('<title>'.$dbName.'</title>');
		
		// Meta tags
		$this->addHtmlLine('<meta name="viewport" content="width=device-width, initial-scale=1.0"/>');
		$this->addHtmlLine('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>');
		$this->addHtmlLine('<meta http-equiv="Pragma" content="no-cache"/>');//TODO: kan weg?
		$this->addHtmlLine('<meta http-equiv="no-cache"/>');//TODO: kan weg?
		$this->addHtmlLine('<meta http-equiv="Expires" content="-1"/>'); //TODO: kan weg?
		$this->addHtmlLine('<meta http-equiv="cache-Control" content="no-cache"/>');//TODO: kan weg?
		
		// JQuery
		$this->addHtmlLine('<script src="app/lib/jquery/jquery-1.11.0.min.js"></script>');
		$this->addHtmlLine('<script src="app/lib/jquery/jquery-migrate-1.2.1.js"></script>');
		$this->addHtmlLine('<script src="app/lib/jquery/jquery-ui-1.10.4.custom.js"></script>');		
		
		// Bootstrap
		$this->addHtmlLine('<link href="app/lib/bootstrap/css/bootstrap.min.css" rel="stylesheet" media="screen">'); // load boostrap.css before Viewer specific css files that overwrite bootstrap.css
		$this->addHtmlLine('<script src="app/lib/bootstrap/js/bootstrap.min.js"></script>'); // bootstrap.js requires Jquery (loaded above)
		
		// Angular
		$this->addHtmlLine('<script src="app/lib/angular/angular.min.js"></script>');
		$this->addHtmlLine('<script src="app/lib/angular/angular-resource.min.js"></script>');
		$this->addHtmlLine('<script src="app/lib/angular/angular-route.min.js"></script>');
		$this->addHtmlLine('<script src="app/lib/angular/angular-filter.min.js"></script>');
		
		// Restangular (with depency for lodash)
		$this->addHtmlLine('<script src="app/lib/restangular/restangular.min.js"></script>');
		$this->addHtmlLine('<script src="app/lib/restangular/lodash.min.js"></script>');
		
		// jquery UI & bootstrap in native AngularJS
		$this->addHtmlLine('<script src="app/lib/ui-bootstrap/ui-bootstrap-tpls-0.12.0.min.js"></script>');
		
		
		// CSS files
		$files = getDirectoryList('app/css');
		foreach ((array)$files as $file){ 
			if (substr($file,-3) !== 'css') continue;
			$this->addHtmlLine('<link href="app/css/'.$file.'" rel="stylesheet" media="screen" type="text/css">');
		}
		
		// Javascript files
		$files = getDirectoryList('app/js');
		foreach ((array)$files as $file){ 
			if (substr($file,-2) !== 'js') continue;
			$this->addHtmlLine('<script src="app/js/'.$file.'"></script>');
		}
		
		$this->addHtmlLine('<script src="app/AmpersandApp.js"></script>');
		// AngularApp controler files
		$files = getDirectoryList('app/controllers');
		foreach ((array)$files as $file){ 
			if (substr($file,-2) !== 'js') continue;
			$this->addHtmlLine('<script src="app/controllers/'.$file.'"></script>');
		}		
		
		$this->addHtmlLine('<script>var session_id = "'.session_id().'";</script>');
		
		$this->addHtmlLine('</head>');
		
		$this->addHtmlLine('<body>');
		
		$this->addHtmlLine(file_get_contents(__DIR__ . '/../app/AmpersandApp.html'));
		
		$this->addHtmlLine('</body>');
		
		$this->addHtmlLine('</html>');
		
		return $this->html;
	
	}
	
	private function addHtmlLine($htmlLine){
		$this->html .= $htmlLine;
	}
	
	// TODO: make non static or private function
	public static function viewAtom($atom, $srcConcept){
		global $allViews; // from Generics.php
		$database = Database::singleton();
		
		foreach ($allViews as $view){
			if($view['concept'] == $srcConcept){
				$viewStrs = array ();
				foreach ($view['segments'] as $viewSegment) 
					if ($viewSegment['segmentType'] == 'Text')
						$viewStrs[] = htmlSpecialChars($viewSegment['Text']);
					elseif ($viewSegment['segmentType'] == 'Html')
						$viewStrs[] = $viewSegment['Html'];
					else {
						$query = "SELECT DISTINCT `tgt` FROM (".$viewSegment['expSQL'].") AS results WHERE src='".addslashes($atom)."'";
						$rows = $database->Exe($query);
						$txt = count($rows) ? $rows[0]['tgt'] : "<View relation not total>"; // this can happen in a create-new interface when the view fields have not yet been filled out.
						$viewStrs[] = htmlSpecialChars($txt);
					}
				return implode($viewStrs);
			}		
		}
		return $atom; // in case no view exists for this $srcConcept
	}
}

?>