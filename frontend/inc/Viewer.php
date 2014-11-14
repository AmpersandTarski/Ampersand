<?php

class Viewer {
	
	private $html;
	
	public function __construct(){		
		
	}
	
	public function __tostring(){
		global $dbName;
		
		$this->addHtmlLine('<!DOCTYPE html>'."\n");  //TODO: "\n" op een of andere manier vervangen, of in functie zetten net als emit()
		$this->addHtmlLine('<html ng-app="AmpersandApp">');
		$this->addHtmlLine('head');
				
		$this->addHtmlLine('<title>'.$dbName.'</title>');
		
		// Meta tags
		$this->addHtmlLine('<meta name="viewport" content="width=device-width, initial-scale=1.0">');
		$this->addHtmlLine('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>');
		$this->addHtmlLine('<meta http-equiv="Pragma" content="no-cache">');
		$this->addHtmlLine('<meta http-equiv="no-cache">');
		$this->addHtmlLine('<meta http-equiv="Expires" content="-1">');
		$this->addHtmlLine('<meta http-equiv="cache-Control" content="no-cache">');
		
		// JQuery
		$this->addHtmlLine('<script src="lib/jquery/jquery-1.11.0.min.js"></script>');
		$this->addHtmlLine('<script src="lib/jquery/jquery-migrate-1.2.1.js"></script>');
		$this->addHtmlLine('<script src="lib/jquery/jquery-ui-1.10.4.custom.js"></script>');
		
		// Bootstrap
		$this->addHtmlLine('<link href="lib/bootstrap/css/bootstrap.min.css" rel="stylesheet" media="screen">'); // load boostrap.css before Viewer specific css files that overwrite bootstrap.css
		$this->addHtmlLine('<script src="lib/bootstrap/js/bootstrap.min.js"></script>'); // bootstrap.js requires Jquery (loaded above)
		
		// Angular
		$this->addHtmlLine('<script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.13/angular.min.js"></script>');
		$this->addHtmlLine('<script src="lib/angular/angular-resource.min.js"></script>');
		$this->addHtmlLine('<script src="lib/angular/angular-route.min.js"></script>');
				
		// CSS files
		$files = getDirectoryList('views/css');
		foreach ((array)$files as $file){ 
			if (substr($file,-3) !== 'css') continue;
			$this->addHtmlLine('<link href="views/css/'.$file.'" rel="stylesheet" media="screen" type="text/css">');
		}
		
		// Javascript files
		$files = getDirectoryList('views/js');
		foreach ((array)$files as $file){ 
			if (substr($file,-2) !== 'js') continue;
			$this->addHtmlLine('<script src="views/js/'.$file.'"></script>');
		}
		
		// AngularApp controler files
		$files = getDirectoryList('controllers');
		foreach ((array)$files as $file){ 
			if (substr($file,-2) !== 'js') continue;
			$this->addHtmlLine('<script src="controllers/'.$file.'"></script>');
		}
		
		$this->addHtmlLine('<script>var session_id = "'.session_id().'";</script>');
		
		$this->addHtmlLine('</head>');
		
		$this->addHtmlLine('<body>');
		
		$this->addHtmlLine(file_get_contents(__DIR__ . '/../views/AmpersandApp.html'));
		
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