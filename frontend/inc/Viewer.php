<?php

class Viewer {
	
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
	
	public function getHtmlHead(){
		global $dbName;
		
		$reflection = new ReflectionClass($this);
		$dir = dirname($reflection->getFileName());	// $dir is set to the directory of the specific viewer (subclass of this class)
		
		$head .= '<title>'.$dbName.'</title>';
		
		// Meta tags
		$head .= '<meta name="viewport" content="width=device-width, initial-scale=1.0">';
		$head .= '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>';
		$head .= '<meta http-equiv="Pragma" content="no-cache">';
		$head .= '<meta http-equiv="no-cache">';
		$head .= '<meta http-equiv="Expires" content="-1">';
		$head .= '<meta http-equiv="cache-Control" content="no-cache">';
		
		// Bootstrap (boostrap.css before Viewer specific css files that overwrite bootstrap.css)
		$head .= '<link href="bootstrap/css/bootstrap.min.css" rel="stylesheet" media="screen">'; // TODO: move to /css folder of Viewer
		
		// CSS files
		$files = getDirectoryList($dir . DIRECTORY_SEPARATOR . 'css');
		foreach ((array)$files as $file){ 
			if (substr($file,-3) !== 'css') continue;
			$head .= '<link href="viewers/'.$reflection->getName().'/css/'.$file.'" rel="stylesheet" media="screen" type="text/css">';
		}
		
		// Javascript files
		$files = getDirectoryList($dir . DIRECTORY_SEPARATOR . 'js');
		foreach ((array)$files as $file){ 
			if (substr($file,-2) !== 'js') continue;
			$head .= '<script src="viewers/'.$reflection->getName().'/js/'.$file.'"></script>';
		}
		
		// Bootstrap (bootstrap.js requires Jquery (loaded above)
		$head .= '<script src="bootstrap/js/bootstrap.min.js"></script>'; // TODO: move to /js folder of Viewer
		
		return $head;
	
	}
	
	public function getHtmlBody(){
		$body .= '<body>';
		
		$body .= $this->getNavigationBar();
		
		$body .= '</body>';
		
		return $body;
	}
	
	protected function getNavigationBar(){
		$session = Session::singleton();
		
		$return .= '<div class="navbar navbar-default navbar-fixed-top">';
		
		$return .= '<div class="tno">
						<div class="container">
							<img src="images/tnotimeline.png" alt="TNO" />
						</div>
					</div>';
		
		$return .= '<div class="container">
						<ul class="nav nav-pills">';					
							
							foreach($session->role->getInterfaces(true) as $interface) 
		$return .= 			'<li id="'.Viewer::escapeHtmlAttrStr($interface->name).'">' // the interface attribute is there so we can style specific menu items with css
								.'<a href="?interface='.Viewer::escapeHtmlAttrStr($interface->name).'&atom=1">'
								.'<span class="glyphicon glyphicon-list-alt"></span> '.htmlSpecialChars($interface->name).'</a>'
							.'</li>';
					
		$return .=			'<li class="dropdown pull-right tooltip-to-be-initialized" data-toggle="tooltip" data-placement="right" title="Create new instances">'
								.'<a class="dropdown-toggle" data-toggle="dropdown" href="#"><span class="glyphicon glyphicon-plus"></span></a>'
								.'<ul class="dropdown-menu" role="menu">';
									foreach($session->role->getInterfaces(false) as $interface) 
		$return .=					'<li><a href="?interface='.$interface->name.'&atom='.$interface->srcConcept.'_'.time().'">'
									.htmlSpecialChars($interface->srcConcept . ' (' .$interface->name . ')').'</a></li>';
		$return .=				'</ul>'
							.'</li>';
					
		$return .=			'<li class="dropdown pull-right tooltip-to-be-initialized" data-toggle="tooltip" data-placement="top" title="Switch between different roles">'
								.'<a class="dropdown-toggle" data-toggle="dropdown" href="#"><span class="glyphicon glyphicon-user"></span></a>'
								.'<ul class="dropdown-menu" role="menu">';
									foreach(Role::getAllRoles() as $role) 
		$return .= 					'<li><a href="?role='.$role->id.'">'.htmlSpecialChars($role->name).'</a></li>';
		$return .= 				'</ul>'
							.'</li>';
					
		$return .= 			'<li class="dropdown pull-right tooltip-to-be-initialized" data-toggle="tooltip" data-placement="top" title="Select application extensions">'
								.'<a class="dropdown-toggle" data-toggle="dropdown" href="#"><span class="glyphicon glyphicon-th"></span></a>'
								.'<ul class="dropdown-menu" role="menu">';
									foreach((array)$GLOBALS['apps'] as $app)
		$return .= 					'<li><a href="'.$app['link'].'"><span class="'.$app['icon'].'"></span> '.htmlSpecialChars($app['name']).'</a></li>';
		$return .= 					'<li><a href="ampersand/installer.php"><span class="glyphicon glyphicon-trash"></span> Reset database</a></li>'
								.'</ul>'
							.'</li>';
					
							if(isset($GLOBALS['viewers'])){
		$return .= 			'<li class="dropdown pull-right tooltip-to-be-initialized" data-toggle="tooltip" data-placement="top" title="Switch between different viewers">'
								.'<a href="#" class="dropdown-toggle" data-toggle="dropdown"><span class="glyphicon glyphicon-eye-open"></span></a>'
								.'<ul class="dropdown-menu" role="menu">';
									foreach($GLOBALS['viewers'] as $key => $viewer) 
		$return .= 					'<li><a href="?viewer='.$key.'"><span class="'.$viewer['icon'].'"></span> '.htmlSpecialChars($viewer['name']).'</a></li>';
		$return .= 				'</ul>'
							.'</li>';
							}
							
		$return .= 			'<li class="pull-right tooltip-to-be-initialized" data-toggle="tooltip" data-placement="left" title="Hide or show violations messages">'
								.'<a href="javascript:void(0)" onclick="$( "#violations" ).toggleClass( "hidden" );"><span class="glyphicon glyphicon-bell messageIndicator"></span></a>'
							.'</li>'

						.'</ul>'
					.'</div>'
					.'</div>';
					
		return $return;
	}
	
	private function getMain(){
	
	
	}
	
	public static function escapeHtmlAttrStr($str) {
		return str_replace(array('"', '&'), array('&quot;', '%26'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
	}
	
}

?>