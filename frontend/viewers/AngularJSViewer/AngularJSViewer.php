<?php

$viewers['AngularJSViewer'] = array('name' => 'Angular viewer', 'class' => 'AngularJSViewer', 'icon' => ''); // activeert viewer extension in framework

class AngularJSViewer extends Viewer {
	
	private $interface;
	private $atomId;
	
	public function __construct($interface, $atomId = null){		
		
		$this->htmlTag = '<html ng-app="AmpersandApp">'; // denotates that it is a angular app
		
		$this->addHtmlHeaderLine('<script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.13/angular.min.js"></script>');
		$this->addHtmlHeaderLine('<script type="text/javascript">var interface = "'.$interface->name.'"; var atom = "'.$atomId.'";</script>');
		
		
		$this->interface = $interface;
		$this->atomId = $atomId;
		
		parent::__construct(); // call parent constructor
		
	}
	
	protected function buildHtmlBody(){		
		$this->addHtmlBodyLine($this->getNavigationBar());
		
		$this->addHtmlBodyLine('<div class="container mainview">');
		$this->addHtmlBodyLine($this->getNotifications());
		if(isset($this->interface)) $this->addHtmlBodyLine($this->getView());
		$this->addHtmlBodyLine('</div>');
			
		$this->addHtmlBodyLine('<script src="extensions/statusColors/js/statusColors.js"></script>');
		$this->addHtmlBodyLine('<script>$(\'.tooltip-to-be-initialized\').toggleClass(\'tooltip-to-be-initialized\').tooltip();</script>'); // TODO: verplaatsen naar Angular??
		
	}
	
	protected function getView(){
		
		$html = file_get_contents(__DIR__ . '/templates/AmpersandApp.html');
			
		return $html;
		
	}

}

?>