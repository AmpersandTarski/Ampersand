<?php

require_once (__DIR__ . '/lib/Mustache/Autoloader.php');
Mustache_Autoloader::register();

$viewers['DefaultViewer'] = array('name' => 'Default viewer', 'class' => 'DefaultViewer', 'icon' => ''); // activeer viewer extension in framework

class DefaultViewer extends Viewer {
	
	private $mustache;
	private $data;

	public function __construct($interfaceName, $atomId = null){
		$this->mustache = new Mustache_Engine(array('loader' => new Mustache_Loader_FilesystemLoader(__DIR__ . '/templates/default/'), // TODO: make config for templates folder
													'partials_loader' => new Mustache_Loader_FilesystemLoader(__DIR__ . '/templates/default/partials/')
											));
		// `{{> foo }}` would look for a partial in `templates/partials/foo.mustache`
				
		$interface = new UserInterface($interfaceName);
		
		$this->data = $interface->getAtomsAndLinks($atomId);
	}
	
	public function getView($template = 'default'){ // TODO: make config for default template.
			
		return $this->mustache->render($template, $this->data); // Mustache loader automatically adds .mustache extension 
	
	}

}

?>