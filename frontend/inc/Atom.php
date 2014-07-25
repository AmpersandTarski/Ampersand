<?php

Class Atom {
	
	public $id;
	
	public function __construct($id){
		
		$this->id = $id;
		
	}
	
	public function getContent($interface){
		$session = Session::singleton();
		
		$content = array();
		$content['id'] = $this->id;
		$content['label'] = $this->id; // TODO: enable ampersand VIEWS here
		
		foreach($session->role->getInterfaces(null, $interface->tgtConcept) as $interfaceForTgtConcept){
			$content['links'][] = $interfaceForTgtConcept->link . '/atom/' . urlencode($this->id);
			$content['interfaces'][] = $interfaceForTgtConcept->name;
		}
		
		foreach ($interface->subInterfaces as $subInterface){
			
			$atoms = $subInterface->getContent($this->id);
			if(count($atoms) > 1){
				foreach((array)$atoms as $atom){
					$content[$subInterface->name][] = $atom;
				} 
			}elseif(count($atoms) == 1){
				$content[$subInterface->name] = $atoms;
			}else{
				$content[$subInterface->name] = array();
			}
		}
		
		return $content;
		
	}
	
}

?>