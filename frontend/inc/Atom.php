<?php

Class Atom {
	
	public $id;
	
	public function __construct($id){
		
		$this->id = $id;
		
	}
	
	public function getContent($interface){
		$content = array();
		$content['id'] = $this->id;
		
		foreach ($interface->subInterfaces as $subInterface){
			
			$atoms = $subInterface->getContent($this->id);
			if(count($atoms) > 1){
				foreach((array)$atoms as $atom){
					$content[$subInterface->name][] = $atom;
				} 
			}else{
				$content[$subInterface->name] = $atoms;
			}
		}
		
		return $content;
		
	}
	
}

?>