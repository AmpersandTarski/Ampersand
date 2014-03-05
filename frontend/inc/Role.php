<?php

class Role {

	public $id;
	public $name;
	public $maintains = array();
	public $interfaces = array();

	public function __construct($id){
		global $allRoles; // from Generics.php
		global $allInterfaceObjects; // from Generics.php
		
		$this->id = $id;
		$this->name = $allRoles[$id]['name'];
		$this->maintains = $allRoles[$id]['ruleNames'];
		
		foreach ($allInterfaceObjects as $interfaceName => $interface){
			if (in_array($this->name, $interface['interfaceRoles'])) $this->interfaces[] = $interfaceName;
		}
		
	}
		
	public function getRules(){
		$rules = array();
		
		foreach($this->maintains as $ruleName){
			$rules[$ruleName] = Session::getRule($ruleName);
		}
		
		return $rules;
	}
	
	public function getInterfaces($srcConceptONE = null){
		$interfaces = array();
		
		foreach($this->interfaces as $interfaceName){
			switch ($srcConceptONE){
				case true :
					if(Session::getInterface($interfaceName)['srcConcept'] == 'ONE') $interfaces[$interfaceName] = Session::getInterface($interfaceName);
					break;
				case false :
					if(Session::getInterface($interfaceName)['srcConcept'] != 'ONE') $interfaces[$interfaceName] = Session::getInterface($interfaceName);
					break;
				case null :
					$interfaces[$interfaceName] = Session::getInterface($interfaceName);
					break;
			}			
		}
		
		return $interfaces;
	}

}


?>