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
		$this->maintains = (array)$allRoles[$id]['ruleNames'];
		
		foreach ($allInterfaceObjects as $interfaceName => $interface){
			if (in_array($this->name, $interface['interfaceRoles']) or empty($interface['interfaceRoles'])) $this->interfaces[] = $interfaceName;
		}
		
	}
		
	public function getRules(){
		$rules = array();
		
		foreach($this->maintains as $ruleName){
			$rules[$ruleName] = Session::getRule($ruleName);
		}
		
		return $rules;
	}
	
	public function getInterfaces($srcConceptONE = null, $srcConcept = null){
		$interfaces = array();
		
		foreach($this->interfaces as $interfaceName){
			if(isset($srcConceptONE)){
				switch ($srcConceptONE){
					case true :
						if(Session::getInterface($interfaceName)['srcConcept'] == 'ONE') $interfaces[$interfaceName] = Session::getInterface($interfaceName);
						break;
					case false :
						if(Session::getInterface($interfaceName)['srcConcept'] != 'ONE') $interfaces[$interfaceName] = Session::getInterface($interfaceName);
						break;
					
				}
			}else{
				if(isset($srcConcept)){
					if(Session::getInterface($interfaceName)['srcConcept'] == $srcConcept) $interfaces[$interfaceName] = Session::getInterface($interfaceName);
				}else{
					$interfaces[$interfaceName] = Session::getInterface($interfaceName);
				}
			}
		}
		
		return $interfaces;
	}
	
	public function isInterfaceForRole($interfaceName){
		return in_array($interfaceName, $this->interfaces);
	}

}


?>