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
			if (UserInterface::isInterfaceForRole($this->name, $interfaceName)) $this->interfaces[] = $interfaceName;
		}
		
	}
		
	public static function getAllRoles(){
		$roles = array();
		global $allRoles; // from Generics.php
		
		foreach((array)$allRoles as $key => $arr){
			$roles[$key] = new Role($key);
		}
		
		return $roles;
	}
	
	public function getRules(){
		$rules = array();
		
		foreach($this->maintains as $ruleName){
			$rules[$ruleName] = Session::getRule($ruleName);
		}
		
		return $rules;
	}
	
	public function getInterfaces($srcConceptONE = null, $srcConcept = null){ // $srcConceptONE: true, false, null (=all), $srcConcept: <concept> or null (=all)
		$interfaces = array();
		
		foreach($this->interfaces as $interfaceName){
			$interface = new UserInterface($interfaceName);
			
			if(isset($srcConceptONE)){
				switch ($srcConceptONE){
					case true :
						if($interface->srcConcept == 'ONE') $interfaces[$interfaceName] = $interface;
						break;
					case false :
						if($interface->srcConcept != 'ONE') $interfaces[$interfaceName] = $interface;
						break;
				}
			}else{
				if(isset($srcConcept)){
					if($interface->srcConcept == $srcConcept 
						|| in_array($srcConcept, Concept::getSpecializations($interface->srcConcept)) ) {
						
						$interfaces[$interfaceName] = $interface;
					}
				}else{
					$interfaces[$interfaceName] = $interface;
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