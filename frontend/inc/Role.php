<?php

class Role {

	public $id;
	public $name; // TODO: kan weg?
	public $label;
	public $maintains = array();
	public $interfaces = array();

	public function __construct($id = null){
		global $allRoles; // from Generics.php
		global $allInterfaceObjects; // from Generics.php
		
		if(is_null($id)){ 
			$id = DEFAULT_ROLEID; // localSettings.php
			ErrorHandling::addLog("Default role selected");
		}
		
		// Check if role exists
		if(!key_exists($id, $allRoles)) throw new Exception ('Role with roleId \''.$id.'\' does not exists');
		
		// Name of role
		$this->id = $id;
		$this->name = $allRoles[$id]['name'];
		$this->label = $allRoles[$id]['name'];
		
		// Rules that are maintained by this role
		$this->maintains = (array)$allRoles[$id]['ruleNames'];
		
		// Interfaces that are accessible by this role
		foreach ($allInterfaceObjects as $interfaceName => $interface){
			if (ObjectInterface::isInterfaceForRole($this->name, $interfaceName)) $this->interfaces[] = $interfaceName;
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
			$rules[$ruleName] = RuleEngine::getRule($ruleName);
		}
		
		return $rules;
	}
	
	public function getInterfaces($srcConceptSESSION = null, $srcConcept = null){ // $srcConceptSESSION: true, false, null (=all), $srcConcept: <concept> or null (=all)
		$interfaces = array();
		
		foreach($this->interfaces as $interfaceName){
			$interface = new ObjectInterface($interfaceName);
			
			if(isset($srcConceptSESSION)){
				switch ($srcConceptSESSION){
					case true :
						if($interface->srcConcept == 'SESSION') $interfaces[] = $interface;
						break;
					case false :
						if($interface->srcConcept != 'SESSION') $interfaces[] = $interface;
						break;
				}
			}else{
				if(isset($srcConcept)){ // TODO: moet dit niet het tgtConcept zijn??
					if($interface->srcConcept == $srcConcept 
						|| in_array($srcConcept, Concept::getSpecializations($interface->srcConcept)) ) {
						
						$interfaces[] = $interface;
					}
				}else{
					$interfaces[] = $interface;
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