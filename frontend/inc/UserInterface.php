<?php

class UserInterface {

	public $name;
	public $interfaceRoles = array();
	public $editableConcepts = array();
	public $srcConcept;
	public $tgtConcept;
	private $expressionSQL;

	public function __construct($name){
		global $allInterfaceObjects; // from Generics.php
		
		$this->name = $allInterfaceObjects[$name]['name'];
		$this->interfaceRoles = $allInterfaceObjects[$name]['interfaceRoles'];
		$this->editableConcepts = $allInterfaceObjects[$name]['editableConcepts'];
		$this->srcConcept = $allInterfaceObjects[$name]['srcConcept'];
		$this->tgtConcept = $allInterfaceObjects[$name]['tgtConcept'];
		$this->expressionSQL = $allInterfaceObjects[$name]['expressionSQL'];
		
		
	}
	
	public static function getAllInterfaces(){ 
		global $allInterfaceObjects; // from Generics.php
		
		return array_keys($allInterfaceObjects);
			
	}
	
	public static function isInterfaceForRole($roleName, $interfaceName = null){
		if(isset($interfaceName)){
			$interface = new UserInterface($interfaceName);
			return (in_array($roleName, $interface->interfaceRoles) or empty($interface->interfaceRoles));
		}		
		
		return (in_array($roleName, $this->interfaceRoles) or empty($this->interfaceRoles));
	}	
}

?>