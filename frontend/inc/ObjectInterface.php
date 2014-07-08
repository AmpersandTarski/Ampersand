<?php

class ObjectInterface {
	
	public $id;
	public $name;
	public $interfaceRoles = array();
	public $editableConcepts = array();
	public $relation;
	public $srcConcept;
	public $tgtConcept;
	public $refInterface;
	private $boxSubInterfaces;
	private $expressionSQL;
	public $subInterfaces = array();

	public function __construct($name, $interface = array()){
		global $allInterfaceObjects; // from Generics.php
		
		if(empty($interface)) $interface = $allInterfaceObjects[$name]; // if no $interface is provided, use toplevel interfaces from $allInterfaceObjects
		
		// Check if interface exists
		if(empty($interface['name'])) throw new Exception ('Specified interface \''.$name.'\' does not exists');
		
		// Set attributes of interface
		$this->id = $interface['name'];
		$this->name = $interface['name'];
		$this->interfaceRoles = $interface['interfaceRoles'];
		$this->editableConcepts = $interface['editableConcepts'];
		$this->relation = $interface['relation'];
		$this->srcConcept = $interface['srcConcept'];
		$this->tgtConcept = $interface['tgtConcept'];
		$this->refInterface = $interface['refSubInterface'];
		$this->boxSubInterfaces = $interface['boxSubInterfaces'];
		$this->expressionSQL = $interface['expressionSQL'];
		
		// Determine subInterfaces
		if(!empty($this->refInterface)){
			$refInterface = new ObjectInterface($this->refInterface);
			foreach($refInterface->subInterfaces as $subInterface){
				$this->subInterfaces[] = $subInterface;
			}
		}else{
			foreach ((array)$this->boxSubInterfaces as $subInterface){
				$this->subInterfaces[] = new ObjectInterface($subInterface['name'], $subInterface);
			}
		}
	}
	
	public function getInterface(){
		
		return $this;
				
	}
	
	public function getContent($srcAtom = null){
		$database = Database::singleton();
		
		if(is_null($srcAtom)) $srcAtom = session_id();
		
		$tgtAtoms = array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$this->expressionSQL.") AS results WHERE src='".addslashes($srcAtom)."' AND `tgt` IS NOT NULL"), 'tgt');
		foreach ($tgtAtoms as $tgtAtom){
			
			if(count($this->subInterfaces) > 0){
				$atom = new Atom($tgtAtom);
				$content[$atom->id] = $atom->getContent($this);
			}else{
				$content[] = $tgtAtom;
			}
		}
		
		return $content;
		
	}
	
	
	public static function isInterfaceForRole($roleName, $interfaceName = null){
		if(isset($interfaceName)){
			$interface = new ObjectInterface($interfaceName);
			return (in_array($roleName, $interface->interfaceRoles) or empty($interface->interfaceRoles));
		}		
		
		return (in_array($roleName, $this->interfaceRoles) or empty($this->interfaceRoles));
	}
	
	private function isEditable($tgtConcept){
		return in_array($tgtConcept, (array)$this->editableConcepts);
	}
}

?>