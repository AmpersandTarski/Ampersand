<?php

class UserInterface {

	public $name;
	public $interfaceRoles = array();
	public $editableConcepts = array();
	public $srcConcept;
	public $tgtConcept;
	private $expressionSQL;

	public function __construct($name, $interface = array()){
		global $allInterfaceObjects; // from Generics.php
		
		if(empty($interface)) $interface = $allInterfaceObjects[$name]; // if no $interface is provided, use toplevel interfaces from $allInterfaceObjects
		
		try{ 
			if(empty($interface['name'])) throw new Exception ('Specified interface does not exists');
			
			$this->name = $interface['name'];
			$this->interfaceRoles = $interface['interfaceRoles'];
			$this->editableConcepts = $interface['editableConcepts'];
			$this->srcConcept = $interface['srcConcept'];
			$this->tgtConcept = $interface['tgtConcept'];
			$this->expressionSQL = $interface['expressionSQL'];
			$this->refSubInterface = $interface['refSubInterface'];
			
			// determine subInterfaces
			if(!empty($this->refSubInterface)){
				$subInterface = new UserInterface($this->refSubInterface);
				$this->subInterfaces = $subInterface->subInterfaces;
			}else{
				$this->subInterfaces = $interface['boxSubInterfaces'];
			}
		}catch (Exception $e){
			ErrorHandling::addError($e->getMessage());
		}
	}
	
	public function getAtomsAndLinks($srcAtom = "1"){
		$database = Database::singleton();
		$subInterfacesAtomsAndLinks = array();
		
		$subInterfacesAtomsAndLinks['atom'] = $srcAtom;
		foreach((array)$this->subInterfaces as $subInterface){
			$interface = new UserInterface($subInterface['name'], $subInterface);
			$temp = array();
			
			$temp['interface'] = $interface->name;
			$temp['editable'] = true; // TODO: depends on config in Generics
			$temp['atoms'] = false; // defaults no atoms
			
			if(!empty($interface->subInterfaces)){
				foreach (array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$interface->expressionSQL.") AS results WHERE src='".addslashes($srcAtom)."' AND `tgt` IS NOT NULL"), 'tgt') as $tgtAtom){
					$temp['atoms'] = $interface->getAtomsAndLinks($tgtAtom);
					
				}
			}else{
				foreach (array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$interface->expressionSQL.") AS results WHERE src='".addslashes($srcAtom)."' AND `tgt` IS NOT NULL"), 'tgt') as $tgtAtom){
					$temp['atoms'][] = array('atom' => $tgtAtom, 'interfaces' => false);
				}
			}
			
			$subInterfacesAtomsAndLinks['interfaces'][] = $temp;
		}
				
		return $subInterfacesAtomsAndLinks;
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