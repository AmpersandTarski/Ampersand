<?php

class ObjectInterface {
	
	public $id;
	public $name;
	public $link;
	public $interfaceRoles = array();
	// public $editableConcepts = array();
	public $relation;
	public $univalent;
	public $totaal;
	public $editable; public $notEditable;
	public $srcConcept;
	public $tgtConcept;
	public $tgtDataType;
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
		$this->link = 'http://localhost/CB/api/v1/interface/'.urlencode($this->name); // TODO: make config for first part of link or do without.
		$this->interfaceRoles = $interface['interfaceRoles'];
		
		$this->editableConcepts = $interface['editableConcepts']; // used by genEditableConceptInfo() function in AmpersandViewer.php
		
		// Information about the (editable) relation if applicable
		$this->relation = $interface['relation'];
		$this->editable = (!empty($interface['relation'])) ? true : false; $this->notEditable = !$this->editable;
		$this->totaal = ($interface['min'] == "One") ? true : false;
		$this->univalent = ($interface['max'] == "One") ? true : false;
		$this->srcConcept = $interface['srcConcept'];
		$this->tgtConcept = $interface['tgtConcept'];
		
		// Set datatype of tgtConcept
		switch($this->tgtConcept){
			// <input> types
			case "TEXT":
				$this->tgtDataType = "text";		// relation to TEXT concept
				break;
			case "DATE":
				$this->tgtDataType = "date";		// relation to DATE concept
				break;
			case "BOOLEAN":
				$this->tgtDataType = "checkbox";	// relation to BOOLEAN concept
				break;
			case "EMAIL":
				$this->tgtDataType = "email";		// relation to EMAIL concept
				break;
			case "PASSWORD":
				$this->tgtDataType = "password"; 	// relation to PASSWORD concept
				break;
			case "COLOR":
				$this->tgtDataType = "color";		// relation to STATUS concept
				break;
			// <textarea>
			case "BLOB":
				$this->tgtDataType = "textarea"; 	// relation to BLOB concept
				break;
			// <select>
			default:
				$this->tgtDataType = "concept"; 	// relation to other concept
		}
		
		// Information about subinterfaces
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
		$session = Session::singleton();
		
		$content = array();
		
		if(is_null($srcAtom)) $srcAtom = session_id();
		
		$tgtAtoms = array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$this->expressionSQL.") AS results WHERE src='".addslashes($srcAtom)."' AND `tgt` IS NOT NULL"), 'tgt');
		foreach ($tgtAtoms as $tgtAtom){
			
			if(count($this->subInterfaces) > 0){
				$atom = new Atom($tgtAtom);
				$content[$atom->id] = $atom->getContent($this);
			}else{
				if(strtolower($tgtAtom) === "true") $tgtAtom = true;
				if(strtolower($tgtAtom) === "false") $tgtAtom = false;
				
				$links = array();
				$interfaces = array();
				foreach($session->role->getInterfaces(null, $this->tgtConcept) as $interfaceForTgtConcept){
					$links[] = $interfaceForTgtConcept->link . '/atom/' . urlencode($tgtAtom);
					$interfaces[] = $interfaceForTgtConcept->name;
				}
				
				$content[] = array('id' => $tgtAtom, 
								   'label' => $tgtAtom,	// TODO: enable ampersand VIEWS here
								   'links' => $links,
								   'interfaces' => $interfaces);
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