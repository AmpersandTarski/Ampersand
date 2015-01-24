<?php

class ObjectInterface {
	
	public $id;
	public $name; // TODO: kan vervallen?
	public $label;
	
	public $interfaceRoles = array();
	
	public $invariantConjuctsIds;
	public $signalConjunctsIds;
	
	public $relation; 
	public $relationIsFlipped;
	public $univalent;
	public $totaal;
	public $editable;
	
	public $srcConcept;
	public $tgtConcept;
	public $tgtDataType;
	
	public $refInterface;
	private $boxSubInterfaces;
	public $subInterfaces = array();
	
	public $expressionSQL;

	public function __construct($name, $interface = array()){
		global $allInterfaceObjects; // from Generics.php
		
		if(empty($interface)) $interface = $allInterfaceObjects[$name]; // if no $interface is provided, use toplevel interfaces from $allInterfaceObjects
		
		// Check if interface exists
		if(empty($interface['name'])) throw new Exception ('Specified interface \''.$name.'\' does not exists');
		
		// Set attributes of interface
		$this->id = $interface['name'];
		$this->name = $interface['name'];
		$this->label = $interface['name'];
		$this->interfaceRoles = $interface['interfaceRoles'];
		
		$this->invariantConjuctsIds = $interface['invConjunctIds']; // only applicable for Top-level interfaces
		$this->signalConjunctsIds = $interface['sigConjunctIds']; // only applicable for Top-level interfaces
		
		$this->editableConcepts = $interface['editableConcepts']; // used by genEditableConceptInfo() function in AmpersandViewer.php
		$this->interfaceInvariantConjunctNames = $interface['interfaceInvariantConjunctNames']; // only applies to top level interface
		
		// Information about the (editable) relation if applicable
		$this->relation = $interface['relation']; 
		$this->relationIsFlipped = $interface['relationIsFlipped'];
		$this->editable = (empty($interface['relation'])) ? false : $interface['relationIsEditable'];
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
	
	
	public static function isInterfaceForRole($roleName, $interfaceName = null){
		if(isset($interfaceName)){
			$interface = new ObjectInterface($interfaceName);
			return (in_array($roleName, $interface->interfaceRoles) or empty($interface->interfaceRoles));
		}		
		
		return (in_array($roleName, $this->interfaceRoles) or empty($this->interfaceRoles));
	}
	
	public static function getSubinterface($interface, $subinterfaceName){
		
		foreach((array)$interface->subInterfaces as $subinterface){
			if($subinterface->name == $subinterfaceName) {
				$result = $subinterface;
			}
		}
		return empty($result) ? false : $result;
	}
}

?>