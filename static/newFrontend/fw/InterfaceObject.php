<?php

class InterfaceObject {
	
	public $id;			// Interface id (i.e. safe name) to use in framework
	public $label;		// Interface name to show in UI
	
	public $interfaceRoles = array();
	
	public $invariantConjuctsIds;
	public $signalConjunctsIds;
	
	public $relation; 
	public $relationIsFlipped;
	public $univalent;
	public $totaal;
	public $editable;
	public $isProperty;
	
	public $srcConcept;
	public $tgtConcept;
	public $viewId;
	public $tgtDataType;
	
	public $refInterfaceId;
	private $boxSubInterfaces;
	public $subInterfaces = array();
	
	public $expressionSQL;

	/*
	 * $refInterfacesArr is used to determine infinite loops in refInterfaceId
	 */
	public function __construct($id, $interface = array(), $refInterfacesArr = array()){
		global $allInterfaceObjects; // from Generics.php
		
		if(empty($interface)) $interface = $allInterfaceObjects[$id]; // if no $interface is provided, use toplevel interfaces from $allInterfaceObjects
		
		// Check if interface exists
		if(empty($interface['id'])) throw new Exception ("Interface \'$id\' does not exists", 500);
		
		// Set attributes of interface
		$this->id = $interface['id'];
		$this->label = $interface['label'];
		
		$this->interfaceRoles = $interface['interfaceRoles'];
		
		$this->invariantConjuctsIds = $interface['invConjunctIds']; // only applicable for Top-level interfaces
		$this->signalConjunctsIds = $interface['sigConjunctIds']; // only applicable for Top-level interfaces
		
		// Information about the (editable) relation if applicable
		$this->relation = $interface['relation']; 
		$this->relationIsFlipped = $interface['relationIsFlipped'];
		$this->editable = (empty($interface['relation'])) ? false : $interface['relationIsEditable'];
		$this->totaal = $interface['exprIsTot'];
		$this->univalent = $interface['exprIsUni'];
		$this->isProperty = $interface['exprIsProp'];
		$this->srcConcept = $interface['srcConcept'];
		$this->tgtConcept = $interface['tgtConcept'];
		isset($interface['viewId']) ? $this->viewId = $interface['viewId'] : null;
		
		// Set datatype of tgtConcept
		switch(strtolower($this->tgtConcept)){ // matching case-insensitive, because template filenames in windows are case-insensitive
			// <input> types
			case "text":
				$this->tgtDataType = "text";		// relation to TEXT, Text, TExt, etc concept
				break;
			case "date":
				$this->tgtDataType = "date";		// relation to DATE, Date, DAte, etc concept
				break;
			case "bool":
				$this->tgtDataType = "checkbox";	// relation to BOOL, etc concept
				break;
			case "password":
				$this->tgtDataType = "password"; 	// relation to PASSWORD, etc concept
				break;
			case "blob":
				$this->tgtDataType = "textarea"; 	// relation to BLOB, etc concept
				break;
			default:
				$this->tgtDataType = "concept"; 	// relation to other concept
		}
		
	/* Information about subinterfaces */
		// Set attributes
		$this->refInterfaceId = $interface['refSubInterfaceId'];
		$this->boxSubInterfaces = $interface['boxSubInterfaces'];
		$this->expressionSQL = $interface['expressionSQL'];
		
		// Check for infinite loop in interface (i.e. subinterface refers back to interface).
		$refInterfacesArr[] = $this->id;
		if(in_array($this->refInterfaceId, $refInterfacesArr)) throw new Exception("Infinite loop in interface '$this->id' by referencing '$this->refInterfaceId'", 500);
				
		// Determine subInterfaces
		if(!empty($this->refInterfaceId)){
					
			$refInterface = new InterfaceObject($this->refInterfaceId, null, $refInterfacesArr);
			foreach($refInterface->subInterfaces as $subInterface){
				$this->subInterfaces[] = $subInterface;
			}
		}else{
			foreach ((array)$this->boxSubInterfaces as $subInterface){
				$this->subInterfaces[] = new InterfaceObject($subInterface['id'], $subInterface, $refInterfacesArr);
			}
		}
	}
	
	public function getInterface(){
		
		return $this;
				
	}
	
	
	public static function isInterfaceForRole($roleName, $interfaceId = null){
		if(isset($interfaceId)){
			$interface = new InterfaceObject($interfaceId);
			return (in_array($roleName, $interface->interfaceRoles) or empty($interface->interfaceRoles));
		}		
		
		return (in_array($roleName, $this->interfaceRoles) or empty($this->interfaceRoles));
	}
	
	public static function getSubinterface($interface, $subinterfaceId){
		
		foreach((array)$interface->subInterfaces as $subinterface){
			if($subinterface->id == $subinterfaceId) {
				$result = $subinterface;
			}
		}
		return empty($result) ? false : $result;
	}
	
	public static function getAllInterfaceObjects(){
		global $allInterfaceObjects; // from Generics.php
		
		return (array)$allInterfaceObjects;
	}
}

?>