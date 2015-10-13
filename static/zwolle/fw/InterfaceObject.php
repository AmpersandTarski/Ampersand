<?php

class InterfaceObject {
	
	public $id;			// Interface id (i.e. safe name) to use in framework
	public $label;		// Interface name to show in UI
	
	public $interfaceRoles = array();
	
	public $invariantConjuctsIds;
	public $signalConjunctsIds;
	
	public $crudC;
	public $crudR;
	public $crudU;
	public $crudD;
	
	public $relation; 
	public $relationIsFlipped;
	public $univalent;
	public $totaal;
	public $editable;
	public $isProperty;
	public $isIdent;
	
	public $srcConcept;
	public $tgtConcept;
	public $viewId;
	public $tgtConceptIsObject;
	
	public $refInterfaceId;
	public $isLinkTo;
	private $boxSubInterfaces;
	public $subInterfaces = array();
	
	public $expressionSQL;

	public function __construct($id, $interface = array()){
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
		
		// CRUD rights
		$this->crudC = array_key_exists('crudC', $interface) ? $interface['crudC'] : Config::get('defaultCrudC');
		$this->crudR = array_key_exists('crudR', $interface) ? $interface['crudR'] : Config::get('defaultCrudR');
		$this->crudU = array_key_exists('crudU', $interface) ? $interface['crudU'] : Config::get('defaultCrudU');
		$this->crudD = array_key_exists('crudD', $interface) ? $interface['crudD'] : Config::get('defaultCrudD');
		
		// Information about the (editable) relation if applicable
		$this->relation = $interface['relation']; 
		$this->relationIsFlipped = $interface['relationIsFlipped'];
		$this->editable = (empty($interface['relation'])) ? false : $interface['relationIsEditable'];
		$this->totaal = $interface['exprIsTot'];
		$this->univalent = $interface['exprIsUni'];
		$this->isProperty = $interface['exprIsProp'];
		$this->isIdent = $interface['exprIsIdent'];
		$this->srcConcept = $interface['srcConcept'];
		$this->tgtConcept = $interface['tgtConcept'];
		isset($interface['viewId']) ? $this->viewId = $interface['viewId'] : null;
		
		// Determine if tgtConcept is Object (true) or Scalar (false)
		$this->tgtConceptIsObject = (Concept::getTypeRepresentation($this->tgtConcept) == "OBJECT") ? true : false;
		
		// Set attributes
		$this->refInterfaceId = $interface['refSubInterfaceId'];
		$this->isLinkTo = $interface['isLinkTo'];
		$this->boxSubInterfaces = $interface['boxSubInterfaces'];
		$this->expressionSQL = $interface['expressionSQL'];
				
		// Determine subInterfaces
		foreach ((array)$this->boxSubInterfaces as $subInterface){
			$this->subInterfaces[] = new InterfaceObject($subInterface['id'], $subInterface);
		}
	}
	
	public function __toString() {
		return $this->id;
	}
	
	public function getInterface(){
		
		return $this;
				
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
	
	public static function getAllInterfacesForConcept($concept){
		$interfaces = array();
	
		foreach (InterfaceObject::getAllInterfaceObjects() as $interfaceId => $interface){
			if ($interface['srcConcept'] == $concept) $interfaces[] = $interfaceId;
		}
	
		return $interfaces;
	}
}

?>