<?php

class InterfaceObject {
	
	private static $allInterfaces; // contains all interface objects
	
	public $id;			// Interface id (i.e. safe name) to use in framework
	public $label;		// Interface name to show in UI
	
	public $interfaceRoles = array();
	public $editableConcepts = array();
	
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
	public $isProperty;
	public $isIdent;
	
	public $srcConcept;
	public $tgtConcept;
	public $viewId;
	public $tgtConceptIsObject;
	
	public $refInterfaceId;
	public $isLinkTo;
	public $isTopLevelIfc = false;
	private $boxSubInterfaces;
	public $subInterfaces = array();
	
	public $expressionSQL;

	public function __construct($id, $interface = array()){
		global $allInterfaceObjects; // from Generics.php
		
		if(empty($interface)){
			$interface = $allInterfaceObjects[$id]; // if no $interface is provided, use toplevel interfaces from $allInterfaceObjects
			$this->isTopLevelIfc = true;
		}
		
		// Check if interface exists
		if(empty($interface['id'])) throw new Exception ("Interface '$id' does not exists", 500);
		
		// Set attributes of interface
		$this->id = $interface['id'];
		$this->label = $interface['label'];
		
		$this->interfaceRoles = $interface['interfaceRoles'];
		
		$this->invariantConjuctsIds = $interface['invConjunctIds']; // only applicable for Top-level interfaces
		$this->signalConjunctsIds = $interface['sigConjunctIds']; // only applicable for Top-level interfaces
		
		// CRUD rights
		$this->crudC = is_null($interface['crudC']) ? Config::get('defaultCrudC', 'transactions') : $interface['crudC'];
		$this->crudR = is_null($interface['crudR']) ? Config::get('defaultCrudR', 'transactions') : $interface['crudR'];
		$this->crudU = is_null($interface['crudU']) ? Config::get('defaultCrudU', 'transactions') : $interface['crudU'];
		$this->crudD = is_null($interface['crudD']) ? Config::get('defaultCrudD', 'transactions') : $interface['crudD'];
		
		// Information about the (editable) relation if applicable
		$this->relation = $interface['relation']; 
		$this->relationIsFlipped = $interface['relationIsFlipped'];
		$this->totaal = $interface['exprIsTot'];
		$this->univalent = $interface['exprIsUni'];
		$this->isProperty = $interface['exprIsProp'];
		$this->isIdent = $interface['exprIsIdent'];
		$this->srcConcept = $interface['srcConcept'];
		$this->tgtConcept = $interface['tgtConcept'];
		isset($interface['viewId']) ? $this->viewId = $interface['viewId'] : null;
		
		// Determine if tgtConcept is Object (true) or Scalar (false)
		$this->tgtConceptIsObject = (Concept::getTypeRepresentation($this->tgtConcept) == "OBJECT") ? true : false;
		
		if($this->crudU && $this->tgtConceptIsObject) $this->editableConcepts[] = $this->tgtConcept;
		
		// Set attributes
		$this->refInterfaceId = $interface['refSubInterfaceId'];
		$this->isLinkTo = $interface['isLinkTo'];
		$this->boxSubInterfaces = $interface['boxSubInterfaces'];
		$this->expressionSQL = $interface['expressionSQL'];
				
		// Determine subInterfaces
		foreach ((array)$this->boxSubInterfaces as $subInterface){
			$ifc = new InterfaceObject($subInterface['id'], $subInterface);
			$this->subInterfaces[] = $ifc;
			$this->editableConcepts = array_merge($this->editableConcepts, $ifc->editableConcepts);
		}
	}
	
	public function __toString() {
		return $this->id;
	}
	
	public function getInterface(){
		
		return $this;
				
	}
	
	public static function getSubinterface($ifcId, $parentIfc = null){
		// Top level interface
		if(is_null($parentIfc)){
			return new InterfaceObject($ifcId);
		// Subinterface
		}else{
			foreach((array)$parentIfc->subInterfaces as $subinterface){
				if($subinterface->id == $ifcId) {
					return $subinterface;
				}
			}
			return false;
		}
	}
	
	public static function getAllInterfaceObjects(){		
		if(!isset(self::$allInterfaces)){
			global $allInterfaceObjects; // from Generics.php
		
			foreach ($allInterfaceObjects as $interfaceId => $interface){
				$ifc = new InterfaceObject($interfaceId);
				self::$allInterfaces[$ifc->id] = $ifc;
			}
		}
		return self::$allInterfaces;
	}
	
	public static function getAllInterfacesForConcept($concept){
		$interfaces = array();
		foreach (InterfaceObject::getAllInterfaceObjects() as $ifc){
			if ($ifc->srcConcept == $concept) $interfaces[$ifc->id] = $ifc;
		}
		return $interfaces;
	}
	
	public static function getPublicInterfaces(){
		$interfaces = array();
		foreach(InterfaceObject::getAllInterfaceObjects() as $ifc){
			if (empty($ifc->interfaceRoles)) $interfaces[$ifc->id] = $ifc;
		}
		return $interfaces;
	}
}

?>