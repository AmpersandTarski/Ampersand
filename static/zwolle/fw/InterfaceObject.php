<?php

class InterfaceObject {
	
	private static $allInterfaces; // contains all interface objects
	private $database;
	
	public $id;			// Interface id (i.e. safe name) to use in framework
	public $path;
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
	public $boxSubInterfaces;
	public $subInterfaces = array();
	
	public $expressionSQL;
	
	/**
	 * 
	 * @var Atom
	 */
	public $srcAtom;
	
	/**
	 * 
	 * @var Atom
	 */
	public $tgtAtom;
	

	/**
	 * 
	 * @param string $id
	 * @param unknown $interface
	 * @param Atom $srcAtom
	 */
	public function __construct($id, $interfaces = array(), $srcAtom = null){
		$this->database = Database::singleton();
		
		if(empty($interfaces)){
		    global $allInterfaceObjects; // from Generics.php
			$interfaces = $allInterfaceObjects; // if no $interface is provided, use toplevel interfaces from $allInterfaceObjects
			$this->isTopLevelIfc = true;
		}
		
		// Check if interface exists
		foreach ($interfaces as $ifc){
		    if($ifc['id'] == $id) $interface = $ifc;
		}
		if(!isset($interface)) throw new Exception ("Interface '$id' does not exists", 500);
		
		// Set attributes of interface
		$this->id = $interface['id'];
		$this->srcAtom = $srcAtom;
		$this->path = is_null($this->srcAtom) ? $this->id : $this->srcAtom->path . '/' . $this->id;
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
		    // TODO: insert check to compare $srcAtom->concept with $this->srcConcept
		
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
			$ifc = new InterfaceObject($subInterface['id'], $this->boxSubInterfaces);
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
	
	/**********************************************************************************************
	 * 
	 * Fuctions related to chaining interfaces and atoms
	 * 
	 *********************************************************************************************/
	public function atom($atomId){
	    // Check if tgtAtom is part of tgtAtoms of interface
	    if(!in_array($atomId, $this->getTgtAtomIds())) throw new Exception ("Resource '{$atomId}[{$this->tgtConcept}]' not found", 404);
	    
	    $this->tgtAtom = new Atom($atomId, $this->tgtConcept, $this->viewId, $this);
	    return $this->tgtAtom;
	}
	
	private function getTgtAtomIds(){
	    $query = "SELECT DISTINCT `tgt` FROM ($this->expressionSQL) AS `results` WHERE `src` = '{$this->srcAtom->idEsc}' AND `tgt` IS NOT NULL";
	    $tgtAtomIds = array_column((array)$this->database->Exe($query), 'tgt');

	    // Integrity check
	    if($this->univalent && (count($tgtAtomIds) > 1)) throw new Exception("Univalent (sub)interface returns more than 1 resource: '{$this->path}'", 500);
	    
	    return (array)$tgtAtomIds;
	}
	
	public function getContent($options = array(), $recursionArr = array()){
	    // CRUD check
	    if(!$this->crudR) throw new Exception("Read not allowed for '{$this->path}'", 405);
	    
	    $session = Session::singleton();
	    
	    // Default options
	    $options['arrayType'] = isset($options['arrayType']) ? $options['arrayType'] : 'num';
	    $options['metaData'] = isset($options['metaData']) ? filter_var($options['metaData'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['navIfc'] = isset($options['navIfc']) ? filter_var($options['navIfc'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['inclLinktoData'] = isset($options['inclLinktoData']) ? filter_var($options['inclLinktoData'], FILTER_VALIDATE_BOOLEAN) : false;
	    
	    // Initialize result array
	    if($this->tgtConceptIsObject && !$this->isProperty) $result = array(); // return array if tgtConceptIsObject (except properties), even if result is empty
	    elseif(!$this->univalent) $result = array(); // return array for non-univalent interfaces
	    else $result = null; // else (i.e. properties and univalent scalars)
	    
	    // Loop over target atoms
	    foreach ($this->getTgtAtomIds() as $tgtAtomId){
	        	
	        $tgtAtom = new Atom($tgtAtomId, $this->tgtConcept, $this->viewId, $this);
	        	
	        // Object
	        if($this->tgtConceptIsObject){
	            // Property leaf: a property at a leaf of a (sub)interface is presented as true/false
	            if($this->isProperty && !$this->isIdent && empty($this->subInterfaces) && empty($this->refInterfaceId)){
	                $result = !is_null($tgtAtom->id); // convert NULL into false and everything else in true
	    
	            // Regular object, with or without subinterfaces
	            }else{
	                $content = $tgtAtom->getContent($options, $recursionArr);
	                	
	                // Add target atom to result array
	                switch($options['arrayType']){
	                    case 'num' :
	                        // if($this->univalent) $result = $content; else $result[] = $content;
	                        $result[] = $content;
	                        break;
	                    case 'assoc' :
	                        // if($this->univalent) $result = $content; else $result[$tgtAtom->id] = $content;
	                        $result[$tgtAtom->id] = $content;
	                        break;
	                    default :
	                        throw new Exception ("Unknown arrayType specified: '{$options['arrayType']}'", 500);
	                }
	                	
	            }
	    
	        // Scalar
	        }else{
	            // Leaf
	            if(empty($this->subInterfaces) && empty($this->refInterfaceId)){
	                $content = Atom::typeConversion($tgtAtom->id, $this->tgtConcept);
	                	
	                if($this->univalent) $result = $content;
	                else $result[] = $content;
	                	
	            // Tree
	            }else{
	                throw new Exception("Scalar cannot have a subinterface (box) defined: '{$this->path}'", 500);
	            }
	        }
	    }
	    
	    // Return result
	    return $result;
	}
	
	/**
	 * Function to create a new Atom at the given interface.
	 * @param array $data
	 * @param array $options
	 * @throws Exception
	 * @return mixed
	 */
	public function create($data, $options = array()){	
	    // CRUD check
	    if(!$this->crudC) throw new Exception ("Create not allowed for '{$this->path}'", 405);
	    if(!$this->tgtConceptIsObject) throw new Exception ("Cannot create non-object [{$this->tgtConcept}] in '{$this->path}'. Use PATCH add operation instead", 405);
	    
	    // Handle options
	    if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
	
	    // Perform create
	    $newAtom = Concept::createNewAtom($this->tgtConcept);
	    $this->database->addAtomToConcept($newAtom);
	
	    // Special case for CREATE in I[Concept] interfaces
	    if($this->srcAtom->id === '_NEW_'){
	        $this->srcAtom->setId($newAtom->id);
	        $this->path = str_replace('_NEW_', $newAtom->id, $this->path);
	    }
	
	    // If interface expression is a relation, also add tuple(this, newAtom) in this relation
	    if($this->relation) $this->database->editUpdate($this->relation, $this->relationIsFlipped, $this->srcAtom, $newAtom);
	    
	    // Walk to new atom
	    $newAtom = $this->atom($newAtom->id);
	    
	    // Set requested state (using patches)
	    $patches = is_array($data) ? $data['patches'] : array();
	    $newAtom->doPatches($patches);
	
	    // Special case for file upload. TODO: make extension with hooks
	    if($this->tgtConcept == "FileObject"){
	         
	        if (is_uploaded_file($_FILES['file']['tmp_name'])){
	            $tmp_name = $_FILES['file']['tmp_name'];
	            $new_name = time() . '_' . $_FILES['file']['name'];
	            $absolutePath = Config::get('absolutePath') . Config::get('uploadPath') . $new_name;
	            $relativePath = Config::get('uploadPath') . $new_name;
	            $result = move_uploaded_file($tmp_name, $absolutePath);
	             
	            if($result) Notifications::addSuccess("File '".$new_name."' uploaded");
	            else throw new Exception ("Error in file upload", 500);
	
	            // Populate filePath and originalFileName relations in database
	            $this->database->editUpdate('filePath', false, $newAtom, new Atom($relativePath, 'FilePath'));
	            $this->database->editUpdate('originalFileName', false, $newAtom, new Atom($_FILES['file']['name'], 'FileName'));
	
	        }else{
	            throw new Exception ("No file uploaded", 500);
	        }
	    }
	
	    // Close transaction
	    $atomStoreNewContent = $this->crudR ? $newAtom : null; // Get and store new content if interface is readable (crudR)
	    $this->database->closeTransaction($newAtom->concept . ' created', false, null, $atomStoreNewContent);
	
	    // Return atom content (can be null)
	    return $newAtom->getStoredContent();
	
	}
	
	/**
	 * Function not implemented. Use Atom->update() method instead.
	 * @throws Exception
	 */
	public function update(){
	    throw new Exception ("Cannot update from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	/**
	 * Function not implemented. Use Atom->patch() method instead.
	 * @throws Exception
	 */
	public function patch(){
	    throw new Exception ("Cannot patch from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	/**
	 * Function not implemented. Use Atom->delete() method instead.
	 * @throws Exception
	 */
	public function delete(){
	    throw new Exception ("Cannot delete from interface '{$this->path}'. Add resource identifier behind path", 405);
	}
	
	public function doPatchReplace($patch){
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
	
	    // PatchReplace only works for UNI expressions. Otherwise, use patch remove and patch add
	    if(!$this->univalent) throw new Exception("Cannot patch replace for non-univalent interface '{$this->path}'. Use patch remove + add instead", 500);
	    
	    // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch replace. No 'value' specfied for patch with path '{$this->path}'", 400);
	    $value = $patch['value'];
	
	    // Interface is property
	    if($this->isProperty && !$this->isIdent){
	        // Throw error when patch value is something else then true, false or null
	        if(!(is_bool($value) || is_null($value))) throw new Exception("Interface '{$this->path}' is property, boolean expected, non-boolean provided");
	        	
	        // When true
	        if($value){
	            $this->database->editUpdate($this->relation, $this->relationIsFlipped, $this->srcAtom, $this->srcAtom);
	        // When false or null
	        }else{
	            $this->database->editDelete($this->relation, $this->relationIsFlipped, $this->srcAtom, $this->srcAtom);
	        }
	        	
	    // Interface is a relation to an object
	    }elseif($this->tgtConceptIsObject){
	        throw new Exception("Cannot patch replace for object reference in interface '{$this->this}'. Use patch remove + add instead", 500);
	
	    // Interface is a relation to a scalar (i.e. not an object)
	    }elseif(!$this->tgtConceptIsObject){
	        	
	        // Replace by nothing => editDelete
	        if(is_null($value)){
	            $this->database->editDelete($this->relation, $this->relationIsFlipped, $this->srcAtom, new Atom(null, $this->tgtConcept));
	            	
	        // Replace by other atom => editUpdate
	        }else{
	            $this->database->editUpdate($this->relation, $this->relationIsFlipped, $this->srcAtom, new Atom($value, $this->tgtConcept));
	        }
	    }else{
	        throw new Exception ("Unknown patch replace. Please contact the application administrator", 500);
	    }
	}
	
	public function doPatchAdd($patch){
	    // CRUD check
	    if(!$this->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
	    
	    // Check if patch value is provided
	    if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch add. No 'value' specfied in '{$this->path}'", 400);
	    
	    $tgtAtom = new Atom($patch['value'], $this->tgtConcept);
	    
	    // Interface is property
	    if($this->isProperty && !$this->isIdent){
	        // Properties must be treated as a 'replace', so not handled here
	        throw new Exception("Cannot patch add for property '{$this->path}'. Use patch replace instead", 500);
	
	    // Interface is a relation to an object
	    }elseif($this->tgtConceptIsObject){
	        // Check: If tgtAtom (value) does not exists and there is not crud create right, throw exception
	        if(!$this->crudC && !$tgtAtom->atomExists()) throw new Exception ("Resource '{$tgtAtom->id}[{$tgtAtom->concept}]' does not exist and may not be created in {$this->path}", 403);
	        	
	        $this->database->editUpdate($this->relation, $this->relationIsFlipped, $this->srcAtom, $tgtAtom);
	
	    // Interface is a relation to a scalar (i.e. not an object)
	    }elseif(!$this->tgtConceptIsObject){    	
	        // Check: If interface is univalent, throw exception
	        if($this->univalent) throw new Exception("Cannot patch add for univalent interface {$this->path}. Use patch replace instead", 500);
	        	
	        $this->database->editUpdate($this->relation, $this->relationIsFlipped, $this->srcAtom, $tgtAtom);
	        
	    }else{
	        throw new Exception ("Unknown patch add. Please contact the application administrator", 500);
	    }
	}
	
	/**
	 * Function not implemented. Use Atom->doPatchRemove() method instead.
	 * @throws Exception
	 */
	public function doPatchRemove(){
	    throw new Exception ("Cannot patch remove from '{$this->path}'. Missing resource identifier", 405);
	}
	
	/**********************************************************************************************
	 *
	 * Static InterfaceObject functions
	 *
	 *********************************************************************************************/
	
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