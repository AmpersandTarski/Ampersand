<?php

Class Atom {
	private $database;
	
	// Ampersand attributes
	public $id;
	public $label;
	public $view;
	public $concept;
	
	private $newContent; // To temporarily store changed atom content 
	
	// JSON-LD attributes
	private $jsonld_id;
	private $jsonld_type;
	
	/**
	 * Atom constructor
	 * @param string $id
	 * @param string $concept
	 * @param string $viewId
	 * @return void
	 */
	public function __construct($id, $concept, $viewId = null){
		$this->database = Database::singleton();
		
		// Ampersand attributes
		$this->id = $id;
		$this->concept = $concept;
		
		// View & label
		$this->view = $this->getView($viewId);
		$this->label = is_null($this->view) ? $this->id : implode($this->view); // no view? label = id
		
		// JSON-LD attributes
		$this->jsonld_id = Config::get('serverURL') . Config::get('apiPath') . '/resource/' . $concept . '/' . $this->id;
		$this->jsonld_type = Config::get('serverURL') . Config::get('apiPath') . '/concept/' . $concept;

	}
	
	/**
	 * Checks if atom exists in database
	 * @return boolean
	 */
	public function atomExists(){
		if($this->id === '_NEW_') return true; // Return true if id is '_NEW_' (special case)
		
		// Note! Mysql is case insensitive for primary keys, e.g. atom 'True' ==  'TRUE' (relevant for all scalars)
		return $this->database->atomExists($this->id, $this->concept);
	}
	
	/**
	 * Sets the new content of the atom in $this->newContent
	 * @param InterfaceObject $interface specifies the interface to use to get the content
	 * @return void
	 */
	public function setNewContent($interface){
		$this->newContent = $this->getContent($interface, true, $this->id);
	}
	
	/**
	 * Returns basic information about an atom
	 * @param array $options
	 * @return array
	 */
	public function getAtom($options = array()){
		$result = array('_id_' => $this->id, '_label_' => $this->label, '_view_' => $this->view);
		
		if($options['jsonld']){
			$result['@id'] = $this->jsonld_id;
			$result['@type'] = $this->jsonld_type;
		}
		
		if($options['navIfc']){
			foreach(InterfaceObject::getAllInterfacesForConcept($this->concept) as $ifc){
				$ifcs[] = array('id' => $ifc->id, 'label' => $ifc->label, 'url' => $this->jsonld_id . '/' . $ifc->id);
			}
			
			$result['_ifcs_'] = $ifcs;
		}
		
		return $result;
	}
	
	/**
	 * Returns components of view
	 * @param string $viewId specifies which view to use
	 * @throws Exception when unknown or unsupported segment type is found
	 * @return NULL|array
	 */
	private function getView($viewId = null){
		$view = Concept::getView($this->concept, $viewId);
	
		if(empty($view) || $this->id == ''){
			return null;
	
		}else{
			$viewStrs = array ();
				
			foreach ($view['segments'] as $viewSegment){
				// text segment
				if ($viewSegment['segmentType'] == 'Text'){
					$viewStrs[$viewSegment['label']] = $viewSegment['Text'];
	
					// expressie segment
				}elseif($viewSegment['segmentType'] == 'Exp'){
					$idEsc = $this->database->escape($this->id);
					$query = "SELECT DISTINCT `tgt` FROM ($viewSegment[expSQL]) AS `results` WHERE `src` = '$idEsc' AND `tgt` IS NOT NULL";
					$tgtAtoms = array_column($this->database->Exe($query), 'tgt');
						
					$txt = count($tgtAtoms) ? $tgtAtoms[0] : null;
					$viewStrs[$viewSegment['label']] = $txt;
	
					// html segment
				}elseif($viewSegment['segmentType'] == 'Html'){
					$errorMessage = "Unsupported segmentType 'Html' in view '" . $view['label'] . "'";
					throw new Exception($errorMessage, 501); // 501: Not implemented
						
					//$viewStrs[$viewSegment['label']] = $viewSegment['Html'];
	
					// unknown segment
				}else{
					$errorMessage = "Unknown segmentType '" . $viewSegment['segmentType'] . "' in view '" . $view['label'] . "'";
					throw new Exception($errorMessage, 501); // 501: Not implemented
				}
			}
			return $viewStrs;
		}
	}
	
	
	/**
	 * 
	 * @param InterfaceObject $interface
	 * @param string $pathEntry
	 * @param string $tgt
	 * @param array $options specifies arrayType, metaData, navIfc and inclLinktoData
	 * @param array $recursionArr contains atomIds when linkto data is included (used to detect recursion and prevent infinite loops)
	 * @throws Exception
	 * @return array|string
	 */
	public function getContent($interface, $pathEntry, $tgt = null, $options = array(), $recursionArr = array()){
		$session = Session::singleton();
		
		// CRUD check
		if(!$interface->crudR) throw new Exception("Read not allowed for '$pathEntry'", 405);
		
		// Default options
		$options['arrayType'] = isset($options['arrayType']) ? $options['arrayType'] : 'num';
		$options['metaData'] = isset($options['metaData']) ? filter_var($options['metaData'], FILTER_VALIDATE_BOOLEAN) : true;
		$options['navIfc'] = isset($options['navIfc']) ? filter_var($options['navIfc'], FILTER_VALIDATE_BOOLEAN) : true;
		$options['inclLinktoData'] = isset($options['inclLinktoData']) ? filter_var($options['inclLinktoData'], FILTER_VALIDATE_BOOLEAN) : false;
		
		$idEsc = $this->database->escape($this->id);
		$query = "SELECT DISTINCT `tgt` FROM ($interface->expressionSQL) AS `results` WHERE `src` = '$idEsc' AND `tgt` IS NOT NULL";
		$tgtAtomIds = array_column($this->database->Exe($query), 'tgt');
		
		// Check if tgtAtom is part of tgtAtoms
		if(!is_null($tgt)){
			if(!in_array($tgt, $tgtAtomIds)) throw new Exception ("Resource '{$tgt}[{$interface->tgtConcept}]' not found", 404);
			$tgtAtomIds = array($tgt);
		}
		
		// Integrity check
		if($interface->univalent && (count($tgtAtomIds) > 1)) throw new Exception("Univalent (sub)interface returns more than 1 resource: '$pathEntry'", 500);
		
		// Initialize result array
		$result = $interface->univalent ? null : array();
		
		// Loop over target atoms
		foreach ($tgtAtomIds as $tgtAtomId){
			
			$tgtAtom = new Atom($tgtAtomId, $interface->tgtConcept, $interface->viewId);				
			
			// Object
			if($interface->tgtConceptIsObject){
				// Property leaf: a property at a leaf of a (sub)interface is presented as true/false
				if($interface->isProperty && !$interface->isIdent && empty($interface->subInterfaces) && empty($interface->refInterfaceId)){
					$result = !is_null($tgtAtom->id); // convert NULL into false and everything else in true

				// Regular object, with or without subinterfaces
				}else{
					
					if(is_null($tgt)) $path = $pathEntry . '/' . $tgtAtom->id; else $path = $pathEntry;
					
					$content = array('_id_' => $tgtAtom->id, '_label_' => $tgtAtom->label, '_view_' => $tgtAtom->view);
					
					if($options['jsonld']){
						$content['@id'] = $tgtAtom->jsonld_id;
						$content['@type'] = $tgtAtom->jsonld_type;
					}
										
					// Meta data
					if($options['metaData']){
						$content['_path_'] = $path;
					}
					
					// Define interface(s) to navigate to for this tgtAtom
					if($options['navIfc']){
						$ifcs = array();
						if($interface->isLinkTo && $session->isAccessibleIfc($interface->refInterfaceId)) 
							$ifcs[] = array('id' => $interface->refInterfaceId, 'label' => $interface->refInterfaceId, 'url' => $this->jsonld_id . '/' . $interface->refInterfaceId);
						else $ifcs = array_map(function($o) { 
							return array('id' => $o->id, 'label' => $o->label, 'url' => $this->jsonld_id . '/' . $o->id); 
						}, $session->getInterfacesToReadConcept($interface->tgtConcept));
						$content['_ifcs_'] = $ifcs;
					}
					
					
					// Subinterfaces											
					foreach($interface->subInterfaces as $subinterface){
						// Skip subinterface if not given read rights
						if(!$subinterface->crudR) continue;
						
						$subcontent = $tgtAtom->getContent($subinterface, $path . '/' . $subinterface->id, null, $options, $recursionArr);
						$content[$subinterface->id] = $subcontent;
							
						// _sortValues_ (if subInterface is uni)
						if($subinterface->univalent && $options['metaData']){
							if(is_bool($subcontent)) $sortValue = $subcontent; // property
							elseif($subinterface->tgtConceptIsObject) $sortValue = $subcontent['_label_']; // use label to sort objects
							else $sortValue = $subcontent; // scalar
							
							$content['_sortValues_'][$subinterface->id] = $sortValue;
						}
					}
					
					// Include content for subinterfaces that refer to other interface (e.g. "label" : expr [LINKTO] INTERFACE <refInterface>)
					if(!empty($interface->refInterfaceId)
							&& (!$interface->isLinkTo || $options['inclLinktoData'])  // Include content is interface is not LINKTO or inclLinktoData is explicitly requested via the options
							&& (!in_array($tgtAtom->id, (array)$recursionArr[$interface->refInterfaceId]))) // Prevent infinite loops
					{
						// Add target atom to $recursionArr to prevent infinite loops
						if($options['inclLinktoData']) $recursionArr[$interface->refInterfaceId][] = $tgtAtom->id;
							
						$refInterface = new InterfaceObject($interface->refInterfaceId, null);
						
						foreach($refInterface->subInterfaces as $subinterface){
							// Skip subinterface if not given read rights
							if(!$subinterface->crudR) continue;
							
							$subcontent = $tgtAtom->getContent($subinterface, $path . '/' . $subinterface->id, null, $options, $recursionArr);
							$content[$subinterface->id] = $subcontent;
								
							// _sortValues_ (if subInterface is uni)
							if($subinterface->univalent && $options['metaData']){
								if(is_bool($subcontent)) $sortValue = $subcontent; // property
								elseif($subinterface->tgtConceptIsObject) $sortValue = $subcontent['_label_']; // use label to sort objects
								else $sortValue = $subcontent; // scalar
									
								$content['_sortValues_'][$subinterface->id] = $sortValue;
							}
						}
					}
					
					// Add target atom to result array
					switch($options['arrayType']){
						case 'num' :
							// if($interface->univalent) $result = $content; else $result[] = $content;
							$result[] = $content;
							break;
						case 'assoc' :
							// if($interface->univalent) $result = $content; else $result[$tgtAtom->id] = $content;
							$result[$tgtAtom->id] = $content;
							break;
						default :
							throw new Exception ("Unknown arrayType specified: '{$options['arrayType']}'", 500);
					}
					
				}
				
			// Scalar
			}else{
				// Leaf
				if(empty($interface->subInterfaces) && empty($interface->refInterfaceId)){
					$content = $this->typeConversion($tgtAtom->id, $interface->tgtConcept);
					
					if($interface->univalent) $result = $content;
					else $result[] = $content;
					
				// Tree
				}else{
					throw new Exception("Scalar cannot have a subinterface (box) defined: '$pathEntry'", 500);
				}
			}
		}
		
		// Return result
		if(!is_null($tgt) && is_array($result)) return current($result);
        elseif($interface->tgtConceptIsObject && !$interface->isProperty) return (array)$result; // always return array if tgtConceptIsObject, even if result is empty
		else return $result;
			
	}
	
	/**********************************************************************************************
	 * 
	 * CREATE, UPDATE and DELETE functions 
	 *  
	 **********************************************************************************************/
	
	/**
	 * 
	 * @param InterfaceObject $interface
	 * @param array $data
	 * @param array $options
	 * @return mixed content of created atom
	 */
	public function create($interface, $pathEntry, $data, $options = array()){
		
		// Handle options
		if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
		
		// Perform create
		$newAtom = Concept::createNewAtom($interface->tgtConcept);
		$this->database->addAtomToConcept($newAtom->id, $newAtom->concept);
		
		// Special case for CREATE in I[Concept] interfaces
		if($this->id === '_NEW_'){
			$this->id = $newAtom->id;
			$pathEntry = str_replace('_NEW_', $newAtom->id, $pathEntry);
		}
		
		$pathEntry .= '/' . $newAtom->id;
		
		// If interface expression is a relation, also add tuple(this, newAtom) in this relation
		if($interface->relation) $this->database->editUpdate($interface->relation, $interface->relationIsFlipped, $this->id, $this->concept, $newAtom->id, $newAtom->concept);
	
		// Set requested state (using patches)
        $patches = is_array($data) ? $data['patches'] : array();
		$newAtom->doPatches($interface, $pathEntry, $patches);
		
		// Close transaction
		$this->database->closeTransaction($newAtom->concept . ' created', false, null, false);
		
        try {
        	// Return content of created atom TODO: make sure that content is also returned when database was not committed
			if($interface->crudR) return $this->getContent($interface, $pathEntry, $newAtom->id, $options);
        } catch (Exception $e){
        	Notifications::addError($e->getMessage());
        }
		
	}
	
	/**
	 * Update atom properties in database
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @param mixed $data contains the data of this atom to put
	 * @param array $options
	 * @return array
	 */
	public function update($interface, $pathEntry, $data, $options){
		
		throw new Exception ("Not yet implemented", 501);
	
		// Get current state of atom
		$before = $this->getContent($interface, $pathEntry, $this->id, $options);
	
		// Determine differences between current state ($before) and requested state ($data)
		$patches = mikemccabe\JsonPatch\JsonPatch::diff($before, $data);
	
		return $this->doPatches($interface, $pathEntry, $patches);
	}
	
	/**
	 * 
	 * @param array $options
	 * @return void
	 */
	public function delete($options = array()){
		
		// Handle options
		if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
	
		// Perform delete
		$this->database->deleteAtom($this->id, $this->concept);

		// Close transaction
		$this->database->closeTransaction($this->concept . ' deleted', false, null, false);
		
	}
	
	/**
	 * Processes array of patches (according to specifications: JSON Patch is specified in RFC 6902 from the IETF)
	 * @param InterfaceObject|NULL $interface specifies the interface of this transaction
	 * @param array $patches contains all patches that need to be applied to this atom
	 * @param array $options 
	 * @return void
	 */
	public function patch($interface, $pathEntry, $patches, $options = array()){
		
		// Handle options
		if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
		$successMessage = isset($options['successMessage']) ? $options['successMessage'] : $this->concept . ' updated';
		
		// Perform patches
		$this->doPatches($interface, $pathEntry, $patches);
		
		// Close transaction
		$this->database->closeTransaction($successMessage, false, null, false);
	}
	
	/**********************************************************************************************
	 *
	 * PATCH functions
	 *
	 **********************************************************************************************/
	
	private function doPatches($interface, $pathEntry, $patches){
		$errorCount = 0;
		foreach ((array)$patches as $key => $patch){
			try{
				// Check patch
				if(!array_key_exists('op', $patch)) throw new Exception ("No 'op' (i.e. operation) specfied for patch #{$key}", 400);
				if(!array_key_exists('path', $patch)) throw new Exception ("No 'path' specfied for patch #{$key}", 400);
		
				$pathInfo = $this->walkIfcPath($patch['path'], $interface);
				$path = $pathEntry . $pathInfo['path'];
		
				// Checks
				if(!$pathInfo['ifc']->crudU) throw new Exception("Update is not allowed for path '{$path}'", 403);
		
				switch($patch['op']){
					case "replace" :
						if(!is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot patch replace '{$path}'. Path ends with resource", 405);
						if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch replace. No 'value' specfied in '{$path}'", 400);
						$this->doPatchReplace($pathInfo['ifc'], $pathInfo['srcAtom'], $patch['value']);
						break;
					case "add" :
						if(!is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot patch add '{$path}'. Path ends with resource", 405);
						if(!array_key_exists('value', $patch)) throw new Exception ("Cannot patch add. No 'value' specfied in '{$path}'", 400);
						$this->doPatchAdd($pathInfo['ifc'], $pathInfo['srcAtom'], $patch['value']);
						break;
					case "remove" :
						if(is_null($pathInfo['tgtAtom'])) throw new Exception ("Cannot patch remove '{$path}'. Missing resource identifier", 405);
						$this->doPatchRemove($pathInfo['ifc'], $pathInfo['srcAtom'], $pathInfo['tgtAtom']);
						break;
					default :
						throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
				}
			}catch (Exception $e){
				Notifications::addError($e->getMessage());
				$errorCount++;
			}
		}
		
		if($errorCount){
			$successMessage .= " WITH ERRORS";
			$totalPatches = count($patches);
			$processed = $totalPatches - $errorCount;
			Notifications::addInfo("{$processed}/{$totalPatches} patches processed. {$errorCount} errors.");
		}
	}
	
	/** 
	 * Performs editUpdate or editDelete based on patch replace operation
	 * @param InterfaceObject $ifc
	 * @param Atom $src
	 * @param mixed $value
	 * @return void
	 */
	private function doPatchReplace($ifc, $src, $value){
		
		// PatchReplace only works for UNI expressions. Otherwise, use patch remove and patch add
		if(!$ifc->univalent) throw new Exception("Cannot patch replace for non-univalent interface {$ifc->label}. Use patch remove + add instead", 500);
		
		/******* Perform patch *********/
		
		// Interface is property
		if($ifc->isProperty && !$ifc->isIdent){
			// Throw error when patch value is something else then true, false or null 
			if(!(is_bool($value) || is_null($value))) throw new Exception("Interface {$ifc->label} is property, boolean expected, non-boolean provided");
			
			// When true
			if($value){						
				$this->database->editUpdate($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $src->id, $ifc->tgtConcept);
			// When false or null
			}else{
				$this->database->editDelete($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $src->id, $ifc->tgtConcept);
			}
			
		// Interface is a relation to an object
		}elseif($ifc->tgtConceptIsObject){
			throw new Exception("Cannot patch replace for object reference in interface '{$ifc->label}'. Use patch remove + add instead", 500);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$ifc->tgtConceptIsObject){
			
			// Replace by nothing => editDelete
			if(is_null($value)){
				$this->database->editDelete($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, null, $ifc->tgtConcept);
			
			// Replace by other atom => editUpdate
			}else{
				$this->database->editUpdate($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $value, $ifc->tgtConcept);
			}
		}else{
			throw new Exception ("Unknown patch replace. Please contact the application administrator", 500);
		}
	}
	
	/**
	 * Performs editUpdate based on patch add operation
	 * @param InterfaceObject $ifc
	 * @param Atom $src
	 * @param mixed $value
	 * @return void
	 */
	private function doPatchAdd($ifc, $src, $value){
		
		/******* Perform patch *********/
		
		// Interface is property
		if($ifc->isProperty && !$ifc->isIdent){
			// Properties must be treated as a 'replace', so not handled here
			throw new Exception("Cannot patch add for property '{$ifc->label}'. Use patch replace instead", 500);
		
		// Interface is a relation to an object
		}elseif($ifc->tgtConceptIsObject){
			
			// Check: If tgtAtom (value) does not exists and there is not crud create right, throw exception 
			if(!$this->database->atomExists($value, $ifc->tgtConcept) && !$ifc->crudC) throw new Exception ("Resource '{$ifc}[{$ifc->tgtConcept}]' does not exist and may not be created in {$ifc->label}", 403);
			
			$this->database->editUpdate($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $value, $ifc->tgtConcept);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$ifc->tgtConceptIsObject){
			
			// Check: If interface is univalent, throw exception
			if($ifc->univalent) throw new Exception("Cannot patch add for univalent interface {$ifc->label}. Use patch replace instead", 500);
			
			$this->database->editUpdate($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $value, $ifc->tgtConcept);
			
		}else{
			throw new Exception ("Unknown patch add. Please contact the application administrator", 500);
		}
	}
	
	/**
	 * Performs editDelete based on patch remove operation
	 * @param InterfaceObject $ifc
	 * @param Atom $src
	 * @param Atom $tgt
	 * @return void
	 */
	private function doPatchRemove($ifc, $src, $tgt){
				
		/******* Perform patch *********/
		
		// Interface is property
		if($ifc->isProperty && !$ifc->isIdent){
			// Properties must be treated as a 'replace', so not handled here
			throw new Exception("Cannot patch remove for property '{$ifc->label}'. Use patch replace instead", 500);
		
		// Interface is a relation to an object
		}elseif($ifc->tgtConceptIsObject){
			
			$this->database->editDelete($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $tgt->id, $ifc->tgtConcept);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$ifc->tgtConceptIsObject){
			if($ifc->univalent) throw new Exception("Cannot patch remove for univalent interface {$ifc->label}. Use patch replace instead", 500);
			
			$this->database->editDelete($ifc->relation, $ifc->relationIsFlipped, $src->id, $ifc->srcConcept, $tgt->id, $ifc->tgtConcept);
			
		}else{
			throw new Exception ("Unknown patch add. Please contact the application administrator", 500);
		}
		
	}
	
	/**********************************************************************************************
	 *
	 * Helper functions
	 *
	 **********************************************************************************************/
	
	/**
	 * Finds the subinterface, srcAtom, tgtAtom combination given a patch path
	 * @param string $path starts with '/' after which a sequence of 'ifc/atom/ifc/atom/ifc/atom/etc' can be placed
	 * @param InterfaceObject|null $interface start walking path from a specific (sub)interface  
	 * @return array (InterfaceObject ifc, Atom srcAtom, Atom|null tgtAtom)
	 */
	public function walkIfcPath($path, $ifc = null){
		$session = Session::singleton();
		
		if(!$this->atomExists()) throw new Exception ("Resource '{$this->id}[{$this->concept}]' not found", 404);
		
		$srcAtomId = $this->id;
		$tgtAtomId = null;
		
		// remove root slash (e.g. '/Projects/xyz/..') and trailing slash (e.g. '../Projects/xyz/')
		$path = trim($path, '/');
		$pathArr = explode('/', $path);
		$path = '/' . $path;
		if(empty($pathArr)) throw new Exception ("No interface path provided", 500); 

		while (count($pathArr)){
			// Ifc
			$interfaceId = array_shift($pathArr);
			$ifc = InterfaceObject::getSubinterface($interfaceId, $ifc);
			
			// Checks
			if($ifc === false) throw new Exception("Interface path does not exists: '$path'", 500);
			if($ifc->isTopLevelIfc && !$session->isAccessibleIfc($ifc->id)) throw new Exception("Interface is not accessible for active roles or accessible roles (login)", 401); // 401: Unauthorized
			if((!$ifc->crudR) && (count($pathArr) > 1)) throw new Exception ("Read not allowed for '{$ifc->id}' in path '$path'", 405); // crudR required to walk the path futher when this is not the last ifc part in the path (count > 1).

			// Set tgtAtom as srcAtom, but skip the first time
			if(!is_null($tgtAtomId)) $srcAtomId = $tgtAtomId;
			
			// Set new tgtAtom
			$tgtAtomId = array_shift($pathArr); // Returns the shifted value, or NULL if array is empty or is not an array.
			
			// Check if tgtAtom is part of (sub)interface
			if(!is_null($tgtAtomId)){
				$idEsc = $this->database->escape($srcAtomId);
				$query = "SELECT DISTINCT `tgt` FROM ($ifc->expressionSQL) AS `results` WHERE `src` = '$idEsc' AND `tgt` IS NOT NULL";
				$tgtAtomIds = array_column($this->database->Exe($query), 'tgt');
				
				if(!in_array($tgtAtomId, $tgtAtomIds)) throw new Exception ("Resource '{$tgtAtomId}[{$ifc->tgtConcept}]' not found", 404);
			}			
		
		}
		$srcAtom = new Atom($srcAtomId, $ifc->srcConcept);
		$tgtAtom = is_null($tgtAtomId) ? null : new Atom($tgtAtomId, $ifc->tgtConcept);
		
		return array('ifc' => $ifc, 'srcAtom' => $srcAtom, 'tgtAtom' => $tgtAtom, 'path' => $path);
		
	}
	
	
	/**
	 * Coversion of php variables to json according to Ampersand technical types (TTypes)
	 * @param mixed $value
	 * @param string $concept
	 * @return mixed
	 */
	public function typeConversion($value, $concept){
		switch(Concept::getTypeRepresentation($concept)){
			case "ALPHANUMERIC" :
			case "BIGALPHANUMERIC" :
			case "HUGEALPHANUMERIC" :
			case "PASSWORD" :
			case "TYPEOFONE" :
				return (string) $value;
			case "BOOLEAN" :
				return (bool) $value;
			case "DATE" :
				$datetime = new DateTime($value);
				return $datetime->format('Y-m-d'); // format in ISO-8601 standard
			case "DATETIME" :
				$datetime = new DateTime($value, new DateTimeZone('UTC')); // datetimes are stored in UTC in database
				$datetime->setTimezone(new DateTimeZone(date_default_timezone_get())); // convert back to systemtime
				return $datetime->format(DateTime::ATOM); // format in ISO-8601 standard, i.e. 2005-08-15T15:52:01+00:00 (DateTime::ATOM)
			case "FLOAT" :
				return (float) $value;
			case "INTEGER" :
				return (int) $value;
			case "OBJECT" :
				return $value;
			default :
				throw new Exception("Unknown/unsupported representation type for concept $concept", 501);
		}
	}
}
?>