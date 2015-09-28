<?php

Class Atom {
	
	// Ampersand attributes
	public $id;
	public $label;
	public $view;
	public $concept;
	
	private $newContent; // To temporarily store changed atom content 
	
	// JSON-LD attributes
	private $jsonld_id;
	private $jsonld_type;
	private $database;
		
	public function __construct($id, $concept, $viewId = null){
		$this->database = Database::singleton();
		
		// Ampersand attributes
		$this->id = $id;
		$this->concept = $concept;
		
		// View & label
		$this->view = $this->getView($viewId);
		$this->label = is_null($this->view) ? $this->id : implode($this->view); // no view? label = id
		
		// JSON-LD attributes
		$this->jsonld_id = JSONLD_ID_PATH . $concept . '/' . $this->id;
		$this->jsonld_type = JSONLD_TYPE_PATH . $concept;

	}
	
	/*
	 * Note! Mysql is case insensitive for primary keys, e.g. atom 'True' ==  'TRUE'
	 */
	public function atomExists(){
		return $this->database->atomExists($this->id, $this->concept);
	}
	
	public function getAtom($interface = null){
		foreach(InterfaceObject::getAllInterfacesForConcept($this->concept) as $interfaceId) $interfaces[] = $this->jsonld_id . '/' . $interfaceId;
		
		$result =  array('@id' => $this->jsonld_id
						,'@label' => $this->label
		        		,'@view' => $this->view
						,'@type' => $this->jsonld_type
						,'@interfaces' => $interfaces
						,'id' => $this->id
						);

		return $result;
	}
	
	/*
	 * var $rootElement specifies if this Atom is the root element (true), or a subelement (false) in an interface
	 * var $tgtAtom specifies that a specific tgtAtom must be used instead of querying the tgtAtoms with the expressionSQL of the interface
	 * var $inclLinktoData specifies if data from LINKTO (ref) subinterfaces must be included or not.
	 * var $arrayType specifies if the arrays in the result are 'assoc' (associative, key index) or 'num' (numeric index).
	 * var $metaData specifies if meta data about objects must be included or not
	 */
	public function getContent($interface, $rootElement = true, $tgtAtom = null, $inclLinktoData = false, $arrayType = "assoc", $metaData = true){
		$session = Session::singleton();
		
		if(is_null($tgtAtom)){
			$idEsc = $this->database->escape($this->id);
			$query = "SELECT DISTINCT `tgt` FROM ($interface->expressionSQL) AS `results` WHERE `src` = '$idEsc' AND `tgt` IS NOT NULL";
			$tgtAtoms = array_column($this->database->Exe($query), 'tgt');
		}else{
			// Make sure that atom is in db (not necessarily the case: e.g. new atom)
			$this->database->addAtomToConcept($this->id, $this->concept);
			
			$tgtAtoms[] = $tgtAtom;
		}
		
		foreach ($tgtAtoms as $tgtAtomId){
			$tgtAtom = new Atom($tgtAtomId, $interface->tgtConcept, $interface->viewId);
			
			// Add @context for JSON-LD to rootElement
			if($rootElement) $content['@context'] = JSONLD_CONTEXT_PATH . $interface->id;
			
			// Leaf
			if(empty($interface->subInterfaces) && empty($interface->refInterfaceId)){
				
				// Property
				if($interface->isProperty && !$interface->isIdent){
					$content = !is_null($tgtAtom->id); // convert NULL into false and everything else in true
				
				// Object
				}elseif($interface->tgtConceptIsObject){
					$content = array();
					
					// Add meta data
					if($metaData){
						
						// Define interface(s) to navigate to for this tgtAtom
						$atomInterfaces = array();
						if($interface->isLinkTo && !$inclLinktoData && $session->role->isInterfaceForRole($interface->refInterfaceId)) $atomInterfaces[] = array('id' => $interface->refInterfaceId, 'label' => $interface->refInterfaceId);
						elseif(isset($session->role)) $atomInterfaces = array_map(function($o) { return array('id' => $o->id, 'label' => $o->label); }, $session->role->getInterfacesForConcept($interface->tgtConcept));
							
						// Add meta data elements
						$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
								, '@label' => $tgtAtom->label
								, '@view' => $tgtAtom->view
								, '@type' => $tgtAtom->jsonld_type
								, '@interfaces' => $atomInterfaces
								, '_sortValues_' => array())
						);
					}
					
					// Add id TODO:can be removed when angular templates use @id instead of id
					$content = array_merge($content, array (  'id' => $tgtAtom->id));
					
				// Scalar
				}else{
					$content = $this->typeConversion($tgtAtom->id, $interface->tgtConcept); // TODO: now same conversion as to database is used, maybe this must be changed to JSON types (or the json_encode/decode does this automaticaly?)
				}
				
			// Tree
			}else{
				$content = array();
					
				// Add meta data
				if($metaData){
					
					// Define interface(s) to navigate to for this tgtAtom
					$atomInterfaces = array();
					if($interface->isLinkTo && !$inclLinktoData && $session->role->isInterfaceForRole($interface->refInterfaceId)) $atomInterfaces[] = array('id' => $interface->refInterfaceId, 'label' => $interface->refInterfaceId);
					elseif(isset($session->role)) $atomInterfaces = array_map(function($o) { return array('id' => $o->id, 'label' => $o->label); }, $session->role->getInterfacesForConcept($interface->tgtConcept));
						
					// Add meta data elements
					$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
							, '@label' => $tgtAtom->label
							, '@view' => $tgtAtom->view
							, '@type' => $tgtAtom->jsonld_type
							, '@interfaces' => $atomInterfaces
							, '_sortValues_' => array())
					);
				}
				
				// Add id TODO:can be removed when angular templates use @id instead of id
				$content = array_merge($content, array (  'id' => $tgtAtom->id));
				
				// Subinterfaces
				if(!empty($interface->subInterfaces)){
					if(!$interface->tgtConceptIsObject) throw new Exception("TgtConcept of interface: '" . $interface->label . "' is scalar and can not have subinterfaces", 501);
				
					foreach($interface->subInterfaces as $subinterface){
						$otherAtom = $tgtAtom->getContent($subinterface, false, null, $inclLinktoData, $arrayType, $metaData);
						$content[$subinterface->id] = $otherAtom;
							
						// _sortValues_ (if subInterface is uni)
						if($subinterface->univalent && $metaData){
							// property
							if(is_bool($otherAtom)) $content['_sortValues_'][$subinterface->id] = $otherAtom;
							// object
							elseif($subinterface->tgtConceptIsObject) $content['_sortValues_'][$subinterface->id] = current((array)$otherAtom)['@label'];
							// scalar
							else $content['_sortValues_'][$subinterface->id] = $otherAtom;
						}
					}
				}

				// Ref subinterfaces (for LINKTO interfaces only when $inclLinktoData = true)
				if(!empty($interface->refInterfaceId) && (!$interface->isLinkTo || $inclLinktoData)){
					if(!$interface->tgtConceptIsObject) throw new Exception("TgtConcept of interface: '" . $interface->label . "' is scalar and can not have a ref interface defined", 501);
				
					$refInterface = new InterfaceObject($interface->refInterfaceId, null);
					foreach($refInterface->subInterfaces as $subinterface){
						$otherAtom = $tgtAtom->getContent($subinterface, false, null, $inclLinktoData, $arrayType, $metaData);
						$content[$subinterface->id] = $otherAtom;
							
						// _sortValues_ (if subInterface is uni)
						if($subinterface->univalent && $metaData){
							// property
							if(is_bool($otherAtom)) $content['_sortValues_'][$subinterface->id] = $otherAtom;
							// object
							elseif($subinterface->tgtConceptIsObject) $content['_sortValues_'][$subinterface->id] = current((array)$otherAtom)['@label'];
							// scalar
							else $content['_sortValues_'][$subinterface->id] = $otherAtom;
						}
					}
				}				
			}
			
			// Determine whether value of atom must be inserted as list or as single value
			
			// Properties are represented as single value
			if($interface->isProperty && !$interface->isIdent && empty($interface->subInterfaces) && empty($interface->refInterfaceId)){
				$arr = $content;
			// Object are always inserted as array
			}elseif($interface->tgtConceptIsObject){
				switch($arrayType){
					case "num" :
						$arr[] = $content;
						break;
					case "assoc" :
					default :
						$arr[$content['id']] = $content;
						break;
				}
			// Non-object UNI results are inserted as single value
			}elseif($interface->univalent){
				$arr = $content;
			// Non-object Non-UNI results are inserted as array 
			}else{
				$arr[] = $content;
			}
			unset($content);
		}
		
		return $arr;
	}
	
	public function put(&$interface, $request_data, $requestType){		
		switch($requestType){
			case 'feedback' :
				$databaseCommit = false;
				break;
			case 'promise' :
				$databaseCommit = true;
				break;
			default :
				throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
		}
		
		// Get current state of atom
		$before = $this->getContent($interface, true, $this->id);
		$before = current($before); // current(), returns first item of array. This is valid, because put() concerns exactly 1 atom.
		
		// Determine differences between current state ($before) and requested state ($request_data)
		$patches = JsonPatch::diff($before, $request_data);
		
		// Put current state based on differences
		foreach ((array)$patches as $key => $patch){
			switch($patch['op']){
				case "replace" :
					$this->doPatchReplace($patch, $interface, $before);
					break;
				case "add" :
					$this->doPatchAdd($patch, $interface, $before);
					break;
				case "remove" :
					$this->doPatchRemove($patch, $interface, $before);
					break;
				default :
					throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
			}
		}
		
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $this->database->closeTransaction('Updated', false, $databaseCommit);
		
		return array(	'patches' 				=> $patches
					,	'content' 				=> current((array)$this->newContent) // current(), returns first item of array. This is valid, because patchAtom() concerns exactly 1 atom.
					,	'notifications' 		=> Notifications::getAll()
					,	'invariantRulesHold'	=> $invariantRulesHold
					,	'requestType'			=> $requestType
					);
	}
	
	public function patch(&$interface, $patches, $requestType){		
		switch($requestType){
			case 'feedback' :
				$databaseCommit = false;
				break;
			case 'promise' :
				break;
				$databaseCommit = true;
			default :
				throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
		}
		
		// Get current state of atom
		$before = $this->getContent($interface, true, $this->id);
		$before = current($before); // current(), returns first item of array. This is valid, because put() concerns exactly 1 atom.
		
		// Patch
		foreach ((array)$patches as $key => $patch){
			switch($patch['op']){
				case "replace" :
					$this->doPatchReplace($patch, $interface, $before);
					break;
				case "add" :
					$this->doPatchAdd($patch, $interface, $before);
					break;
				case "remove" :
					$this->doPatchRemove($patch, $interface, $before);
					break;
				default :
					throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
			}
		}
		
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $this->database->closeTransaction('Updated', false, $databaseCommit);
		
		return array(	'patches' 				=> $patches
					,	'content' 				=> current((array)$this->newContent) // current(), returns first item of array. This is valid, because patchAtom() concerns exactly 1 atom.
					,	'notifications' 		=> Notifications::getAll()
					,	'invariantRulesHold'	=> $invariantRulesHold
					,	'requestType'			=> $requestType
					);
	}
	
	private function doPatchReplace($patch, $interface, $before){
		
		$pathArr = explode('/', $patch['path']);
			
		$tgtInterface = $interface;
		$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
		
		// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
		if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
		
		// find the right subinterface
		while (count($pathArr)){
			$interfaceId = array_shift($pathArr);
			
			// if path starts with '@' skip
			if(substr($interfaceId, 0, 1) == '@') return; // break function
			if($interfaceId == '_sortValues_') return; // break function
			
			$tgtInterface = InterfaceObject::getSubinterface($tgtInterface, $interfaceId);
			
			$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
			$tgtAtom = array_shift($pathArr); // set tgtAtom 	
			
		}
		
		// Check if interface is editable
		if(!$tgtInterface->editable){
			Notifications::addError($tgtInterface->label . " is not editable in interface '" . $interface->label . "'");
			return; 
		}
		
		// Convert true and false into "true" and "false" strings
		if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true);
		
		/******* Perform edit *********/
		
		// Interface is property
		if ($tgtInterface->isProperty && !$tgtInterface->isIdent){
			// Throw error when patch value is something else then true, false or null 
			if(!(is_bool($patch['value']) || is_null($patch['value']))) throw new Exception("Interface $tgtInterface->label is property, boolean expected, non-boolean provided");
			
			// When true
			if($patch['value']){						
				$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $srcAtom, $tgtInterface->tgtConcept);
			// When false or null
			}else{
				$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $srcAtom, $tgtInterface->tgtConcept);
			}
			
		// Interface is a relation to an object
		}elseif($tgtInterface->tgtConceptIsObject){
			// Replace by nothing => editDelete
			if(empty($patch['value'])){
				// The $tgtAtom(s) is/are not provided, so we have to get this value to perform the editDelete function
				try{
					$tgtAtoms = JsonPatch::get($before, $patch['path']);
				}catch(Exception $e){
					Notifications::addError($e->getMessage());
				}
				
				foreach ((array)$tgtAtoms as $key => $val){
					$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $key, $tgtInterface->tgtConcept);
				}
			// Replace by other atom(s) => editUpdate
			}else{
				foreach ((array)$patch['value'] as $key => $val){
					$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $key, $tgtInterface->tgtConcept);
				}
			}
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$tgtInterface->tgtConceptIsObject){
			if(is_bool($patch['value'])) $patch['value'] = var_export($patch['value'], true);
			
			// Replace by nothing => editDelete
			if(empty($patch['value'])){
				// The $tgtAtom(s) is/are not provided, so we have to get this value to perform the editDelete function
				try{
					$tgtAtoms = JsonPatch::get($before, $patch['path']);
				}catch(Exception $e){
					Notifications::addError($e->getMessage());
				}
				
				foreach ((array)$tgtAtoms as $val){
					$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $val, $tgtInterface->tgtConcept);
				}
			
			// Replace by other atom(s) => editUpdate
			}else{
				foreach ((array)$patch['value'] as $val){
					$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $val, $tgtInterface->tgtConcept, $originalAtom);
				}
			}
		}
	}
		
	private function doPatchAdd($patch, $interface, $before){
								
		$pathArr = explode('/', $patch['path']);
		
		$tgtInterface = $interface;
		$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
		
		// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
		if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
		
		// find the right subinterface
		while (count($pathArr)){
			$interfaceId = array_shift($pathArr);
			
			// if path starts with '@' skip
			if(substr($interfaceId, 0, 1) == '@') return; // break function
			if($interfaceId == '_sortValues_') return; // break function
		
			$tgtInterface = InterfaceObject::getSubinterface($tgtInterface, $interfaceId);
		
			$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
			$tgtAtom = array_shift($pathArr); // set tgtAtom
		
		}
		
		// Check if interface is editable
		if(!$tgtInterface->editable){
			Notifications::addError($tgtInterface->label . " is not editable in interface '" . $interface->label . "'");
			return;
		}
		
		// Convert true and false into "true" and "false" strings
		if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true);
		if(is_bool($patch['value'])) $patch['value'] = var_export($patch['value'], true);		
		
		/******* Perform edit *********
		 * Properties are always a 'replace', so no dealing with them here
		 */
		
		/* Interface is a relation to an object
		 */
		if($tgtInterface->tgtConceptIsObject){
			$tgtAtom = $patch['value']['id'];
			
			// In case $tgtAtom is null provide error.
			if(is_null($tgtAtom)) Notifications::addError($tgtInterface->label . ": add operation without value '");
			
			$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$tgtInterface->tgtConceptIsObject){
			$tgtAtom = $patch['value'];
			
			// In case $tgtAtom is null provide error.
			if(is_null($tgtAtom)) Notifications::addError($tgtInterface->label . ": add operation without value '");
				
			$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
		}
	}
	
	private function doPatchRemove($patch, $interface, $before){
				
		$pathArr = explode('/', $patch['path']);
		
		$tgtInterface = $interface;
		$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
		
		// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
		if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
		
		// find the right subinterface
		while (count($pathArr)){
			$interfaceId = array_shift($pathArr);
			
			// if path starts with '@' skip
			if(substr($interfaceId, 0, 1) == '@') return; // break function
			if($interfaceId == '_sortValues_') return; // break function
			
			$tgtInterface = InterfaceObject::getSubinterface($tgtInterface, $interfaceId);
			
			$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
			$tgtAtom = array_shift($pathArr); // set tgtAtom

		}
		
		// Check if interface is editable
		if(!$tgtInterface->editable){
			Notifications::addError($tgtInterface->label . " is not editable in interface '" . $interface->label . "'");
			return;
		}
		
		/******* Perform edit *********
		 * Properties are always a 'replace', so no dealing with them here
		 */
		
		/* Interface is a relation to an object
		 */
		if($tgtInterface->tgtConceptIsObject){
		
			$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
		
		/* Interface is a relation to a scalar (i.e. not an object)
		 * Two situations:
		 * 1) Interface is UNI -> not handled here, this is detected as a replace to ''
		 * 2) Interface is not UNI -> $tgtAtom is index of array, so we have to get the corresponding value
		 */
		}elseif(!$tgtInterface->tgtConceptIsObject){
			try{
				$tgtAtom = JsonPatch::get($before, $patch['path']);
			}catch(Exception $e){
				Notifications::addError($e->getMessage());
			}
			$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
		}
	}
	
	public function setNewContent($interface){
		
		$this->newContent = $this->getContent($interface, true, $this->id);

	}
	
	public function delete($requestType){	
		if(is_null($this->concept)) throw new Exception('Concept type of atom ' . $this->id . ' not provided', 500);
		
		switch($requestType){
			case 'feedback' :
				$databaseCommit = false;
				break;
			case 'promise' :
				$databaseCommit = true;
				break;
			default :
				throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
		}
		
		$this->database->deleteAtom($this->id, $this->concept);
		
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $this->database->closeTransaction('Atom deleted', false, $databaseCommit, false);
		
		return array('notifications' 		=> Notifications::getAll()
					,'invariantRulesHold'	=> $invariantRulesHold
					,'requestType'			=> $requestType
		);
		
	}
	
	public function post(&$interface, $request_data, $requestType){		
		switch($requestType){
			case 'feedback' :
				$databaseCommit = false;
				break;
			case 'promise' :
				$databaseCommit = true;
				break;
			default :
				throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
		}
		
		// Get current state of atom
		$before = $this->getContent($interface, true, $this->id);
		$before = current($before); // current(), returns first item of array. This is valid, because put() concerns exactly 1 atom.
		
		// Determine differences between current state ($before) and requested state ($request_data)
		$patches = JsonPatch::diff($before, $request_data);
		
		// Skip remove operations, because it is a POST operation and there are no values in de DB yet
		$patches = array_filter($patches, function($patch){return $patch['op'] <> 'remove';});
		
		// Patch
		foreach ((array)$patches as $key => $patch){
			switch($patch['op']){
				case "replace" :
					$this->doPatchReplace($patch, $interface, $before);
					break;
				case "add" :
					$this->doPatchAdd($patch, $interface, $before);
					break;
				case "remove" :
					$this->doPatchRemove($patch, $interface, $before);
					break;
				default :
					throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
			}
		}
		
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $this->database->closeTransaction('Updated', false, $databaseCommit);
		
		return array(	'patches' 				=> $patches
					,	'content' 				=> current((array)$this->newContent) // current(), returns first item of array. This is valid, because patchAtom() concerns exactly 1 atom.
					,	'notifications' 		=> Notifications::getAll()
					,	'invariantRulesHold'	=> $invariantRulesHold
					,	'requestType'			=> $requestType
					);
	}
	
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
	
	/*
	 * Conversion to PHP/JSON types
	 */
	public function typeConversion($value, $concept){
		switch(Concept::getTypeRepresentation($concept)){
			case "DATE" :
				$date = new DateTime($value);
				return $date->format('Y-m-d');
			case "DATETIME" :
				$datetime = new DateTime($value);
				return $datetime->format('Y-m-d H:i:s');
			case "INTEGER" :
				return (int) $value;
			case "BOOLEAN" :
				return (bool) $value;
			case "DECIMAL" :
				return (float) $value;
			default :
				return $value;
		}
	
	}
}

?>