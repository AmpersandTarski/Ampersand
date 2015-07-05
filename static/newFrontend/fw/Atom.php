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
		foreach(Concept::getAllInterfaces($this->concept) as $interfaceId) $interfaces[] = $this->jsonld_id . '/' . $interfaceId;
		
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
	 */
	public function getContent($interface, $rootElement = true, $tgtAtom = null){
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
		
		// defaults 
		if(!$interface->univalent && !($interface->tgtDataType == "Object")) $arr = array(); // define $arr as array if $interface is not univalent and representation of tgtconcept not an object
		else $arr = null;
		
		foreach ($tgtAtoms as $tgtAtomId){
			$tgtAtom = new Atom($tgtAtomId, $interface->tgtConcept, $interface->viewId);

			// determine value atom 
			if($interface->isProperty && empty($interface->subInterfaces) && $interface->relation <> ''){ // $interface->relation <> '' because I is also a property and this is not the one we want
				$content = !is_null($tgtAtom->id);
				
			}elseif($interface->tgtDataType == "Object"){
				$content = array();
				
				// Add @context for JSON-LD to rootElement
				if($rootElement) $content['@context'] = JSONLD_CONTEXT_PATH . $interface->id;
				
				// Define interface(s) to navigate to for this tgtAtom
				if($interface->isLinkTo) $atomInterfaces = array($interface->refInterfaceId);
				elseif(isset($session->role)) $atomInterfaces = array_map(function($o) { return $o->id; }, $session->role->getInterfacesForConcept($interface->tgtConcept));
				else $atomInterfaces = array(); // TODO: add interfaces not connected to a specific role
				
				// Add other elements
				$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
														, '@label' => $tgtAtom->label
				                    					, '@view' => $tgtAtom->view
													 	, '@type' => $tgtAtom->jsonld_type
														, '@interfaces' => $atomInterfaces
														, '_sortValues_' => array()
													 	, 'id' => $tgtAtom->id));
				
			}elseif($interface->tgtDataType == "Date"){
				$date = new DateTime($tgtAtom->id);
				$content = $date->format('Y-m-d');
			
			}else{ // Representation of tgtconcept of interface is scalar (i.e. not object)
				// if(strtolower($tgtAtom->id) == "true") $tgtAtom->id = true; // convert string "true" to boolval true
				// if(strtolower($tgtAtom->id) == "false") $tgtAtom->id = false; // convert string "false" to boolval false
				
				$content = $tgtAtom->id;
			}
			
			// subinterfaces
			if(!empty($interface->subInterfaces) && $interface->tgtDataType != "Object") throw new Exception("TgtConcept of interface: '" . $interface->label . "' is primitive datatype and can not have subinterfaces", 501);
			foreach($interface->subInterfaces as $subinterface){
				$otherAtom = $tgtAtom->getContent($subinterface, false);
				$content[$subinterface->id] = $otherAtom;
				
				// _sortValues_ (if subInterface is uni)
				if($subinterface->univalent){
					$content['_sortValues_'][$subinterface->id] = ($subinterface->tgtDataType == "Object") ? current((array)$otherAtom)['@label'] : $otherAtom;
				}
				
			}
			
			// determine whether value of atom must be inserted as list or as single value
			if($interface->isProperty && $interface->relation <> ''){ // $interface->relation <> '' because I is also a property and this is not the one we want
				$arr = $content;
			}elseif($interface->tgtDataType == "Object"){
				$arr[$content['id']] = $content;
			}elseif($interface->univalent){
				$arr = $content;
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
		if ($tgtInterface->isProperty){
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
		}elseif($tgtInterface->tgtDataType == "Object"){
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
		}elseif($tgtInterface->tgtDataType != "Object"){
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
		if($tgtInterface->tgtDataType == "Object"){
			$tgtAtom = $patch['value']['id'];
			
			// In case $tgtAtom is null provide error.
			if(is_null($tgtAtom)) Notifications::addError($tgtInterface->label . ": add operation without value '");
			
			$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif($tgtInterface->tgtDataType != "Object"){
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
		if($tgtInterface->tgtDataType == "Object"){
		
			$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
		
		/* Interface is a relation to a scalar (i.e. not an object)
		 * Two situations:
		 * 1) Interface is UNI -> not handled here, this is detected as a replace to ''
		 * 2) Interface is not UNI -> $tgtAtom is index of array, so we have to get the corresponding value
		 */
		}elseif($tgtInterface->tgtDataType != "Object"){
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
				
				if ($viewSegment['segmentType'] == 'Text'){ 
					$viewStrs[$viewSegment['label']] = htmlSpecialChars($viewSegment['Text']);
				
				}elseif($viewSegment['segmentType'] == 'Html'){
					$viewStrs[$viewSegment['label']] = $viewSegment['Html'];
				
				}else{
					$idEsc = $this->database->escape($this->id);
					$query = "SELECT DISTINCT `tgt` FROM ($viewSegment[expSQL]) AS `results` WHERE `src` = '$idEsc' AND `tgt` IS NOT NULL";
					$tgtAtoms = array_column($this->database->Exe($query), 'tgt');
					
					$txt = count($tgtAtoms) ? htmlSpecialChars($tgtAtoms[0]) : null;
					$viewStrs[$viewSegment['label']] = $txt;
				}
			}
			return $viewStrs;
		}
	}
}

?>