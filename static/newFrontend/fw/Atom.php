<?php

Class Atom {
	
	// Ampersand attributes
	public $id;
	public $label;
	public $view;
	public $concept;
	
	// JSON-LD attributes
	private $jsonld_id;
	private $jsonld_type;
		
	public function __construct($id, $concept = null, $viewId = null){
		
		// Ampersand attributes
		$this->id = $id;
		$this->concept = $concept;
		
		// View & label
		$this->view = $this->getView($viewId);
		is_null($this->view) ? $this->label = $this->id : $this->label = implode($this->view); // no view? label = id
		
		// JSON-LD attributes
		$this->jsonld_id = JSONLD_ID_PATH . $concept . '/' . $this->id;
		$this->jsonld_type = JSONLD_TYPE_PATH . $concept;

	}
	
	public function getAtom($interface = null){
		foreach(Concept::getAllInterfaces($this->concept) as $interfaceId) $interfaces[] = API_INTERFACES_PATH . $interfaceId . '/' . $this->id;
		
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
		$database = Database::singleton();
		
		if(is_null($tgtAtom)){
			$query = "SELECT DISTINCT `tgt` FROM (".$interface->expressionSQL.") AS results WHERE src='".addslashes($this->id)."' AND `tgt` IS NOT NULL";
			$tgtAtoms = array_column($database->Exe($query), 'tgt');
		}else{
			// Make sure that atom is in db (not necessarily the case: e.g. new atom)
			$database->addAtomToConcept($this->id, $this->concept);
			
			$tgtAtoms[] = $tgtAtom;
		}
		
		// define $arr as array if $interface is not univalent and its tgtDataType a primitive datatype (i.e. not concept) 
		if(!$interface->univalent && !($interface->tgtDataType == "concept")) $arr = array();
		
		foreach ($tgtAtoms as $tgtAtomId){
			$tgtAtom = new Atom($tgtAtomId, $interface->tgtConcept, $interface->viewId);

			// determine value atom 
			if($interface->isProperty && $interface->relation <> ''){ // $interface->relation <> '' because I is also a property and this is not the one we want
				$content = !is_null($tgtAtom->id);
				
			}elseif($interface->tgtDataType == "concept"){ // // TgtConcept of interface is a concept (i.e. not primitive datatype).
				$content = array();
				
				// Add @context for JSON-LD to rootElement
				if($rootElement) $content['@context'] = JSONLD_CONTEXT_PATH . $interface->id;
				
				// Add other elements
				$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
														, '@label' => $tgtAtom->label
				                    					, '@view' => $tgtAtom->view
													 	, '@type' => $tgtAtom->jsonld_type
													 	, 'id' => $tgtAtom->id));
				
			}else{ // TgtConcept of interface is primitive datatype
				if(strtolower($tgtAtom->id) == "true") $tgtAtom->id = true; // convert string "true" to boolval true
				if(strtolower($tgtAtom->id) == "false") $tgtAtom->id = false; // convert string "false" to boolval false
				
				$content = $tgtAtom->id;
			}
			
			// subinterfaces
			if(!empty($interface->subInterfaces) && $interface->tgtDataType != "concept") throw new Exception("TgtConcept of interface: '" . $interface->label . "' is primitive datatype and can not have subinterfaces", 501);
			foreach($interface->subInterfaces as $subinterface){
				$otherAtom = $tgtAtom->getContent($subinterface, false);
				$content[$subinterface->id] = $otherAtom;
				
			}
			
			// determine whether value of atom must be inserted as list or as single value
			if($interface->isProperty && $interface->relation <> ''){ // $interface->relation <> '' because I is also a property and this is not the one we want
				$arr = $content;
			}elseif($interface->univalent AND !($interface->tgtDataType == "concept")){ // in cause of univalent (i.e. count of $tgtAtoms <= 1) and a datatype (i.e. not concept)
				$arr = $content;
			}elseif(!($interface->tgtDataType == "concept")){
				$arr[] = $content;
			}else{
				$arr[$tgtAtom->id] = $content;
			}			
				
			unset($content);			
		
		}
		
		return $arr;

	}
	
	public function patch(&$interface, $request_data, $requestType){
		$database = Database::singleton();
		
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
		$before = current($before); // current(), returns first item of array. This is valid, because patchAtom() concerns exactly 1 atom.
		
		// Determine differences between current state ($before) and requested state ($request_data)
		$patches = JsonPatch::diff($before, $request_data);
		
		// Patch current state based on differences
		foreach ((array)$patches as $key => $patch){
			switch($patch['op']){ // operations
				case "replace" :
					$pathArr = explode('/', $patch['path']);
						
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
					
					// find the right subinterface
					while (count($pathArr)){
						$interfaceId = array_shift($pathArr);
						
						// if path starts with '@' skip
						if(substr($interfaceId, 0, 1) == '@') break 2; // break while and switch
						
						$tgtInterface = InterfaceObject::getSubinterface($tgtInterface, $interfaceId);
						
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom 	
						
					}
					
					// replace property value (true/false) by the srcAtomId TODO: place below within editable check.
					if ($tgtInterface->isProperty){
						if(!is_bool($patch['value'])) throw new Exception("Interface $tgtInterface->label is property, boolean expected, non-boolean provided");
						if($patch['value']){						
							$database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $srcAtom, $tgtInterface->tgtConcept);
						}else{
							$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $srcAtom, $tgtInterface->tgtConcept);
						}
						
						break;
					}
					
					// if tgtDataType is a concept (i.e. ! prim. datatype), use key of object in $patch['value']
					if (is_null($tgtAtom) AND $tgtInterface->tgtDataType == "concept") $tgtAtom = key($patch['value']);
					// elseif tgtDataType is a primitieve datatype (i.e. !concept), use patch value instead of path index.
					elseif ($tgtInterface->tgtDataType != "concept") $tgtAtom = $patch['value'];
					// else
					else throw new Exception('Unknown variant of patch replace: ' . $patch['op'] . ' on ' . $patch['path'], 501);
					
					// perform editUpdate
					if($tgtInterface->editable){
						if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true); // convert true and false into "true" and "false" strings
						
						// in case $tgtAtom is empty string -> perform remove instead of replace.
						if($tgtAtom !== ''){
							$originalAtom = $tgtInterface->univalent ? null : JsonPatch::get($before, $patch['path']);
							$database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept, $originalAtom);
						}else{
							// the final $tgtAtom is not provided, so we have to get this value to perform the editDelete function
							$tgtAtom = JsonPatch::get($before, $patch['path']);
							$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
						}					
					}else{
						Notifications::addError($tgtInterface->label . " is not editable in interface '" . $interface->label . "'");
					}
					
					break;
					
				/*
				 *
				 * PROPERTIES are always a 'replace', so no dealing with them here
				 */
				case "add" :					
					$pathArr = explode('/', $patch['path']);
					
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
					
					// find the right subinterface
					while (count($pathArr)){
						$interfaceId = array_shift($pathArr);
						
						// if path starts with '@' skip
						if(substr($interfaceId, 0, 1) == '@') break 2; // break while and switch
					
						$tgtInterface = InterfaceObject::getSubinterface($tgtInterface, $interfaceId);
					
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom
					
					}
					
					// if tgtDataType is a primitieve datatype (i.e. !concept), use patch value instead of path index.
					if (!($tgtInterface->tgtDataType == "concept")) $tgtAtom = $patch['value'];
					
					// perform editUpdate
					if($tgtInterface->editable){
						if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true); // convert true and false into "true" and "false" strings
					
						// in case $tgtAtom is null (result of empty array in array_shift) -> provide error.
						if(is_null($tgtAtom)) Notifications::addError($tgtInterface->label . ": add operation without value '");
						
						$database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
						
					}else{
						Notifications::addError($tgtInterface->label . " is not editable in interface '" . $interface->label . "'");
					}
					
					break;
					
				/* 
				 * 
				 * PROPERTIES are always a 'replace', so no dealing with them here
				 */
				case "remove" :
					$pathArr = explode('/', $patch['path']);
					
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
					
					// find the right subinterface
					while (count($pathArr)){
						$interfaceId = array_shift($pathArr);
						
						// if path starts with '@' skip
						if(substr($interfaceId, 0, 1) == '@') break 2; // break while and switch
						
						$tgtInterface = InterfaceObject::getSubinterface($tgtInterface, $interfaceId);
						
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom

					}
					
					// perform editDelete
					if($tgtInterface->editable){
						// in case of 'remove' for a link to a non-concept (i.e. datatype), the final $tgtAtom value is not provided, so we have to get this value to perform the editDelete function
						// two situations: 1) expr is UNI -> path is '/<attr name>' or 2) expr is not UNI -> path is '/<attr name>/<key>', where key is entry in array of values.
						if(!($tgtInterface->tgtDataType == "concept")) $tgtAtom = JsonPatch::get($before, $patch['path']);
						$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
					}else{
						Notifications::addError($tgtInterface->label . " is not editable in interface '" . $interface->label . "'");
					}
					
					break;
			}
		}
		
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $database->closeTransaction('Updated', false, $databaseCommit);
		
		return $patches;
		
	}
	
	public function delete(){
		$database = Database::singleton();
		
		if(is_null($this->concept)) throw new Exception('Concept type of atom ' . $this->id . ' not provided', 500);
		
		$database->deleteAtom($this->id, $this->concept);
		
		// Close transaction => ROLLBACK or COMMIT.
		$database->closeTransaction('Atom deleted', false);
		
		return;
		
	}
	
	private function getView($viewId = null){
		$database = Database::singleton();
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
					$query = "SELECT DISTINCT `tgt` FROM (".$viewSegment['expSQL'].") AS results WHERE src='".addslashes($this->id)."' AND `tgt` IS NOT NULL";
					$tgtAtoms = array_column($database->Exe($query), 'tgt');
					
					$txt = count($tgtAtoms) ? $tgtAtoms[0] : $this->id; // this can happen in a create-new interface when the view fields have not yet beenfilled out, while the atom is shown
					$viewStrs[$viewSegment['label']] = htmlSpecialChars($txt);
				}
			}
			return $viewStrs;
		}
	}
}

?>