<?php

Class Atom {
	
	// Ampersand attributes
	public $id;
	public $label;
	public $concept;
	
	// JSON-LD attributes
	private $jsonld_id;
	private $jsonld_type;
		
	public function __construct($id, $concept = null){
		
		// Ampersand attributes
		$this->id = $id;
		$this->concept = $concept;
		$this->label = $this->getLabel();
		
		// JSON-LD attributes
		$this->jsonld_id = JSONLD_ID_PATH . $concept . '/' . $this->id;
		$this->jsonld_type = JSONLD_TYPE_PATH . $concept;

	}
	
	public function getAtom($interface = null){
		foreach(Concept::getAllInterfaces($this->concept) as $interfaceName) $interfaces[] = API_INTERFACES_PATH . $interfaceName . '/' . $this->id;
		
		$result =  array('@id' => $this->jsonld_id
						,'@label' => $this->label
						,'@type' => $this->jsonld_type
						,'@interfaces' => $interfaces
						,'id' => $this->id
						);

		return $result;
	}
	
	/*
	 * var $rootElement specifies if this Atom is the root element (true), or a subelement (false) in an interface
	 */
	public function getContent($interface, $rootElement = true){
		$database = Database::singleton();
		
		$query = "SELECT DISTINCT `tgt` FROM (".$interface->expressionSQL.") AS results WHERE src='".addslashes($this->id)."' AND `tgt` IS NOT NULL";
		$tgtAtoms = array_column($database->Exe($query), 'tgt');
		
		foreach ($tgtAtoms as $tgtAtomId){
			$tgtAtom = new Atom($tgtAtomId, $interface->tgtConcept);
				
			// determine value atom 
			if($interface->tgtDataType == "concept"){ // subinterface refers to other concept (i.e. not datatype).
				$content = array();
				
				// Add @context for JSON-LD to rootElement
				if($rootElement) $content['@context'] = JSONLD_CONTEXT_PATH . $interface->name;
				
				// Add other elements
				$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
														, '@label' => $tgtAtom->label
													 	, '@type' => $tgtAtom->jsonld_type
													 	, 'id' => $tgtAtom->id));
				
			}else{
				if(strtolower($tgtAtom->id) == "true") $tgtAtom->id = true; // convert string "true" to boolval true
				if(strtolower($tgtAtom->id) == "false") $tgtAtom->id = false; // convert string "false" to boolval false
				
				$content = $tgtAtom->id;
				
				
			}
			
			// subinterfaces
			foreach($interface->subInterfaces as $subinterface){
			
				$otherAtom = $tgtAtom->getContent($subinterface, false);
				$content[$subinterface->name] = $otherAtom;
				
			}
			
			// determine whether value of atom must be inserted as list or as single value
			if($interface->univalent AND !($interface->tgtDataType == "concept")){ // in cause of univalent (i.e. count of $tgtAtoms <= 1) and a datatype (i.e. not concept)
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
	
	public function patch(&$interface, $request_data){
		$database = Database::singleton();
		
		// Check if new Atom
		$before = $this->getContent($interface);
		if($before == false){ // was empty($before = $this->getContent($interface)), but prior to PHP 5.5, empty() only supports variables, not expressions.
			$database->addAtomToConcept($this->id, $this->concept);
			$before = $this->getContent($interface);
		}
		$before = current($before);
		
		$patches = JsonPatch::diff($before, $request_data);
		
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
						$interfaceName = array_shift($pathArr);
						
						// if path starts with '@' skip
						if(substr($interfaceName, 0, 1) == '@') break 2; // break while and switch
						
						$tgtInterface = ObjectInterface::getSubinterface($tgtInterface, $interfaceName);
						
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom 
						if (is_null($tgtAtom) AND $tgtInterface->tgtDataType == "concept") $tgtAtom = key($patch['value']);
						elseif (is_null($tgtAtom)) $tgtAtom = $patch['value'];
						
					}
					
					// perform editUpdate
					if($tgtInterface->editable){
						if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true); // convert true and false into "true" and "false" strings
						
						// in case $tgtAtom is empty string -> perform remove instead of replace.
						if(!$tgtAtom == ''){ 
							$database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
						}else{
							// the final $tgtAtom is not provided, so we have to get this value to perform the editDelete function
							$tgtAtom = JsonPatch::get($before, $patch['path']);
							$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
							 
						}					
					}else{
						Notifications::addError($tgtInterface->name . " is not editable in interface '" . $interface->name . "'");
					}
					
					break;
				case "add" :
					$pathArr = explode('/', $patch['path']);
					
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
					
					// find the right subinterface
					while (count($pathArr)){
						$interfaceName = array_shift($pathArr);
						
						// if path starts with '@' skip
						if(substr($interfaceName, 0, 1) == '@') break 2; // break while and switch
					
						$tgtInterface = ObjectInterface::getSubinterface($tgtInterface, $interfaceName);
					
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom
					
					}
					
					// perform editUpdate
					if($tgtInterface->editable){
						if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true); // convert true and false into "true" and "false" strings
					
						// in case $tgtAtom is null (result of empty array in array_shift) -> provide error.
						if(is_null($tgtAtom)) Notifications::addError($tgtInterface->name . ": add operation without value '");
						
						$database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
						
					}else{
						Notifications::addError($tgtInterface->name . " is not editable in interface '" . $interface->name . "'");
					}
					
					break;
				case "remove" :
					$pathArr = explode('/', $patch['path']);
					
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					// remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					if(current($pathArr) == false) array_shift($pathArr); // was empty(current($pathArr)), but prior to PHP 5.5, empty() only supports variables, not expressions.
					
					// find the right subinterface
					while (count($pathArr)){
						$interfaceName = array_shift($pathArr);
						
						// if path starts with '@' skip
						if(substr($interfaceName, 0, 1) == '@') break 2; // break while and switch
						
						$tgtInterface = ObjectInterface::getSubinterface($tgtInterface, $interfaceName);
						
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom 

					}
					
					// perform editDelete
					if($tgtInterface->editable){
						// in case of 'remove' for a link to a non-concept (i.e. datatype), the final $tgtAtom is not provided, so we have to get this value to perform the editDelete function
						if(is_null($tgtAtom)) $tgtAtom = JsonPatch::get($before, $patch['path']);
						$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept);
					}else{
						Notifications::addError($tgtInterface->name . " is not editable in interface '" . $interface->name . "'");
					}
					
					break;
			}
		}
		
		// Close transaction => ROLLBACK or COMMIT.
		$database->closeTransaction('Updated', false); 
		
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
	
	private function getLabel(){
		$database = Database::singleton();
		$view = Concept::getView($this->concept);
		
		if(empty($view) || $this->id == ''){ // no view? label = id
			return $this->id;
		
		}else{
			$viewStrs = array ();
			
			foreach ($view['segments'] as $viewSegment){
				
				if ($viewSegment['segmentType'] == 'Text'){ 
					$viewStrs[] = htmlSpecialChars($viewSegment['Text']);
				
				}elseif($viewSegment['segmentType'] == 'Html'){
					$viewStrs[] = $viewSegment['Html'];
				
				}else{
					$query = "SELECT DISTINCT `tgt` FROM (".$viewSegment['expSQL'].") AS results WHERE src='".addslashes($this->id)."' AND `tgt` IS NOT NULL";
					$tgtAtoms = array_column($database->Exe($query), 'tgt');
					
					$txt = count($tgtAtoms) ? $tgtAtoms[0] : $this->id; // this can happen in a create-new interface when the view fields have not yet beenfilled out, while the atom is shown
					$viewStrs[] = htmlSpecialChars($txt);
				}
			}
			return implode($viewStrs);
		}
	}
}

?>