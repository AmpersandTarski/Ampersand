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
	 * @return array (string @id, string @label, array @view, string @type, array @interfaces, string id)
	 */
	public function getAtom(){
		foreach(InterfaceObject::getAllInterfacesForConcept($this->concept) as $ifc){
			$interfaces[] = $this->jsonld_id . '/' . $ifc->id;
		}
		
		return array( '@id' => $this->jsonld_id
					, '@label' => $this->label
		        	, '@view' => $this->view
					, '@type' => $this->jsonld_type
					, '@interfaces' => $interfaces
					, 'id' => $this->id
					);
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
	 * Returns atom content given a certain interface
	 * @param InterfaceObject $interface
	 * @param boolean $rootElement specifies if this Atom is the root element (true), or a subelement (false) in an interface
	 * @param string $tgtAtom specifies that a specific tgtAtom must be used instead of querying the tgtAtoms with the expressionSQL of the interface
	 * @param boolean $inclLinktoData specifies if data from LINKTO (ref) subinterfaces must be included or not.
	 * @param string $arrayType specifies if the arrays in the result are 'assoc' (associative, key index) or 'num' (numeric index).
	 * @param boolean $metaData specifies if meta data about objects must be included or not
	 * @param array $recursionAtomArr contains atomIds when linkto data is included (used to detect recursion and prevent infinite loops)
	 * @param string $path contains JSON path to objects in (sub)interface(s) (needed to determine patch operations in frontend)
	 * @return array
	 */
    public function getContent($interface, $rootElement = true, $tgtAtom = null, $inclLinktoData = false, $arrayType = "assoc", $metaData = true, $recursionAtomArr = array(), $path = null){
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
			if($rootElement) $content['@context'] = Config::get('serverURL') . Config::get('apiPath') . '/interface/' . $interface->id;
			
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
						if($interface->isLinkTo && !$inclLinktoData && $session->isAccessibleIfc($interface->refInterfaceId)) $atomInterfaces[] = array('id' => $interface->refInterfaceId, 'label' => $interface->refInterfaceId);
						else $atomInterfaces = array_map(function($o) { return array('id' => $o->id, 'label' => $o->label); }, $session->getInterfacesToReadConcept($interface->tgtConcept));
							
						// Add meta data elements
						$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
								, '@label' => $tgtAtom->label
								, '@path' => $path .= $rootElement ? '/' : $interface->id . '/' . $tgtAtom->id . '/'
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
					if($interface->isLinkTo && !$inclLinktoData && $session->isAccessibleIfc($interface->refInterfaceId)) $atomInterfaces[] = array('id' => $interface->refInterfaceId, 'label' => $interface->refInterfaceId);
					else $atomInterfaces = array_map(function($o) { return array('id' => $o->id, 'label' => $o->label); }, $session->getInterfacesToReadConcept($interface->tgtConcept));
						
					// Add meta data elements
					$content = array_merge($content, array (  '@id' => $tgtAtom->jsonld_id
							, '@label' => $tgtAtom->label
							, '@path' => $path .= $rootElement ? '/' : $interface->id . '/' . $tgtAtom->id . '/'
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
						$otherAtom = $tgtAtom->getContent($subinterface, false, null, $inclLinktoData, $arrayType, $metaData, null, $path);
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
				if(!empty($interface->refInterfaceId) && (!$interface->isLinkTo || $inclLinktoData) && ($recursionAtomArr[$tgtAtom->id] < 2)){
					if(!$interface->tgtConceptIsObject) throw new Exception("TgtConcept of interface: '" . $interface->label . "' is scalar and can not have a ref interface defined", 501);
					
					if($inclLinktoData) $recursionAtomArr[$tgtAtom->id]++;
					
					$refInterface = new InterfaceObject($interface->refInterfaceId, null);
					foreach($refInterface->subInterfaces as $subinterface){
						$otherAtom = $tgtAtom->getContent($subinterface, false, null, $inclLinktoData, $arrayType, $metaData, $recursionAtomArr, $path);
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
						if($interface->univalent && !$rootElement) $arr = $content;
						else $arr[] = $content;
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
	
	/**********************************************************************************************
	 * 
	 * PUT POST DELETE and PATCH functions 
	 *  
	 **********************************************************************************************/
	
	/**
	 * Put (new) atom properties in database
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @param mixed $requestData contains the data of this atom to put
	 * @param string $requestType specifies the intention of this transaction, i.e.'promise' or 'feedback'
	 * @return array (array patches, array content, array notifications, boolean invariantRulesHold, string requestType)
	 */
	public function put(&$interface, $requestData, $requestType){		
				
		// Get current state of atom
		$before = $this->getContent($interface, true, $this->id);
		$before = current($before); // current(), returns first item of array. This is valid, because put() concerns exactly 1 atom.
		
		// Determine differences between current state ($before) and requested state ($request_data)
		$patches = JsonPatch::diff($before, $requestData);
		
		return $this->patch($interface, $patches, $requestType);
	}
	
	/**
	 * Create atom in database
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @param mixed $requestData contains the data of this atom to post
	 * @param string $requestType specifies the intention of this transaction, i.e.'promise' or 'feedback'
	 * @return array (array patches, array content, array notifications, boolean invariantRulesHold, string requestType)
	 */
	public function post(&$interface, $requestData, $requestType){
		// Get current state of atom
		$before = $this->getContent($interface, true, $this->id);
		$before = current($before); // current(), returns first item of array. This is valid, because put() concerns exactly 1 atom.
		
		// Determine differences between current state ($before) and requested state ($request_data)
		$patches = JsonPatch::diff($before, $requestData);
	
		// Skip remove operations, because it is a POST operation and there are no values in de DB yet
		$patches = array_filter($patches, function($patch){return $patch['op'] <> 'remove';});
	
		// Patch
		$successMessage = $this-> concept . ' created';
		return $this->patch($interface, $patches, $requestType, $successMessage);
	}
	
	/**
	 * Delete atom from database
	 * @param string $requestType specifies the intention of this transaction, i.e.'promise' or 'feedback'
	 * @throws Exception when concept of atom is not known
	 * @return array (array notifications, boolean invariantRulesHold, string requestType)
	 */
	public function delete($requestType){
		if(is_null($this->concept)) throw new Exception('Concept type of atom ' . $this->id . ' not provided', 500);
	
		$databaseCommit = $this->processRequestType($requestType);
	
		$this->database->deleteAtom($this->id, $this->concept);
	
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $this->database->closeTransaction($this->concept . ' deleted', false, $databaseCommit, false);
	
		return array('notifications' 		=> Notifications::getAll()
				,'invariantRulesHold'	=> $invariantRulesHold
				,'requestType'			=> $requestType
		);
	
	}
	
	/**
	 * Processes array of patches (according to specifications: JSON Patch is specified in RFC 6902 from the IETF)
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @param array $patches contains all patches that need to be applied to this atom
	 * @param string $requestType specifies the intention of this transaction, i.e.'promise' or 'feedback'
	 * @param string $successMessage
	 * @throws Exception when patch operation is unknown
	 * @return array (array patches, array content, array notifications, boolean invariantRulesHold, string requestType)
	 */
	public function patch(&$interface, $patches, $requestType, $successMessage = null){
		
		$databaseCommit = $this->processRequestType($requestType);
		
		if(is_null($successMessage)) $successMessage = $this->concept . ' updated';
		
		// Patch
		foreach ((array)$patches as $key => $patch){
			try{
				switch($patch['op']){
					case "replace" :
						$this->doPatchReplace($patch, $interface);
						break;
					case "add" :
						$this->doPatchAdd($patch, $interface);
						break;
					case "remove" :
						$this->doPatchRemove($patch, $interface);
						break;
					default :
						throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
				}
			}catch (Exception $e){
				Notifications::addError($e->getMessage());
			}
		}
		
		// $databaseCommit defines if transaction should be committed or not when all invariant rules hold. Returns if invariant rules hold.
		$invariantRulesHold = $this->database->closeTransaction($successMessage, false, $databaseCommit, true);
		
		return array( 'patches' 			=> $patches
					, 'content' 			=> current((array)$this->newContent) // current(), returns first item of array. This is valid, because patchAtom() concerns exactly 1 atom.
					, 'notifications' 		=> Notifications::getAll()
					, 'invariantRulesHold'	=> $invariantRulesHold
					, 'requestType'			=> $requestType
					);
	}
	
	/** 
	 * Performs editUpdate or editDelete based on patch replace operation
	 * @param array $patch
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @throws Exception
	 * @return void
	 */
	private function doPatchReplace($patch, $interface){
		
		$patchInfo = $this->processPatchPath($patch, $interface);
		$tgtInterface = $patchInfo['ifc'];
		
		/******* Perform edit *********/
		
		// Interface is property
		if ($tgtInterface->isProperty && !$tgtInterface->isIdent){
			// Throw error when patch value is something else then true, false or null 
			if(!(is_bool($patch['value']) || is_null($patch['value']))) throw new Exception("Interface $tgtInterface->label is property, boolean expected, non-boolean provided");
			
			// When true
			if($patch['value']){						
				$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $patchInfo['srcAtom'], $tgtInterface->srcConcept, $patchInfo['srcAtom'], $tgtInterface->tgtConcept);
			// When false or null
			}else{
				$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $patchInfo['srcAtom'], $tgtInterface->srcConcept, $patchInfo['srcAtom'], $tgtInterface->tgtConcept);
			}
			
		// Interface is a relation to an object
		}elseif($tgtInterface->tgtConceptIsObject){
			throw new Exception("Cannot perform patch replace for object reference: {$patch['path']}. Use add or remove instead", 500);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$tgtInterface->tgtConceptIsObject){
			
			// Replace by nothing => editDelete
			if(empty($patch['value'])){
				
				$this->database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $patchInfo['srcAtom'], $tgtInterface->srcConcept, $patch['oldValue'], $tgtInterface->tgtConcept);
			
			// Replace by other atom => editUpdate
			}else{
				$this->database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $patchInfo['srcAtom'], $tgtInterface->srcConcept, $patch['value'], $tgtInterface->tgtConcept, $patch['oldValue']);
			}
		}
	}
	
	/**
	 * Performs editUpdate based on patch add operation
	 * @param array $patch
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @throws Exception
	 * @return void
	 */
	private function doPatchAdd($patch, $interface){

		// Report error when no patch value is provided.
		if(is_null($patch['value'])) throw new Exception("Patch operation add provided without value: '{$patch['path']}'", 500);
		
		$patchInfo = $this->processPatchPath($patch, $interface);
		
		/******* Perform edit *********
		 * Properties are treated as a 'replace', so not handled here
		 * UNI interface expressions to scalar are also a 'replace' and not handled here
		 * 
		 * If interface is an expression to an object -> perform editUpdate
		 * If interface is an non-uni expression to a scalar -> perform editUpdate
		 */
		$this->database->editUpdate($patchInfo['ifc']->relation, $patchInfo['ifc']->relationIsFlipped, $patchInfo['srcAtom'], $patchInfo['ifc']->srcConcept, $patch['value'], $patchInfo['ifc']->tgtConcept);
		
	}
	
	/**
	 * Performs editDelete based on patch remove operation
	 * @param array $patch
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @return void
	 */
	private function doPatchRemove($patch, $interface){
		
		$patchInfo = $this->processPatchPath($patch, $interface);
		
		/******* Perform edit *********
		 * Properties are treated as a 'replace', so not handled here
		 * UNI interface expressions to scalar are also a 'replace' and not handled here
		 * 
		 * If interface is an expression to an object -> perform editDelete
		 * If interface is an non-uni expression to a scalar -> perform editDelete
		 */
		$this->database->editDelete($patchInfo['ifc']->relation, $patchInfo['ifc']->relationIsFlipped, $patchInfo['srcAtom'], $patchInfo['ifc']->srcConcept, $patchInfo['tgtAtom'], $patchInfo['ifc']->tgtConcept);
	}
	
	/**********************************************************************************************
	 *
	 * Helper functions
	 *
	 **********************************************************************************************/
	
	/**
	 * Finds the subinterface, srcAtom, tgtAtom combination given a patch path
	 * @param array $patch
	 * @param InterfaceObject $interface specifies the interface of this transaction
	 * @throws Exception when interface path does not exists or is not editable
	 * @return array (InterfaceObject ifc, string srcAtom, string tgtAtom)
	 */
	private function processPatchPath($patch, $interface){
		
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
		if($tgtInterface === false) throw new Exception("Interface path does not exists: {$patch['path']}", 500);
		if(!$tgtInterface->editable) throw new Exception($tgtInterface->label . " is not editable in interface '" . $interface->label . "'", 403);
		
		return array('ifc' => $tgtInterface, 'srcAtom' => $srcAtom, 'tgtAtom' => $tgtAtom);
		
	}
	
	/**
	 * Checks request type and returns boolean to determine database commit
	 * @param string $requestType
	 * @throws Exception when unknown request type specified (allowed: 'feedback' and 'promise')
	 * @return boolean (true for 'promise', false for 'feedback')
	 */
	private function processRequestType($requestType){
		switch($requestType){
			case 'feedback' : return false;
			case 'promise' : return true;
			default : throw new Exception("Unkown request type '$requestType'. Supported are: 'feedback', 'promise'", 500);
		}
	}
	
	/**
	 * Coversion of php variables to json according to Ampersand technical types (TTypes)
	 * @param mixed $value
	 * @param string $concept
	 * @return mixed
	 */
	public function typeConversion($value, $concept){
		switch(Concept::getTypeRepresentation($concept)){
			case "DATE" :
				$datetime = new DateTime($value);
				return $datetime->format('Y-m-d'); // format in ISO-8601 standard
			case "DATETIME" :
				$datetime = new DateTime($value, new DateTimeZone('UTC')); // datetimes are stored in UTC in database
				$datetime->setTimezone(new DateTimeZone(date_default_timezone_get())); // convert back to systemtime
				return $datetime->format(DateTime::ATOM); // format in ISO-8601 standard, i.e. 2005-08-15T15:52:01+00:00 (DateTime::ATOM)
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