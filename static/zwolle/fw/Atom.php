<?php

Class Atom {
    /**
     * Dependency injection of a database connection class
     * @var Database
     */
	private $database;
	
	/**
	 * Ampersand identifier of the atom
	 * @var string
	 */
	public $id;
	
	/**
	 * Escaped identifier for use in database queries
	 * @var string 
	 */
	public $idEsc;
	
	/**
	 * Specifies path (interface + atom) from which this atom is instantiated
	 * @var string
	 */
	public $path;
	
	/**
	 *Specifies the interface from which this atom is instantiated
	 * @var InterfaceObject
	 */
	private $parentIfc;
	
	/**
	 * Label of atom to be displayed in user interfaces
	 * @var string
	 */
	public $label;
	
	/**
	 * Array of attributes of this atom to be used by the user interface frontend templates
	 * @var array
	 */
	public $view;
	
	/**
	 * Specifies the concept of which this atom is an instance
	 * @var string
	 */
	public $concept;
	
	/**
	 * Specifies interface id to be used to get/set stored content if no $parentIfc is applicable (i.e. atom is src of toplevel interface)
	 * @var string
	 */
	public $topLevelIfcId = null;
	
	/**
	 * Variable to temporarily store changed atom content
	 * @var mixed
	 */
	private $storedContent = null;
	
	private $jsonld_id;
	private $jsonld_type;
	
	/**
	 * Atom constructor
	 * @param string $id
	 * @param string $concept
	 * @param string $viewId
	 * @param InterfaceObject $ifc
	 * @return void
	 */
	public function __construct($id, $concept, $viewId = null, $ifc = null){
		$this->database = Database::singleton();
		$this->parentIfc = $ifc;
		$this->concept = $concept;
		
		$this->setId($id);
		
		// View & label
		if(is_null($viewId)) $viewId = Concept::getDefaultViewId($this->concept); // If default viewId is specified, use it
		if(is_null($viewId)){
		    $this->view = null;
		    $this->label = $this->id;
		}else{
		    $view = new View($viewId, $this->database);
		    $this->view = $view->getView($this);
		    
		    $viewString = implode($this->view);
		    $this->label = empty($viewString) ? $this->id : $viewString; // empty viewString => label = id
		}
		
		// JSON-LD attributes
		$this->jsonld_id = Config::get('serverURL') . Config::get('apiPath') . '/resource/' . $concept . '/' . $this->id;
		$this->jsonld_type = Config::get('serverURL') . Config::get('apiPath') . '/concept/' . $concept;

	}
	
	/**
	 * Set identifier of atom
	 * @param string $id
	 */
	public function setId($id){
	    $this->id = $id;
		$this->idEsc = $this->database->escape($this->database->typeConversion($this));
		
		if(is_null($this->parentIfc)){
		  $this->path = 'resources/' . $this->concept . '/' . $this->id;
		}else{
		  $this->path = $this->parentIfc->path . '/' . $this->id;
		}
	}
	
	public function ifc($ifcId){
	    $subIfcs = is_null($this->parentIfc) ? null : $this->parentIfc->boxSubInterfaces;
	    $ifc = new InterfaceObject($ifcId, $subIfcs, $this);
	    
	    // Check if interface can be used with this atom as source
	    if($this->concept != $ifc->srcConcept) throw new Exception ("Source concept of atom '{$this->id}[{$this->concept}]' does not match source concept [{$ifc->srcConcept}] of interface '{$ifc->path}'", 500);
	    
	    return $ifc;
	}
	
	/**
	 * Checks if atom exists in database
	 * @return boolean
	 */
	public function atomExists(){
		if($this->id === '_NEW_') return true; // Return true if id is '_NEW_' (special case)
		
		return $this->database->atomExists($this);
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

/**************************************************************************************************
 *
 * Functions to get content of atom using interfaces
 *
 *************************************************************************************************/
	
	/**
	 * Store content of atom at a certain point (e.g. before database commit/rollback)
	 * @return Atom
	 */
	public function setStoredContent(){
	    // If parentIfc is null (toplevel) switch to topLevelIfc to be able to return/store the new content
	    $this->storedContent = is_null($this->parentIfc) ? $this->ifc($this->topLevelIfcId)->getContent() : $this->getContent();
	    return $this;
	}
	
	public function getStoredContent(){
	    return $this->storedContent;
	}
	
	public function getContent($options = array(), $recursionArr = array()){
	    // CRUD check
	    if(!$this->parentIfc->crudR) throw new Exception("Read not allowed for '{$this->path}'", 405);
	    
	    $session = Session::singleton();
	    
	    // Default options
	    $options['arrayType'] = isset($options['arrayType']) ? $options['arrayType'] : 'num';
	    $options['metaData'] = isset($options['metaData']) ? filter_var($options['metaData'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['navIfc'] = isset($options['navIfc']) ? filter_var($options['navIfc'], FILTER_VALIDATE_BOOLEAN) : true;
	    $options['inclLinktoData'] = isset($options['inclLinktoData']) ? filter_var($options['inclLinktoData'], FILTER_VALIDATE_BOOLEAN) : false;
	    
	    $content = array( '_id_' => $this->id
	                    , '_label_' => $this->label
	                    , '_view_' => $this->view
	                    );
	    
	    if($options['jsonld']){
	        $content['@id'] = $this->jsonld_id;
	        $content['@type'] = $this->jsonld_type;
	    }
	     
	    // Meta data
	    if($options['metaData']){
	        $content['_path_'] = $this->path;
	    }
	    
	    // Define interface(s) to navigate to for this tgtAtom
	    if($options['navIfc']){
	        $ifcs = array();
	        if($this->parentIfc->isLinkTo && $session->isAccessibleIfc($this->parentIfc->refInterfaceId))
	            $ifcs[] = array('id' => $this->parentIfc->refInterfaceId, 'label' => $this->parentIfc->refInterfaceId, 'url' => $this->jsonld_id . '/' . $this->parentIfc->refInterfaceId);
	        else $ifcs = array_map(function($o) {
	            return array('id' => $o->id, 'label' => $o->label, 'url' => $this->jsonld_id . '/' . $o->id);
	        }, $session->getInterfacesToReadConcept($this->concept));
	        $content['_ifcs_'] = $ifcs;
	    }
	    
	    
	    // Subinterfaces
	    foreach($this->parentIfc->subInterfaces as $subinterface){
	        // Skip subinterface if not given read rights
	        if(!$subinterface->crudR) continue;
	         
	        $subcontent = $this->ifc($subinterface->id)->getContent($options, $recursionArr);
	        
	        $content[$subinterface->id] = $subcontent;
	    
	        // _sortValues_ (if subInterface is uni)
	        if($subinterface->univalent && $options['metaData']){
	            if(is_bool($subcontent)) $sortValue = $subcontent; // property
	            elseif($subinterface->tgtConceptIsObject) $sortValue = current((array)$subcontent)['_label_']; // use label to sort objects
	            else $sortValue = $subcontent; // scalar
	    
	            $content['_sortValues_'][$subinterface->id] = $sortValue;
	        }
	    }
	    
	    // Include content for subinterfaces that refer to other interface (e.g. "label" : expr [LINKTO] INTERFACE <refInterface>)
	    if(!empty($this->parentIfc->refInterfaceId)
	            && (!$this->parentIfc->isLinkTo || $options['inclLinktoData'])  // Include content is interface is not LINKTO or inclLinktoData is explicitly requested via the options
	            && (!in_array($this->id, (array)$recursionArr[$this->parentIfc->refInterfaceId]))) // Prevent infinite loops
	    {
	        // Add target atom to $recursionArr to prevent infinite loops
	        if($options['inclLinktoData']) $recursionArr[$this->parentIfc->refInterfaceId][] = $this->id;
	    
	        $refInterface = new InterfaceObject($this->refInterfaceId, null);
	         
	        foreach($refInterface->subInterfaces as $subinterface){
	            // Skip subinterface if not given read rights
	            if(!$subinterface->crudR) continue;
	    
	            $subcontent = $this->ifc($subinterface->id)->getContent($options, $recursionArr);
	            $content[$subinterface->id] = $subcontent;
	             
	            // _sortValues_ (if subInterface is uni)
	            if($subinterface->univalent && $options['metaData']){
	                if(is_bool($subcontent)) $sortValue = $subcontent; // property
	                elseif($subinterface->tgtConceptIsObject) $sortValue = current((array)$subcontent)['_label_']; // use label to sort objects
	                else $sortValue = $subcontent; // scalar
	    
	                $content['_sortValues_'][$subinterface->id] = $sortValue;
	            }
	        }
	    }
	    
	    return $content;
			
	}
	
/**************************************************************************************************
 * 
 * CREATE, UPDATE, PATCH and DELETE functions 
 *  
 *************************************************************************************************/
	
    /**
     * Function not implemented. Use InterfaceObject->create() method instead.
     * @throws Exception
     */
	public function create(){
	    throw new Exception ("Cannot create atom at path '{$this->path}'. Add interface identifier behind path", 405);
	}
	
	/**
	 * Update atom properties in database. Function not (yet) implemented.
	 * @param mixed $data contains the data of this atom to put
	 * @param array $options
	 * @return array
	 */
	public function update($data, $options){
		throw new Exception ("Not yet implemented", 501);
	   
		/* Old code, needs refactoring 
		 
		// Get current state of atom
		$before = $this->getContent($this->id, $options);
	
		// Determine differences between current state ($before) and requested state ($data)
		$patches = mikemccabe\JsonPatch\JsonPatch::diff($before, $data);
	
		return $this->doPatches($patches);
		
		*/
	}
	

	public function patch($patches, $options = array()){
	    // CRUD check for patch is performed by Atom->doPatches() method
	        
		// Handle options
		if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
		$successMessage = isset($options['successMessage']) ? $options['successMessage'] : $this->concept . ' updated';
		
		// Perform patches
		$this->doPatches($patches);
		
		// Close transaction
		$this->database->closeTransaction($successMessage, false, null, $this);
		
		return $this->getStoredContent();
	}
	
	
	public function delete($options = array()){
	    // CRUD check
	    if(!$this->parentIfc->crudD) throw new Exception("Delete not allowed for '{$this->path}'", 405);
	    if(!$this->parentIfc->tgtConceptIsObject) throw new Exception ("Cannot delete non-object [{$this->concept}] in '{$this->path}'. Use PATCH remove operation instead", 405);
	     
	    // Handle options
	    if(isset($options['requestType'])) $this->database->setRequestType($options['requestType']);
	
	    // Perform delete
	    $this->database->deleteAtom($this);
	
	    // Close transaction
	    $this->database->closeTransaction($this->concept . ' deleted', false, null);
	
	    return;
	}
	
/**************************************************************************************************
 *
 * Private functions to perform patches
 *
 *************************************************************************************************/
	
	/**
	 * Performs given patches on atom
	 * @param array $patches
	 * @throws Exception
	 * @return void
	 */
	public function doPatches($patches = array()){	    
		$errorCount = 0;
		foreach ((array)$patches as $key => $patch){
			try{
				// Check patch
				if(!array_key_exists('op', $patch)) throw new Exception ("No 'op' (i.e. operation) specfied for patch #{$key}", 400);
				if(!array_key_exists('path', $patch)) throw new Exception ("No 'path' specfied for patch #{$key}", 400);
				
				$atomOrIfc = $this->walkIfcPath($patch['path']);
		
				switch($patch['op']){
					case "replace" :
						$atomOrIfc->doPatchReplace($patch);
						break;
					case "add" :
						$atomOrIfc->doPatchAdd($patch);
						break;
					case "remove" :
						$atomOrIfc->doPatchRemove();
						break;
					default :
						throw new Exception("Unknown patch operation '" . $patch['op'] ."'. Supported are: 'replace', 'add' and 'remove'", 501);
				}
			}catch (Exception $e){
				Notifications::addErrorException($e);
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
     * Function not implemented. Use InterfaceObject->doPatchReplace() method instead.
     * @throws Exception
     */
	public function doPatchReplace(){
	    throw new Exception ("Cannot patch replace from '{$this->path}'. Path ends with resource", 405);
	}
	
    /**
     * Function not implemented. Use InterfaceObject->doPatchAdd() method instead.
     * @throws Exception
     */
	public function doPatchAdd(){
	    throw new Exception ("Cannot patch add from '{$this->path}'. Path ends with resource", 405);
	    
	}
	

	public function doPatchRemove(){
	    $ifc = $this->parentIfc;
	   
	    // CRUD check
	    if(!$ifc->crudU) throw new Exception("Update is not allowed for path '{$this->path}'", 403);
	    
		// Interface is property
		if($ifc->isProperty && !$ifc->isIdent){
			// Properties must be treated as a 'replace', so not handled here
			throw new Exception("Cannot patch remove for property '{$ifc->path}'. Use patch replace instead", 500);
		
		// Interface is a relation to an object
		}elseif($ifc->tgtConceptIsObject){
			
			$this->database->editDelete($ifc->relation, $ifc->relationIsFlipped, $this->parentIfc->srcAtom, $this);
		
		// Interface is a relation to a scalar (i.e. not an object)
		}elseif(!$ifc->tgtConceptIsObject){
			if($ifc->univalent) throw new Exception("Cannot patch remove for univalent interface {$ifc->path}. Use patch replace instead", 500);
			
			$this->database->editDelete($ifc->relation, $ifc->relationIsFlipped, $this->parentIfc->srcAtom, $this);
			
		}else{
			throw new Exception ("Unknown patch add. Please contact the application administrator", 500);
		}
		
	}
	
/**************************************************************************************************
 *
 * Helper functions
 *
 *************************************************************************************************/
		
	public function walkIfcPath($path){
	    $session = Session::singleton();
	
	    if(!$this->atomExists()) throw new Exception ("Resource '{$this->id}[{$this->concept}]' not found", 404);
	    
	    $atom = $this; // starting point
	    
	    $path = trim($path, '/'); // remove root slash (e.g. '/Projects/xyz/..') and trailing slash (e.g. '../Projects/xyz/')
	    if($path == '') return $this; // if no path is specified, return $this (atom)
	    
	    $pathArr = explode('/', $path);
	    while (count($pathArr)){
	        // Ifc
	        $interfaceId = array_shift($pathArr); // returns the shifted value, or NULL if array is empty or is not an array.
	        $ifc = $atom->ifc($interfaceId);
	        	
	        // Checks
	        if($ifc->isTopLevelIfc && !$session->isAccessibleIfc($ifc->id)) throw new Exception("Interface is not accessible for active roles or accessible roles (login)", 401); // 401: Unauthorized
	        if((!$ifc->crudR) && (count($pathArr) > 1)) throw new Exception ("Read not allowed for interface path '{$ifc->path}'", 405); // crudR required to walk the path further when this is not the last ifc part in the path (count > 1).
	        	
	        // Atom
	        $atomId = array_shift($pathArr); // returns the shifted value, or NULL if array is empty or is not an array.
	        $atom = is_null($atomId) ? null : $ifc->atom($atomId);
	    }
	    
	    return is_null($atom) ? $ifc : $atom;
	}
	
	/**
	 * Coversion of php variables to json according to Ampersand technical types (TTypes)
	 * @param mixed $value
	 * @param string $concept
	 * @return mixed
	 */
	public static function typeConversion($value, $concept){
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