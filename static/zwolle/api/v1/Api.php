<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;
class Api{	
	
	/****************************** INSTALLER & SESSION RESET ******************************/
	/**
	 * @url GET installer
	 * @param string $sessionId
	 * @param int $roleId
	 */
	public function installer($sessionId, $roleId = 0){
		try{			
			Database::createDB();
			
			$db = Database::singleton();
			$db->reinstallDB();
			
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			
			return Notifications::getAll(); // Return all notifications
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url GET session
	 */
	public function getSession(){
		try{
			$session = Session::singleton();
			
			return array('id' => session_id());
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	
	}
	
	/**
	 * @url DELETE session/{session_id}
	 */
	public function destroySession($session_id){
		try{
			$session = Session::singleton($session_id);
			$session->destroySession($session_id);
		
			return array('notifications' => Notifications::getAll());
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**************************** FILE ****************************/
	/**
	 * @url POST file
	 * @param string $sessionId
	 * @param int $roleId
	 */
	public function fileUpload($sessionId, $roleId = 0){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			
			// TODO: Check if upload is allowed in interface
			
			if (is_uploaded_file($_FILES['file']['tmp_name'])){
				$tmp_name = $_FILES['file']['tmp_name'];
				$new_name = $_FILES['file']['name'];
				$target = UPLOAD_DIR . $new_name;
				$result = move_uploaded_file($tmp_name, $target);
				
				if($result) Notifications::addSuccess("File '".$new_name."' uploaded");
				else Notifications::addError("Error in file upload");
			}else{
			    Notifications::addError('No file uploaded');
			}
			
			$result = array('notifications' => Notifications::getAll(), 'files' => $_FILES, 'filename' => $new_name);
			return $result;
	
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}

	/**************************** INTERFACES ****************************/
	/**
	 * @url GET resource/{concept}/{srcAtomId}/{interfaceId}
	 * @url GET resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param string $sessionId
	 * @param int $roleId
	 * @param boolean $inclLinktoData
	 * @param string $arrayType
	 * @param boolean $metaData
	 */
	public function getAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId = null, $sessionId = null, $roleId = 0, $inclLinktoData = false, $arrayType = "assoc", $metaData = true){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
		
			$result = array();
			
			if($session->interface->srcConcept != $concept) throw new Exception("Concept '$concept' cannot be used as source concept for interface '".$session->interface->label."'", 400);
			
			$atom = new Atom($srcAtomId, $concept);
			if(!$atom->atomExists()) throw new Exception("Resource '$srcAtomId' not found", 404);
			
			$result = (array)$atom->getContent($session->interface, true, $tgtAtomId, $inclLinktoData, $arrayType, $metaData);
			
			if(empty($result)) Notifications::addInfo("No results found");			
	
			if(is_null($tgtAtomId)){
				// return array of atoms (i.e. tgtAtoms of the interface given srcAtomId)
				return array_values($result); // array_values transforms assoc array to non-assoc array
			}else{
				// return 1 atom (i.e. tgtAtomId)
				return current($result);
			}
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url PATCH resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param string $sessionId
	 * @param int $roleId
	 * @param string $requestType
	 *
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function patchAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $sessionId = null, $roleId = 0, $requestType = 'feedback', $request_data = null){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
			
			// TODO: insert check if Atom may be patched  with this interface
			
			$session->atom = new Atom($tgtAtomId, $session->interface->tgtConcept);
			if(!$session->atom->atomExists()) throw new Exception("Resource '$tgtAtomId' does not exists", 404);
				
			return $session->atom->patch($session->interface, $request_data, $requestType);
	
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url PUT resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param string $sessionId
	 * @param int $roleId
	 * @param string $requestType
	 * 
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function putAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $sessionId = null, $roleId = 0, $requestType = 'feedback', $request_data = null){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);

			// TODO: insert check if Atom may be updated with this interface
			
			if(!$session->database->atomExists($tgtAtomId, $session->interface->tgtConcept)){
				// TODO: insert check if Atom may be created with this interface
				$session->database->addAtomToConcept($tgtAtomId, $session->interface->tgtConcept);
			}
			
			$session->atom = new Atom($tgtAtomId, $session->interface->tgtConcept);
			
			return $session->atom->put($session->interface, $request_data, $requestType);
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url DELETE resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param string $sessionId
	 * @param int $roleId
	 * @param string $requestType
	 * 
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function deleteAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $sessionId = null, $roleId = 0, $requestType = 'feedback'){
		
		throw new RestException(501); // 501: disabled until CRUD rights can be specified in interfaces
		
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
		
			// TODO: insert check if Atom may be deleted with this interface
			
			$session->atom = new Atom($tgtAtomId, $session->interface->tgtConcept);
			if(!$session->atom->atomExists()) throw new Exception("Resource '$tgtAtomId' does not exists", 404);
			
			return $session->atom->delete($requestType);
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url POST resource/{concept}/{srcAtomId}/{interfaceId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $sessionId
	 * @param int $roleId
	 * @param string $requestType
	 * 
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function postAtom($concept, $srcAtomId, $interfaceId, $sessionId = null, $roleId = 0, $requestType = 'feedback', $request_data = null){
		try{
			$session = Session::singleton($sessionId);			
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
			
			// TODO: insert check if Atom may be created with this interface
			
			$concept = $session->interface->tgtConcept;
			$newAtomId = $session->database->addAtomToConcept(Concept::createNewAtom($concept), $concept);
			$session->atom = new Atom($newAtomId, $concept);
			
			if(!$session->atom->atomExists()) throw new Exception("Resource not created", 500);
			
			return $session->atom->post($session->interface, $request_data, $requestType);
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	

	/**************************** CONCEPTS ****************************/
    /**
     * @url GET concept
     * @url GET concept/{concept}
     */
    public function getConcepts($concept = null){
    	try{
    		if(isset($concept)){
        		// return Concept::getConcept(); // Return specific concept
        		throw new RestException(501); // 501: not implemented
    		}else{
    			return Concept::getAllConcepts(); // Return list of all concepts
    		}
        	
        }catch(Exception $e){
       		throw new RestException($e->getCode(), $e->getMessage());
       	}
    }
	
	/**
     * @url GET resource/{concept}
     */
    public function getConceptAtoms($concept){
    	try{
        	return Concept::getAllAtomObjects($concept); // "Return list of all atoms for $concept"
        	
        }catch(Exception $e){
        	throw new RestException($e->getCode(), $e->getMessage());
        }
    }
    
    /**
     * @url GET resource/{concept}/{atomId}
     */
    public function getConceptAtom($concept, $atomId){
    	try{
    		$atom = new Atom($atomId, $concept);
    		if(!$atom->atomExists()) throw new Exception("Resource '$atomId' not found", 404);
    		return $atom->getAtom();
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
   		}
    }
    	
	/**************************** UI stuff ****************************/
    
    /**
     * @url GET navBar
     * @param string $sessionId
     * @param int $roleId
     */
    public function getNavBar($sessionId = null, $roleId = 0){
    	try{
    		$session = Session::singleton($sessionId);
    		$session->setRole($roleId);
    		
    		// top level interfaces
    		$top = array();
    		foreach ($session->role->getInterfacesForNavBar() as $ifc){
    			$top[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
    		}
    		
    		// new interfaces
    		$new = array();
    		foreach ($session->role->getInterfacesToCreateAtom() as $ifc){
    			$new[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
    		}
    		
    		// roles
    		$roles = array();
    		$allRoles = LOGIN_ENABLED ? Role::getAllSessionRoles($sessionId) : Role::getAllRoleObjects();
    		foreach((array)$allRoles as $role){
    			$roles[] = array('id' => $role->id, 'label' => $role->label);
    		}
    		
    		return array ('top' => $top
    					 ,'new' => $new
    					 ,'refreshMenu' => $GLOBALS['navBar']['refreshMenu']
    					 ,'appMenu' => $GLOBALS['navBar']['appMenu']
    					 ,'roleMenu' => $GLOBALS['navBar']['roleMenu']
    					 ,'roles' => $roles
    					 ,'notifications' => Notifications::getAll()
    		);
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
    	}
    }
    
    /**
     * @url GET notifications/all
     * @param int $roleId
     */
    public function getAllNotifications($roleId = 0){
    	try{
    		$session = Session::singleton();
    		$session->setRole($roleId);
    			
    		foreach ((array)$GLOBALS['hooks']['before_API_getAllNotifications_getViolations'] as $hook) call_user_func($hook);
    
    		$session->role->getViolations();
    		// RuleEngine::checkProcessRules($session->role->id);  // Leave here to measure performance difference
    
    		return Notifications::getAll();
    			
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
    	}
    }
}
?>