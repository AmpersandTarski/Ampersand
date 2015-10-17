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
			if(Config::get('productionEnv')) throw new Exception ("Database reinstall not allowed in production environment", 403);
			
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
	 * @url DELETE session/{session_id}
	 */
	public function destroySession($session_id){
		try{
			if($session_id != session_id()) throw new Exception ("You can only destroy your own session", 403);
			$session = Session::singleton();
			$session->destroySession();
		
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
				$new_name = time() . '_' . $_FILES['file']['name'];
				$target = UPLOAD_DIR . $new_name;
				$result = move_uploaded_file($tmp_name, $target);
				
				if($result) Notifications::addSuccess("File '".$new_name."' uploaded");
				else Notifications::addError("Error in file upload");
			}else{
			    Notifications::addError('No file uploaded');
			}
			
			$newAtom = $session->database->addAtomToConcept(Concept::createNewAtom('Upload'), 'Upload');
			$session->database->editUpdate('fileName', false, $newAtom, 'Upload', $new_name, 'FileName');
			$session->database->editUpdate('originalFileName', false, $newAtom, 'Upload', $_FILES['file']['name'], 'FileName');
			$session->database->commitTransaction();
			
			$result = array('notifications' => Notifications::getAll(), 'files' => $_FILES, 'uploadId' => $newAtom);
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
			
			if(!$session->interface->crudR) throw new Exception("GET is not allowed for interface " . $session->interface->label, 405);
			
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
			
			if(!$session->interface->crudU) throw new Exception("PATCH is not allowed for interface " . $session->interface->label, 405);
			
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

			if(!$session->interface->crudU) throw new Exception("PUT is not allowed for interface " . $session->interface->label, 405);
			
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
		
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
		
			if(!$session->interface->crudD) throw new Exception("DELETE is not allowed for interface " . $session->interface->label, 405);
			
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
			
			if(!$session->interface->crudC) throw new Exception("POST is not allowed for interface " . $session->interface->label, 405);
			
			$concept = $session->interface->tgtConcept;
			$atomId = $request_data['id'] ? $request_data['id'] : Concept::createNewAtom($concept);
			
			if($session->database->atomExists($atomId, $concept)) throw new Exception ("Resource already exists. POST method is not allowed", 405);
			
			$newAtomId = $session->database->addAtomToConcept($atomId, $concept);
			$session->atom = new Atom($newAtomId, $concept);
			
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
    		foreach ($session->role->getInterfacesForNavBar() as $ifc){
    			$top[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
    		}
    		
    		// new interfaces
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
    					 ,'defaultSettings' => array ('notifications' => Notifications::getDefaultSettings()) 
    					 ,'notifications' => Notifications::getAll()
    					 ,'session' => array ( 'id' => $session->id
    					 					 , 'loggedIn' => Session::sessionUserLoggedIn()
    					 					 , 'sessionRoles' => $roles
    					 					 )
    					 , 'sessionVars' => Session::getSessionVars()
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