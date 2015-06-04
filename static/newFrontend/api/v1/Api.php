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
	public function installer($sessionId, $roleId = null){
		try{			
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			
			$db = Database::singleton();
			$db->resetDatabase();
			
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

	/**************************** NOTIFICATIONS ****************************/	
	/**
	 * @url GET notifications/all
	 * @param int $roleId
	 */
	public function getAllNotifications($roleId = null){
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
	
	/**
	 * @url GET extensions/all
	 */
	public function getAllExtensions(){
		try{
			return (array) $GLOBALS['apps'];
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}

	/**************************** OBJECTINTERFACES ****************************/
	/**
	 * @url GET resource/{concept}/{srcAtomId}/{interfaceId}
	 * @url GET resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param string $sessionId
	 * @param int $roleId
	 */
	public function getAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId = null, $sessionId = null, $roleId = null){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
		
			$result = array();
			
			if($session->interface->srcConcept != $concept) throw new Exception("Concept '$concept' cannot be used as source concept for interface '".$session->interface->label."'", 400);
			
			$atom = new Atom($srcAtomId, $concept);
			if(!$atom->atomExists()) throw new Exception("Resource '$srcAtomId' not found", 404);
			
			$result = (array)$atom->getContent($session->interface, true, $tgtAtomId);
			
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
	public function patchAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $sessionId = null, $roleId = null, $requestType = 'feedback', $request_data = null){
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
	public function putAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $sessionId = null, $roleId = null, $requestType = 'feedback', $request_data = null){
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
	public function deleteAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $sessionId = null, $roleId = null, $requestType = 'feedback'){
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
	public function postAtom($concept, $srcAtomId, $interfaceId, $sessionId = null, $roleId = null, $requestType = 'feedback', $request_data = null){
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
    
    
    /**************************** ATOMS ********************************/
	
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
    
	/**
     * @url POST resource/{concept}
	 * @status 201
     */
	function postConceptAtom($concept){ 
		try{
			$database = Database::singleton();
			return $database->addAtomToConcept(Concept::createNewAtom($concept), $concept); // insert new atom in database
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}

	/**
	 * @url DELETE resource/{concept}/{atomId}
	 * @status 204
	 */
	function deleteConceptAtom($concept, $atomId){ 
		try{
			$database = Database::singleton();
			$atom = new Atom($atomId, $concept);
			if(!$atom->atomExists()) throw new Exception("Resource '$atomId' not found", 404);
			$atom->delete();
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
    	
	/**************************** ROLES ****************************/
	/**
     * @url GET roles
     */
    public function getAllRoles(){
    	try{
			return Role::getAllRoles(); // "Return list of all roles with properties as defined in class Role"
		
    	}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
    }
    
    /**
     * @url GET role
     * @url GET role/{roleId}
     */
    public function getRole($roleId = NULL){
    	try{
    		if($roleId !== NULL){	// do not use isset(), because roleNr can be 0.
    			return new Role($roleId); // Return role with properties as defined in class Role
    		}else{
    			return new Role(); // Return default role	
    		}
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
   		}
    }
    
    /**
     * @url GET role/name/{roleName}
     */
    public function getRoleByName($roleName){
    	try{
    		return Role::getRole($roleName);
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
   		}
    }
    
    
	/**************************** INTERFACES ****************************/
    /**
     * @url GET interfaces/all
     * @param int $roleId
     */
    public function getAllInterfaces($roleId = null){
    	try{
    		$session = Session::singleton();
    		$session->setRole($roleId);
    		
    		return array ('top' => $session->role->getInterfacesForNavBar()
    					 ,'new' => $session->role->getInterfacesToCreateAtom());
    		
    	}catch(Exception $e){
    		throw new RestException(404, $e->getMessage());
    	}
    }
    
	/**
     * @url GET interfaces
	 * @url GET interface/{interfaceId}
	 * @param string $interfaceId
	 * @param int $roleId
     */
    public function getInterfaces($interfaceId = null, $roleId = null){
    	try{
    		$session = Session::singleton();
    		$session->setRole($roleId);
    	
	    	if(!is_null($interfaceId)){
		    	$session->setInterface($interfaceId);
	    		
		    	return $session->interface->getInterface(); // Return specific interface
	    		
	        }else{
	        	
	        	return $session->role->getInterfaces();  // Return list of all interfaces for the given/default role
			}
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
    }
}
?>