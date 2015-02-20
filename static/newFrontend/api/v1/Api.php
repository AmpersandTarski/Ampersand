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
			include (__DIR__ . '/../../generics/InstallerDBstruct.php');
			include (__DIR__ . '/../../generics/InstallerDefPop.php');
			
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			
			$db = Database::singleton();
			
			$db->closeTransaction('Database reset to initial value', true);	
			
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
	 * @url GET interface/{interfaceId}
	 * @url GET interface/{interfaceId}/{atomId}
	 * @param string $interfaceId
	 * @param string $sessionId
	 * @param string $atomId
	 * @param int $roleId
	 */
	public function getAtom($interfaceId, $sessionId = null, $atomId = null, $roleId = null){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
		
			$result = array();
			
			if(is_null($atomId)) {
				foreach(Concept::getAllAtomIds($session->interface->srcConcept) as $atomId){
					$atom = new Atom($atomId, $session->interface->srcConcept);
					$result = array_merge($result, (array)$atom->getContent($session->interface));
				}
			}else{
		
				$atom = new Atom($atomId, $session->interface-srcConcept);
				$result = $atom->getContent($session->interface);
			}
			
			if(empty($result)) throw new Exception('Resource not found', 404);
	
			return array_values($result); // array_values transforms assoc array to non-assoc array
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url PATCH interface/{interfaceId}/{atomId}
	 * @param string $interfaceId
	 * @param string $sessionId
	 * @param string $atomId
	 * @param int $roleId
	 */
	public function patchAtom($interfaceId, $sessionId, $atomId, $roleId = null, $request_data = null){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
			
			$atom = new Atom($atomId, $session->interface->tgtConcept);	
			
			return array_merge(array('patches' => $atom->patch($session->interface, $request_data))
							  ,array('content' => current((array)$atom->getContent($session->interface, true, $atom->id))) // current(), returns first item of array. This is valid, because patchAtom() concerns exactly 1 atom.
							  ,array('notifications' => Notifications::getAll())
							  );
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url DELETE interface/{interfaceId}/{atomId}
	 * @param string $interfaceId
	 * @param string $sessionId
	 * @param string $atomId
	 * @param int $roleId
	 */
	public function deleteAtom($interfaceId, $sessionId, $atomId, $roleId = null){
		try{
			$session = Session::singleton($sessionId);
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
		
			// TODO: insert check if Atom may be deleted with this interface
			
			$atom = new Atom($atomId, $session->interface->tgtConcept);
			$atom->delete();
			
			return array('notifications' => Notifications::getAll());
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url POST interface/{interfaceId}
	 * @param string $interfaceId
	 * @param string $sessionId
	 * @param int $roleId
	 */
	public function postAtom($interfaceId, $sessionId, $roleId = null){
		try{
			$session = Session::singleton($sessionId);
			$db = Database::singleton();
			
			$session->setRole($roleId);
			$session->setInterface($interfaceId);
			
			// TODO: insert check if Atom may be created with this interface
			
			$concept = $session->interface->srcConcept;
			$atomId = $db->addAtomToConcept(Concept::createNewAtom($concept), $concept);
			$atom = new Atom($atomId, $concept);
			
			return array_values($atom->getContent($session->interface)); // array_values transforms assoc array to non-assoc array
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**************************** CONTEXT ****************************/
	
	/**
	 * @url GET context/{interfaceId}
	 */
	public function getInterface($interfaceId){
		throw new RestException(501);
		try{			
			$interface = new ObjectInterface($interfaceId);
			
			return $interface;
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
	 * @url DELETE resource/{concept}/{atom}
	 * @status 204
	 */
	function deleteConceptAtom($concept, $atom){ 
		try{
			$database = Database::singleton();
			$database->deleteAtom($atom, $concept); // delete atom + all relations with other atoms
		
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
    		
    		return array ('top' => $session->role->getInterfaces(true)
    					 ,'new' => $session->role->getInterfaces(false));
    		
    	}catch(Exception $e){
    		throw new RestException(404, $e->getMessage());
    	}
    }
    
	/**
     * @url GET interfaces
	 * @url GET interfaces/{interfaceId}
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