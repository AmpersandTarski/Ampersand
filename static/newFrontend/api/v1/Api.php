<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;
class Api
{
/****************************** INSTALLER & SESSION RESET ******************************/
	/**
	 * @url GET installer
	 * @param int $roleId
	 */
	public function installer($roleId){
		include (__DIR__ . '/../../generics/InstallerDBstruct.php');
		include (__DIR__ . '/../../generics/InstallerDefPop.php');
		
		$db = Database::singleton();
		
		$db->closeTransaction('Database reset to initial value', true);	
		
		return Notifications::getAll(); // Return all notifications
		
	}
	
	/**
	 * @url GET session
	 */
	public function getSession(){
	
		$session = Session::singleton();
		return array('id' => session_id());
	
	}
	
	/**
	 * @url DELETE session
	 * @url DELETE session/{session_id}
	 */
	public function destroySession($session_id){

		$session = Session::singleton();
		$session->destroySession($session_id);
		
		return array('notifications' => Notifications::getAll());
		
	}

/**************************** NOTIFICATIONS ****************************/	
	/**
	 * @url GET notifications/all
	 * @param int $roleId
	 */
	public function getAllNotifications($roleId = null)
	{
		$session = Session::singleton();
		$session->setRole($roleId);
		
		$session->role->getViolations();
		// RuleEngine::checkProcessRules($session->role->id);  // Leave here to measure performance difference
		
		return Notifications::getAll();

	}
	
	/**
	 * @url GET extensions/all
	 */
	public function getAllExtensions()
	{
		return (array) $GLOBALS['apps'];
	}

/**************************** OBJECTINTERFACES ****************************/

	/**
	 * @url GET interface/{interfaceName}
	 * @url GET interface/{interfaceName}/{atomid}
	 * @param string $interfaceName
	 * @param string $sessionId
	 * @param string $atomid
	 * @param int $roleId
	 */
	public function getAtom($interfaceName, $sessionId, $atomid = null, $roleId = null)
	{
		$session = Session::singleton($sessionId);
	
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
	
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
		
		$result = array();
		if(is_null($atomid)) {
			foreach(Concept::getAllAtomIds($session->interface->srcConcept) as $atomId){
				$atom = new Atom($atomId, $session->interface->srcConcept);
				$result = array_merge($result, (array)$atom->getContent($session->interface));
			}
		}else{
	
			$atom = new Atom($atomid, $session->interface-srcConcept);
			$result = $atom->getContent($session->interface);
		}
		
		if(empty($result)) throw new RestException(404, 'Resource not found');

		return array_values($result); // array_values transforms assoc array to non-assoc array

	}
	
	/**
	 * @url PATCH interface/{interfaceName}/{atomid}
	 * @param string $interfaceName
	 * @param string $sessionId
	 * @param string $atomid
	 * @param int $roleId
	 */
	public function patchAtom($interfaceName, $sessionId, $atomid, $roleId = null, $request_data = null)
	{
		$session = Session::singleton($sessionId);
		
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
	
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
	
		$interface = new ObjectInterface($interfaceName);
		$atom = new Atom($atomid, $interface->srcConcept);		
		
		return array_merge(
				array('patches' => $atom->patch($interface, $request_data)),
				array('content' => current((array)$atom->getContent($interface))),
				array('notifications' => Notifications::getAll()));
	
	}
	
	/**
	 * @url DELETE interface/{interfaceName}/{atomid}
	 * @param string $interfaceName
	 * @param string $sessionId
	 * @param string $atomid
	 * @param int $roleId
	 */
	public function deleteAtom($interfaceName, $sessionId, $atomid, $roleId = null)
	{
		$session = Session::singleton($sessionId);
	
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
	
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
			
		$interface = new ObjectInterface($interfaceName);
		
		// TODO: insert check if Atom may be deleted with this interface
		
		$atom = new Atom($atomid, $interface->srcConcept);
		$atom->delete();	
		
		return array('notifications' => Notifications::getAll());
	
	}
	/**
	 * @url POST interface/{interfaceName}
	 * @param string $interfaceName
	 * @param string $sessionId
	 * @param int $roleId
	 */
	public function postAtom($interfaceName, $sessionId, $roleId = null){
		$session = Session::singleton($sessionId);
		$db = Database::singleton();
		
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
		
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
			
		$interface = new ObjectInterface($interfaceName);
		
		// TODO: insert check if Atom may be created with this interface
		
		$concept = $session->interface->srcConcept;
		$atomId = $db->addAtomToConcept(Concept::createNewAtom($concept), $concept);
		$atom = new Atom($atomId, $concept);
		
		return array_values($atom->getContent($session->interface));
		
	}
	

/**************************** CONCEPTS AND ATOMS ****************************/
    /**
     * @url GET concepts
     */
    public function getConcepts()
    {
        return Concept::getAllConcepts(); // "Return list of all concepts"
    }
	
	/**
     * @url GET concept/{concept}
	 * @param string $direction the direction of the relations: "from", "to", "both".
     */
    public function getConcept($concept, $direction = "both")
    {
        return "Return all attributes and relations (direction: $direction) of concept $concept";
    }
	
	/**
     * @url GET concept/{concept}/atoms
     */
    public function getConceptAtoms($concept)
    {
        return Concept::getAllAtomObjects($concept); // "Return list of all atoms for $concept"
    }
    
	/**
     * @url POST concept/{concept}/atom
	 * @status 201
     */
	function postConceptAtom($concept){ 
		$database = Database::singleton();
	 
		return $database->addAtomToConcept(Concept::createNewAtom($concept), $concept); // insert new atom in database
	}

	/**
	 * @url DELETE concept/{concept}/atom/{atom}
	 * @status 204
	 */
	function deleteConceptAtom($concept, $atom){ 
		$database = Database::singleton();
		
		$database->deleteAtom($atom, $concept); // delete atom + all relations with other atoms
	}
	
/**************************** RULES AND VIOLATIONS ****************************/
	
	/**
     * @url GET rule
	 * @url GET rule/{ruleName}
     */
    public function getRules($ruleName = NULL)
    {
		if(isset($ruleName)){
			
			return RuleEngine::getRule($ruleName); // "Return rule with name $ruleName";
		}else{
			return "Return list of all rules";
		}
    }
	
	/**
     * @url GET rule/{ruleName}/violations
     */
    public function getViolations($ruleName)
    {
		$rule = RuleEngine::getRule($ruleName);
        return RuleEngine::checkProcessRule($rule); // "Return list of violations (tuples of src, tgt atom) for rule $rule"
    }
    
	
/**************************** ROLES ****************************/
	
	/**
     * @url GET roles/all
     */
    public function getAllRoles()
    {
		return Role::getAllRoles(); // "Return list of all roles with properties as defined in class Role"
		
    }
    
    /**
     * @url GET role
     * @url GET role/{roleNr}
     */
    public function getRole($roleNr = NULL)
    {
    	if($roleNr !== NULL){	// do not use isset(), because roleNr can be 0.
    		return new Role($roleNr); // Return role with properties as defined in class Role
    	}else{
    		return new Role(); // Return default role
    			
    	}
    }
    
	
/**************************** INTERFACES ****************************/
    
    /**
     * @url GET interfaces/all
     * @param int $roleId
     */
    public function getAllInterfaces($roleId = null){
    	$session = Session::singleton();
    	
    	try{
    		$session->setRole($roleId);
    	}catch(Exception $e){
    		throw new RestException(404, $e->getMessage());
    	}
    
    	return array ('top' => $session->role->getInterfaces(true)
    				 ,'new' => $session->role->getInterfaces(false));
    }
    
	/**
     * @url GET interfaces
	 * @url GET interfaces/{interfaceName}
	 * @param string $interfaceName
	 * @param int $roleId
     */
    public function getInterfaces($interfaceName = null, $roleId = null){
    	$session = Session::singleton();
    	try{
    		$session->setRole($roleId);
    	}catch(Exception $e){
    		throw new RestException(404, $e->getMessage());
    	}
    	
    	if(!is_null($interfaceName)){
	    	try{
	    		$session->setInterface($interfaceName);
	    	}catch(Exception $e){
	    		throw new RestException(404, 'Interface \''.$interfaceName.'\' does not exists');
	    	}
    		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not for specified role: ' . $roleId );
    		return $session->interface->getInterface();
        }else{
        	return $session->role->getInterfaces();  // "Return list of all interfaces"
		}
    }	
	
}

?>