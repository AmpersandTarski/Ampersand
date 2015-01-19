<?php

use Luracast\Restler\Data\Object;
class Api
{
/****************************** INSTALLER & SESSION RESET ******************************/
	/**
	 * @url GET installer
	 * @param int $roleId
	 */
	public function installer($roleId){
		include (__DIR__ . '/../../ampersand/InstallerDBstruct.php');
		include (__DIR__ . '/../../ampersand/InstallerDefPop.php');
		
		RuleEngine::checkRules($roleId);
		
		ErrorHandling::addSuccess("Database reset to initial value");
		
		return ErrorHandling::getAll(); // Return all notifications
		
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
		RuleEngine::checkProcessRules($session->role->id);
		
		$test = ErrorHandling::getAll(); // "Return all notifications
		
		return $test;
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
	 * @url GET interface/{interfaceName}/atom
	 * @url GET interface/{interfaceName}/atom/{atomid}
	 * @param string $interfaceName
	 * @param string $sessionId
	 * @param string $atomid
	 * @param int $roleId
	 */
	public function getAtom($interfaceName, $sessionId, $atomid = null, $roleId = null)
	{
		$session = Session::singleton($sessionId);
	
		if(is_null($atomid)) $atomid = $sessionId;
	
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
	
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
	
		$atom = new Atom($atomid);
		return current($atom->getContent($session->interface));
	}
	
	/**
	 * @url PATCH interface/{interfaceName}/atom/{atomid}
	 * @param string $interfaceName
	 * @param string $atomid
	 * @param int $roleId
	 */
	public function patchAtom($interfaceName, $atomid, $roleId = null, $request_data = null)
	{
		$session = Session::singleton();
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
	
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
	
		$atom = new Atom($atomid);
		$interface = new ObjectInterface($interfaceName);
		 
		return $atom->patch($interface, $request_data);
	
	}
	
	/**
	 * @url GET interface/{interfaceName}/atoms
	 * @param string $interfaceName
	 * @param string $sessionId
	 * @param int $roleId
	 */
	public function getAtoms($interfaceName, $sessionId, $roleId = null)
	{
		$session = Session::singleton($sessionId);
				
		try{
			$session->setRole($roleId);
			$session->setInterface($interfaceName);
		}catch(Exception $e){
			throw new RestException(404, $e->getMessage());
		}
		
		if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
		
		foreach(Concept::getAllAtomIds($session->interface->srcConcept) as $atomId){
			$atom = new Atom($atomId, $session->interface->srcConcept);
			$arr[] = current($atom->getContent($session->interface));
		}
		return $arr;
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
	function postAtom($concept){ 
		$database = Database::singleton();
	 
		return $database->addAtomToConcept(Concept::createNewAtom($concept), $concept); // insert new atom in database
	}

	/**
	 * @url DELETE concept/{concept}/atom/{atom}
	 * @status 204
	 */
	function deleteAtom($concept, $atom){ 
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
     * @url GET interfaces/top
     * @param int $roleId
     */
    public function getTopLevelInterfaces($roleId = null)
    {
    	$session = Session::singleton();
    	try{
    		$session->setRole($roleId);
    	}catch(Exception $e){
    		throw new RestException(404, $e->getMessage());
    	}
    	 
    	return $session->role->getInterfaces(true);  // "Return list of all interfaces"
    }
    
	/**
     * @url GET interfaces
	 * @url GET interface/{interfaceName}
	 * @param string $interfaceName
	 * @param int $roleId
     */
    public function getInterfaces($interfaceName = null, $roleId = null)
    {
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