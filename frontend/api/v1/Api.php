<?php

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
		
		// TODO: check all invariantRules
		ErrorHandling::addSuccess("Database reset to initial value");
		
		return ErrorHandling::getAll(); // Return all notifications
		
	}
	
	/**
	 * @url GET session/
	 */
	public function getSession(){
	
		$session = Session::singleton();
		return array('id' => session_id());
	
	}
	
	/**
	 * @url DELETE session/
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
		RuleEngine::checkRules($session->role->id);
		
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
		

/**************************** CONCEPTS AND ATOMS ****************************/
    /**
     * @url GET concepts/
     */
    public function getConcepts()
    {
        return Concept::getAllConcepts(); // "Return list of all concepts"
    }
	
	/**
     * @url GET concept/{concept}/
	 * @param string $direction the direction of the relations: "from", "to", "both".
     */
    public function getConcept($concept, $direction = "both")
    {
        return "Return all attributes and relations (direction: $direction) of concept $concept";
    }
	
	/**
     * @url GET concept/{concept}/atoms/
     */
    public function getAtoms($concept)
    {
        return Concept::getAllAtoms($concept); // "Return list of all atoms for $concept"
    }
    
    /**
     * @url GET interface/{interfaceName}/atoms
     * @url GET interface/{interfaceName}/atom/{atomid}/
     * @param string $interfaceName
     * @param string $atomid
     * @param int $roleId
     */
    public function getAtom($interfaceName, $atomid = null, $roleId = null)
    {
    	$session = Session::singleton();
    	 
    	if(is_null($atomid)) $atomid = session_id(); // TODO: wordt al gedaan door Session->setAtom(), hieronder met session->atom werken?
    	 
    	try{
    		$session->setRole($roleId);
    		$session->setInterface($interfaceName);
    	}catch(Exception $e){
    		throw new RestException(404, $e->getMessage());
    	}
    	 
    	if(!$session->role->isInterfaceForRole($interfaceName)) throw new RestException(403, 'Interface is not accessible for specified role: '.$session->role->name.' (roleId:' . $roleId .')' );
    	 
    	$atom = new Atom($atomid);
    	$interface = new ObjectInterface($interfaceName);
    	return current($atom->getContent($interface));
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
     * @url POST concept/{concept}/atom/
	 * @status 201
     */
	function postAtom($concept){ 
		$database = Database::singleton();
	 
		return $database->addAtomToConcept(Concept::createNewAtom($concept), $concept); // insert new atom in database
	}

	/**
	 * @url DELETE concept/{concept}/atom/{atom}/
	 * @status 204
	 */
	function deleteAtom($concept, $atom){ 
		$database = Database::singleton();
		
		$database->deleteAtom($atom, $concept); // delete atom + all relations with other atoms
	}
	
/**************************** RELATIONS AND LINKS ****************************/
	
	/**
	 * @url POST relation/{relationName}/link
	 * @status 201
	 */
	public function postLink($relation, $srcConcept, $tgtConcept, $srcAtom = null, $tgtAtom = null){
		$database = Database::singleton();
	
		// Check if combination of $relation, $srcConcept, $tgtConcept exists. Otherwise this causes database issues.
		if (!Relation::isCombination($relation, $srcConcept, $tgtConcept)){
			// Tip: If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration. TODO: create ticket for Prototype generator to fix this.
			throw new Exception('Cannot find ' . $relation . '[' . $srcConcept . '*' . $tgtConcept.']');
		}
		
		// If srcAtom is not specified, a new atom of srcConcept is created
		if($srcAtom == null){
			$srcAtom = $database->addAtomToConcept(Concept::createNewAtom($srcConcept), $srcConcept);
		}elseif(!Concept::isAtomInConcept($srcAtom, $srcConcept)){
			$database->addAtomToConcept($srcAtom, $srcConcept);
		}
		
		// If tgtAtom is not specified, a new atom of tgtConcept is created
		if($tgtAtom == null){
			$tgtAtom = $database->addAtomToConcept(Concept::createNewAtom($tgtConcept), $tgtConcept);
		}elseif(!Concept::isAtomInConcept($tgtAtom, $tgtConcept)){
			$database->addAtomToConcept($tgtAtom, $tgtConcept);
		}
		
		$database->editUpdate($relation, false, $srcAtom, $srcConcept, $tgtAtom, $tgtConcept, 'child', '');
		
		ErrorHandling::addLog('Tupple ('.$srcAtom.' - '.$tgtAtom.') inserted into '.$relation.'['.$srcConcept.'*'.$tgtConcept.']');
		
		return array('relation' => $relation
					, 'srcConcept' => $srcConcept
					, 'tgtConcept' => $tgtConcept
					, 'srcAtom' => $srcAtom
					, 'tgtAtom' => $tgtAtom);
	}
	
	/**
	 * @url DELETE relation/{relationName}/link
	 * @status 204
	 */
	public function deleteLink($relation, $srcConcept, $srcAtom, $tgtConcept, $tgtAtom){
		$database = Database::singleton();
		
		// Check if combination of $relation, $srcConcept, $tgtConcept exists. Otherwise this causes database issues.
		if (!Relation::isCombination($relation, $srcConcept, $tgtConcept)){
			// Tip: If you have defined this relation in Ampersand, then you must be sure to also have defined an INTERFACE that uses this relation (or else it does not show up in the PHP relation administration. TODO: create ticket for Prototype generator to fix this.
			throw new Exception('Cannot find ' . $relation . '[' . $srcConcept . '*' . $tgtConcept.']');
		}
		
		$database->editDelete($relation, false, $srcAtom, $srcConcept, $tgtAtom, $tgtConcept);
		
		ErrorHandling::addLog('Tupple ('.$srcAtom.' - '.$tgtAtom.') deleted from '.$relation.'['.$srcConcept.'*'.$tgtConcept.']');
	}
	
/**************************** RULES AND VIOLATIONS ****************************/
	
	/**
     * @url GET rule/
	 * @url GET rule/{ruleName}/
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
     * @url GET rule/{ruleName}/violations/
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
     * @url GET role/{roleNr}/
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
     * @url GET interfaces/
	 * @url GET interface/{interfaceName}/
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


/**************************** OLD TRANSACTION ****************************/
	/**
     * @url POST transaction/
     */
    public function processCommands($commands, $role)
    {
		$session = Session::singleton();
		$session->database->transaction(json_decode($commands), $role);
		
		return ErrorHandling::getAll();

    }
	
	
}

?>