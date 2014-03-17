<?php

class Api
{

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
     * @url GET concept/{concept}/atom/{atom}/
	 * @param sting $depth the number of levels of linked atoms to return
     */
    public function getAtom($concept, $atom, $depth = "1")
    {
        return "Return all attributes and links (depth: $depth) of atom $atom of concept $concept";
    }
	
/**************************** RULES AND VIOLATIONS ****************************/
	
	/**
     * @url GET rules/
	 * @url GET rule/{ruleName}/
     */
    public function getRules($ruleName = NULL)
    {
		if(isset($ruleName)){
			
			return Session::getRule($ruleName); // "Return rule with name $ruleName";
		}else{
			return "Return list of all rules";
		}
    }
	
	/**
     * @url GET rule/{ruleName}/violations/
     */
    public function getViolations($ruleName)
    {
		$rule = Session::getRule($ruleName);
        return RuleEngine::checkProcessRule($rule); // "Return list of violations (tuples of src, tgt atom) for rule $rule"
    }
	
/**************************** ROLES ****************************/
	
	/**
     * @url GET roles/
	 * @url GET role/{roleNr}/
     */
    public function getRoles($roleNr = NULL)
    {
        if($roleNr !== NULL){	// do not use isset(), because roleNr can be 0.		
			return new Role($roleNr); // "Return role with properties as defined in class Role"
		}else{
			return Role::getAllRoles(); // "Return list of all roles with properties as defined in class Role"
			
		}
    }
	
	/**
	 * @url GET role/{roleNr}/interfaces
	 * @url GET role/{roleNr}/interface/{interfaceName}
     */
	public function getRoleInterfaces($roleNr, $interfaceName = NULL)
	{
		$role = new Role($roleNr);
		
		if(isset($interfaceName)){
			if(!$role->isInterfaceForRole($interfaceName)) return false; // Interface is not for specified role
			
			return new UserInterface($interfaceName);
		}else{
			return $role->interfaces;
		}
	}
	
/**************************** INTERFACES ****************************/
	
	/**
     * @url GET interfaces/
	 * @url GET interface/{interfaceName}/
	 * @url GET interface/{interfaceName}/atom/{atom}
	 * @view views/default.mustache
     */
    public function getInterfaces($interfaceName = NULL, $atom = "1")
    {
		// HtmlFormat::$view = 'default.mustache';
		
        if($interfaceName !== NULL){
			$interface = new UserInterface($interfaceName);
			return $interface->getAtomsAndLinks($atom); // "Return interface with properties as defined in class UserInterfae"
		}else{
			return UserInterface::getAllInterfaces(); // "Return list of all interfaces"
			
		}
    }
	
	/**
	 * @url GET viewer/{viewerName}/interface/{interfaceName}/
	 * @url GET viewer/{viewerName}/interface/{interfaceName}/atom/{atom}
     */	
	public function getInterfaceWithView($viewerName, $interfaceName, $atom = "1")
	{
		$interface = new UserInterface($interfaceName);
		return Viewer::viewInterface($interface, $atom);
	
	}

/**************************** POST ****************************/
	/**
     * @url POST transaction/
     */
    public function processCommands($commands, $role)
    {
		$database = Database::singleton();
		
		$database->transaction(json_decode($commands), $role);
		
		return ErrorHandling::getAll();
		
    }
	
}

?>