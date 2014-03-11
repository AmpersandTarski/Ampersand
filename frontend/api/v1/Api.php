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
     * @url GET concept/{concept}/atoms/
     */
    public function getAtoms($concept)
    {
        return Concept::getAllAtoms($concept); // "Return list of all atoms for $concept"
    }
	
	/**
     * @url GET concept/{concept}/atom/{atom}/
     */
    public function getAtom($concept, $atom)
    {
        return "Return all attributes of atom $atom of concept $concept";
    }
	
	/**
     * @url GET concept/{concept}/atom/{atom}/links/
	 * @param string $direction the direction of the requested links: "from", "to", "both".
     */
    public function getLinks($concept, $atom, $direction = "both")
    {
        return "Return all links (direction: $direction) atom $atom of concept $concept";
    }
	
	/**
     * @url GET concept/{concept}/relations/
	 * @param string $direction the direction of the requested relations: "from", "to", "both".
     */
    public function getRelations($concept, $direction = "both")
    {
        return "Return list of all relations (direction: $direction) for $concept";
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
     */
    public function getInterfaces($interfaceName = NULL)
    {
        if($interfaceName !== NULL){
			
			return new UserInterface($interfaceName); // "Return interface with properties as defined in class UserInterfae"
		}else{
			return UserInterface::getAllInterfaces(); // "Return list of all interfaces"
			
		}
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

