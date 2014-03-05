<?php


class Api
{

/**************************** CONCEPTS AND ATOMS ****************************/
    /**
     * @url GET concepts/
     */
    public function getConcepts()
    {
        return "List of all concepts";
    }
	
	/**
     * @url GET concept/{concept}/atoms/
     */
    public function getAtoms($concept)
    {
        return "List of all atoms for $concept";
    }
	
	/**
     * @url GET concept/{concept}/atom/{atom}/
     */
    public function getAtom($concept, $atom)
    {
        return "All attributes of atom $atom of concept $concept";
    }
	
	/**
     * @url GET concept/{concept}/atom/{atom}/links/
	 * @param string $direction the direction of the requested links: "from", "to", "both".
     */
    public function getLinks($concept, $atom, $direction = "both")
    {
        return "All links (direction: $direction) atom $atom of concept $concept";
    }
	
	/**
     * @url GET concept/{concept}/relations/
	 * @param string $direction the direction of the requested relations: "from", "to", "both".
     */
    public function getRelations($concept, $direction = "both")
    {
        return "List of all relations (direction: $direction) for $concept";
    }
	
/**************************** RULES AND VIOLATIONS ****************************/
	
	/**
     * @url GET rules/
	 * @url GET role/{role}/rules/
     */
    public function getRules($role = NULL)
    {
        if($role){
			return "List of rules that are maintained by role $role";
		}else{
			return "List of all rules";
		}
    }
	
	/**
     * @url GET rule/{rule}/violations/
     */
    public function getViolations($rule)
    {
        return "List of violations (tuples of src, tgt atom) for rule $rule";
    }
	
/**************************** ROLES ****************************/
	
	/**
     * @url GET roles/
	 * @url GET rule/{rule}/roles/
     */
    public function getRoles($rule = NULL)
    {
		
        if($rule){
			return "List of all roles that maintain rule $rule";
		}else{
			return Session::getRoles();
			
		}
    }
	
}

?>

