<?php

class Role {

	public $id;
	public $label;
	public $maintains = array();
	public $interfaces = array();
	
	/*
	 * param int $id
	 */
	public function __construct($roleId = 0){
				
		if(!is_int($roleId)) throw new Exception("No valid role id provided. Role id must be an integer", 500);
		
		// Role information
		$roleInfo = Role::getRoleInfo($roleId);
		$this->id = $roleId;
		$this->label = $roleInfo['name'];
		
		// Rules that are maintained by this role
		$this->maintains = (array)$roleInfo['ruleNames'];
		
		// Interfaces that are accessible by this role
		foreach (InterfaceObject::getAllInterfaceObjects() as $interfaceId => $interface){
			$ifc = new InterfaceObject($interfaceId);
			if (in_array($this->label, $ifc->interfaceRoles) || empty($ifc->interfaceRoles)) $this->interfaces[] = $ifc;
		}
	}
	
	public static function getRoleInfo($roleId){		
		foreach(Role::getAllRoles() as $arr){
			if($arr['id'] == $roleId) return $arr;
		}
		throw new Exception ("Role with roleId \'$roleId\' does not exists", 404);
	}
	
	public static function getAllRoles(){
		global $allRoles; // from Generics.php
		
		$roles = $allRoles;
		$roles[] = Role::getRoleZero();
		return (array)$roles;
	}
		
	public static function getAllRoleObjects(){		
		$roleObjects = array();
		foreach(Role::getAllRoles() as $role){
			$roleObjects[] = new Role($role['id']);
		}
		
		return $roleObjects;
	}
	
	public static function getRoleZero(){
		return array( 'id' => 0
            		, 'name' => 'No role'
            		, 'ruleNames'  => array ()
            		, 'interfaces' => array ());
	}
	
	public static function getAllSessionRoles($sessionId){		
		$sessionRoleLabels = array();
		$sessionRoles = array();
		
		$interface = new InterfaceObject('SessionRoles');
		$session = new Atom($sessionId, 'SESSION');
		$sessionRoleLabels = array_keys((array)$session->getContent($interface, true));
		
		foreach(Role::getAllRoleObjects() as $role){
			if(in_array($role->label, $sessionRoleLabels) || $role->id == 0) $sessionRoles[] = $role;
		}
		
		return $sessionRoles;
	}
	
	public static function getRoleByName($roleName){		
		foreach(Role::getAllRoles() as $arr){
			if($arr['name'] == $roleName) return new Role($arr['id']);
		}
		return false; // when $roleName is not found in $allRoles
	}
	
	private function getInterfacesForConcept($concept){
		$interfaces = array();
		
		foreach($this->getSessionInterfaces() as $interface){
			if($interface->srcConcept == $concept || 
					in_array($concept, Concept::getSpecializations($interface->srcConcept)) 
				) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesToReadConcept($concept){
		$interfaces = array();
		
		foreach($this->getInterfacesForConcept($concept) as $interface){
			if($interface->crudR) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesForNavBar(){
		$interfaces = array();
		foreach($this->interfaces as $interface){
			if(($interface->srcConcept == 'SESSION' || $interface->srcConcept == 'ONE') && $interface->crudR) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function getInterfacesToCreateAtom(){
		$interfaces = array();
		foreach($this->interfaces as $interface){
			//if($interface->srcConcept != 'SESSION' && $interface->srcConcept != 'ONE') $interfaces[] = $interface;
			if($interface->crudC && $interface->isIdent) $interfaces[] = $interface;
		}
		return $interfaces;
	}
	
	public function isInterfaceForRole($interfaceId){		
		return in_array($interfaceId, array_map(function($o) { return $o->id; }, $this->getSessionInterfaces()));
	}
	
	public function getViolations(){
		$conjunctIds = array();
		$conjunctRuleMap = array();
		foreach ($this->maintains as $ruleName){
			$rule = RuleEngine::getRule($ruleName);
			foreach($rule['conjunctIds'] as $conjunctId) $conjunctRuleMap[$conjunctId][] = $ruleName; 
			$conjunctIds = array_merge($conjunctIds, $rule['conjunctIds']);
		}
		$signals = RuleEngine::getSignalsFromDB($conjunctIds);
		
		/*
		 * $signal[] = array('conjId' => , 'src' => , 'tgt' => )
		 * 
		 */
		foreach ($signals as $signal){
			foreach($conjunctRuleMap[$signal['conjId']] as $ruleName){
				Notifications::addViolation(RuleEngine::getRule($ruleName), $signal['src'], $signal['tgt']);
			}			
		}
		
	}
	
	private function getSessionInterfaces(){
		if(LOGIN_ENABLED){
			$session = Session::singleton();
			return (array)$session->accessibleInterfaces;
		}else{
			return (array)$this->interfaces;
		}
	}
}
?>