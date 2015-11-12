<?php

class Role {

	public $id;
	public $label;
	public $maintains = array();
	public $interfaces = array();
	public $editableConcepts = array();
	public $active = false;
	
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
		
		// Editable concepts
		$this->editableConcepts = (array)$roleInfo['editableConcepts'];
		
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
		
		return (array)$allRoles;
	}
		
	public static function getAllRoleObjects(){		
		$roleObjects = array();
		foreach(Role::getAllRoles() as $role){
			$roleObjects[] = new Role($role['id']);
		}
		
		return $roleObjects;
	}
	
	public static function getRoleByName($roleName){		
		foreach(Role::getAllRoles() as $arr){
			if($arr['name'] == $roleName) return new Role($arr['id']);
		}
		return false; // when $roleName is not found in $allRoles
	}
	
	public function isInterfaceForRole($interfaceId){		
		return in_array($interfaceId, array_map(function($o) { return $o->id; }, $this->interfaces));
	}
}
?>