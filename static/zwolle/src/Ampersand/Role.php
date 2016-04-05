<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;
use Ampersand\Interfacing\InterfaceObject;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Role {
    /**
     * Contains all role definitions
     * @var Role[]
     */
    private static $allRoles;
    
    /**
     * Role identifier
     * @var int
     */
	public $id;
	
	/**
	 * Name of the role
	 * @var string
	 */
	public $label;
	
	/**
	 * Specifies if this role is active within the current session
	 * @var boolean
	 */
	public $active = false;
	
	/**
	 * Specifies all rules that are maintained by this role
	 * @var array
	 */
	private $maintains = array();
	
	/**
	 * Specifies all interfaces that are accessible by this role
	 * @var unknown
	 */
	private $interfaces = array();
	
	/**
	 * Constructor of role
	 * Private function to prevent outside instantiation of roles. Use Role::getRoleById($roleId) or Role::getRoleByName($roleName)
	 * 
	 * @param array $roleDef
	 */
	private function __construct($roleDef){
		$this->id = $roleDef['id'];
		$this->label = $roleDef['name'];
		$this->maintains = $roleDef['maintains'];
		
		foreach ($roleDef['interfaces'] as $ifcId){
		    $this->interfaces[] = InterfaceObject::getInterface($ifcId);
		}
	}
	
	/**
	 * Get array of rules names that are maintained by this role
	 * @return string[]
	 */
	public function maintains(){
		return $this->maintains;
	}
	
	/**
	 * Get interfaces that are accessible for this role
	 * @return InterfaceObject[]
	 */
	public function interfaces(){
		return $this->interfaces;
	}
	
	/**********************************************************************************************
	 *
	 * Static functions
	 *
	 *********************************************************************************************/
	
	/**
	 * Return role object
	 * @param int $roleId
	 * @throws Exception
	 * return Role
	 */
	public static function getRoleById($roleId){
	    if(!is_int($roleId)) throw new Exception("No valid role id provided. Role id must be an integer", 500);
	    
	    foreach(self::getAllRoles() as $role){
	        if($role->id == $roleId) return $role;
	    }
	    
	    throw new Exception("Role with id '{$roleId}' is not defined", 500);
	}
	
	/**
	 * Return role object
	 * @param string $roleName
	 * @throws Exception if role is not defined
	 * @return Role
	 */
	public static function getRoleByName($roleName){
	    if(!array_key_exists($roleName, $roles = self::getAllRoles())) throw new Exception("Role '{$roleName}' is not defined", 500);
	
	    return $roles[$roleName];
	}
	
	/**
	 * Returns array with all role objects
	 * @return Role[]
	 */
	public static function getAllRoles(){
	    if(!isset(self::$allRoles)) self::setAllRoles();
	     
	    return self::$allRoles;
	}
	
	/**
	 * Import all role definitions from json file and create and save Role objects
	 * @return void
	 */
	private static function setAllRoles(){
	    self::$allRoles = array();
	
	    // import json file
	    $file = file_get_contents(Config::get('pathToGeneratedFiles') . 'roles.json');
	    $allRoleDefs = (array)json_decode($file, true);
	    
	    foreach ($allRoleDefs as $roleDef) self::$allRoles[$roleDef['name']] = new Role($roleDef);
	}
}
?>