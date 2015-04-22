<?php

class Role {

	public $id;
	public $name; // TODO: kan weg?
	public $label;
	public $maintains = array();
	public $interfaces = array();

	public function __construct($id = null){
		global $allRoles; // from Generics.php
		
		if(is_null($id)){ 
			$id = DEFAULT_ROLEID; // localSettings.php
			Notifications::addLog("Default role selected");
		}
		
		// Check if role exists
		if(!key_exists($id, $allRoles)) throw new Exception ("Role with roleId \'$id\' does not exists", 404);
		
		// Name of role
		$this->id = $id;
		$this->name = $allRoles[$id]['name'];
		$this->label = $allRoles[$id]['name'];
		
		// Rules that are maintained by this role
		$this->maintains = (array)$allRoles[$id]['ruleNames'];
		
		// Interfaces that are accessible by this role
		foreach (InterfaceObject::getAllInterfaceObjects() as $interfaceId => $interface){
			if (InterfaceObject::isInterfaceForRole($this->name, $interfaceId)) $this->interfaces[] = new InterfaceObject($interfaceId);
		}		
	}
		
	public static function getAllRoles(){
		global $allRoles; // from Generics.php
		
		$roles = array();
		foreach((array)$allRoles as $key => $arr){
			$roles[$key] = new Role($key);
		}
		
		return $roles;
	}
	
	public static function getRole($roleName){
		global $allRoles; // from Generics.php
		
		foreach((array)$allRoles as $key => $arr){
			if($arr['name'] == $roleName) return new Role($key);
		}
		return false; // when $roleName is not found in $allRoles
	}
	
	public function getInterfaces($topLevel = null, $srcConcept = null){ // $topLevel: true, false, null (=all), $srcConcept: <concept> or null (=all)
		$interfaces = array();
		
		foreach($this->interfaces as $interface){
			
			if(isset($topLevel)){
				switch ($topLevel){
					case true :
						if($interface->srcConcept == 'SESSION' || $interface->srcConcept == 'ONE') 
							$interfaces[] = $interface;
						break;
					case false :
						if($interface->srcConcept != 'SESSION' && $interface->srcConcept != 'ONE') $interfaces[] = $interface;
						break;
				}
			}else{
				if(isset($srcConcept)){ // TODO: moet dit niet het tgtConcept zijn??
					if($interface->srcConcept == $srcConcept 
						|| in_array($srcConcept, Concept::getSpecializations($interface->srcConcept)) ) {
						
						$interfaces[] = $interface;
					}
				}else{
					$interfaces[] = $interface;
				}
			}
		}
		
		return $interfaces;
	}
	
	public function isInterfaceForRole($interfaceId){
		return in_array($interfaceId, array_map(function($o) { return $o->id; }, $this->interfaces));
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

}


?>