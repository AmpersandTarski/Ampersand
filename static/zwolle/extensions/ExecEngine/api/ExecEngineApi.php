<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;

class ExecEngineApi{
	
	/****************************** PARSE FILE ******************************/
	/**
	 * @url GET run
	 * @param array $roleIds
	 */
	public function run($roleIds = null){
		try{
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			
			// Check sessionRoles if allowedRolesForRunFunction is specified
			$allowedRoles = Config::get('allowedRolesForRunFunction','execEngine');
			if(!is_null($allowedRoles)){
				$ok = false;
				
				foreach($session->getSessionRoles() as $role){
					if(in_array($role->label, $allowedRoles)) $ok = true;
				}
				if(!$ok) throw new Exception("You do not have access to run the exec engine", 401);
			}			
			
			ExecEngine::run(true);
			
			$db = Database::singleton();
			$db->closeTransaction('Run completed',false,true,false);
			
			$result = array('notifications' => Notifications::getAll());
			return $result;
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
}
?>