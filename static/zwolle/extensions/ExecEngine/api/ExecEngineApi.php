<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;

class ExecEngineApi{
	
	/****************************** PARSE FILE ******************************/
	/**
	 * @url GET run
	 */
	public function run(){
		try{
			$session = Session::singleton();
			$db = Database::singleton();
			
			$allowedRoles = (array)Config::get('allowedRolesForRunFunction','execEngine');
			if(LOGIN_ENABLED && !is_null($allowedRoles)){
				$ok = false;
				
				$sessionRoles = Role::getAllSessionRoles(session_id());
				foreach($sessionRoles as $role){
					if(in_array($role->label, $allowedRoles)) $ok = true;
				}
				if(!$ok) throw new Exception("You do not have access to run the exec engine", 401);
			}
				
			$session->setRole();
			
			// ExecEngine::run(); // Not required, because closeTransaction call below already kicks the ExecEngine
			
			$db->closeTransaction('Run completed',false,true,false);
			
			$result = array('notifications' => Notifications::getAll());
			return $result;
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
}
?>