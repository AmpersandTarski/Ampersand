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
			
			$session->setRole();
			
			ExecEngine::run();
			
			$db->closeTransaction('Run completed',false,true,false);
			
			$result = array('notifications' => Notifications::getAll());
			return $result;
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
}
?>