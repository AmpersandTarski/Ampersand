<?php

use Luracast\Restler\Data\Object;
use Luracast\Restler\RestException;
class Api{	
	
	/****************************** INSTALLER & SESSION RESET ******************************/
	/**
	 * @url GET installer
	 * @param array $roleIds
	 */
	public function installer($roleIds = null){
		try{
			if(Config::get('productionEnv')) throw new Exception ("Database reinstall not allowed in production environment", 403);
			
			Database::createDB();
			
			$db = Database::singleton();
			$db->reinstallDB();
			
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			
			return Notifications::getAll(); // Return all notifications
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url DELETE session/{session_id}
	 */
	public function destroySession($session_id){
		try{
			if($session_id != session_id()) throw new Exception ("You can only destroy your own session", 403);
			$session = Session::singleton();
			$session->destroySession();
		
			return array('notifications' => Notifications::getAll());
			
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**************************** FILE ****************************/
	/**
	 * @url POST file
	 * @param array $roleIds
	 */
	public function fileUpload($roleIds = null){
		try{
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			
			// TODO: Check if upload is allowed in interface
			
			if (is_uploaded_file($_FILES['file']['tmp_name'])){
				$tmp_name = $_FILES['file']['tmp_name'];
				$new_name = time() . '_' . $_FILES['file']['name'];
				$target = Config::get('uploadPath') . '/' . $new_name;
				$result = move_uploaded_file($tmp_name, $target);
				
				if($result) Notifications::addSuccess("File '".$new_name."' uploaded");
				else Notifications::addError("Error in file upload");
			}else{
			    Notifications::addError('No file uploaded');
			}
			
			$newAtom = $session->database->addAtomToConcept(Concept::createNewAtom('Upload'), 'Upload');
			$session->database->editUpdate('fileName', false, $newAtom, 'Upload', $new_name, 'FileName');
			$session->database->editUpdate('originalFileName', false, $newAtom, 'Upload', $_FILES['file']['name'], 'FileName');
			$session->database->commitTransaction();
			
			$result = array('notifications' => Notifications::getAll(), 'files' => $_FILES, 'uploadId' => $newAtom);
			return $result;
	
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}

	/**************************** INTERFACES ****************************/
	/**
	 * @url GET resource/{concept}/{srcAtomId}/{interfaceId}
	 * @url GET resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param array $roleIds
	 * @param boolean $inclLinktoData
	 * @param string $arrayType
	 * @param boolean $metaData
	 */
	public function getAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId = null, $roleIds = null, $inclLinktoData = false, $arrayType = "assoc", $metaData = true){
		try{
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			$session->setInterface($interfaceId);
		
			$result = array();
			
			if($session->interface->srcConcept != $concept) throw new Exception("Concept '$concept' cannot be used as source concept for interface '".$session->interface->label."'", 400);
			
			if(!$session->interface->crudR) throw new Exception("GET is not allowed for interface " . $session->interface->label, 405);
			
			$atom = new Atom($srcAtomId, $concept);
			if(!$atom->atomExists()) throw new Exception("Resource '$srcAtomId' not found", 404);
			
			$result = (array)$atom->getContent($session->interface, true, $tgtAtomId, $inclLinktoData, $arrayType, $metaData);
			
			if(empty($result)) Notifications::addInfo("No results found");			
	
			if(is_null($tgtAtomId)){
				// return array of atoms (i.e. tgtAtoms of the interface given srcAtomId)
				return array_values($result); // array_values transforms assoc array to non-assoc array
			}else{
				// return 1 atom (i.e. tgtAtomId)
				return current($result);
			}
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url PATCH resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param array $roleIds
	 * @param string $requestType
	 *
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function patchAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $roleIds = null, $requestType = 'feedback', $request_data = null){
		try{
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			$session->setInterface($interfaceId);
			
			if(!$session->interface->crudU) throw new Exception("PATCH is not allowed for interface " . $session->interface->label, 405);
			
			$session->atom = new Atom($tgtAtomId, $session->interface->tgtConcept);
			if(!$session->atom->atomExists()) throw new Exception("Resource '$tgtAtomId' does not exists", 404);
				
			return $session->atom->patch($session->interface, $request_data, $requestType);
	
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url PUT resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param array $roleIds
	 * @param string $requestType
	 * 
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function putAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $roleIds = null, $requestType = 'feedback', $request_data = null){
		try{
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			$session->setInterface($interfaceId);

			if(!$session->interface->crudU) throw new Exception("PUT is not allowed for interface " . $session->interface->label, 405);
			
			if(!$session->database->atomExists($tgtAtomId, $session->interface->tgtConcept)){
				// TODO: insert check if Atom may be created with this interface
				$session->database->addAtomToConcept($tgtAtomId, $session->interface->tgtConcept);
			}
			
			$session->atom = new Atom($tgtAtomId, $session->interface->tgtConcept);
			
			return $session->atom->put($session->interface, $request_data, $requestType);
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url DELETE resource/{concept}/{srcAtomId}/{interfaceId}/{tgtAtomId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param string $tgtAtomId
	 * @param array $roleIds
	 * @param string $requestType
	 * 
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function deleteAtom($concept, $srcAtomId, $interfaceId, $tgtAtomId, $roleIds = null, $requestType = 'feedback'){
		
		try{
			$session = Session::singleton();
			$session->activateRoles($roleIds);
			$session->setInterface($interfaceId);
		
			if(!$session->interface->crudD) throw new Exception("DELETE is not allowed for interface " . $session->interface->label, 405);
			
			$session->atom = new Atom($tgtAtomId, $session->interface->tgtConcept);
			if(!$session->atom->atomExists()) throw new Exception("Resource '$tgtAtomId' does not exists", 404);
			
			return $session->atom->delete($requestType);
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	
	/**
	 * @url POST resource/{concept}/{srcAtomId}/{interfaceId}
	 * @param string $concept
	 * @param string $srcAtomId
	 * @param string $interfaceId
	 * @param array $roleIds
	 * @param string $requestType
	 * 
	 * RequestType: reuqest for 'feedback' (try) or request to 'promise' (commit if possible).
	 */
	public function postAtom($concept, $srcAtomId, $interfaceId, $roleIds = null, $requestType = 'feedback', $request_data = null){
		try{
			$session = Session::singleton();			
			$session->activateRoles($roleIds);
			$session->setInterface($interfaceId);
			
			if(!$session->interface->crudC) throw new Exception("POST is not allowed for interface " . $session->interface->label, 405);
			
			$concept = $session->interface->tgtConcept;
			$atomId = $request_data['id'] ? $request_data['id'] : Concept::createNewAtom($concept);
			
			if($session->database->atomExists($atomId, $concept)) throw new Exception ("Resource already exists. POST method is not allowed", 405);
			
			$newAtomId = $session->database->addAtomToConcept($atomId, $concept);
			$session->atom = new Atom($newAtomId, $concept);
			
			return $session->atom->post($session->interface, $request_data, $requestType);
		
		}catch(Exception $e){
			throw new RestException($e->getCode(), $e->getMessage());
		}
	}
	

	/**************************** CONCEPTS ****************************/
    /**
     * @url GET concept
     * @url GET concept/{concept}
     */
    public function getConcepts($concept = null){
    	try{
    		if(isset($concept)){
        		// return Concept::getConcept(); // Return specific concept
        		throw new RestException(501); // 501: not implemented
    		}else{
    			return array_keys(Concept::getAllConcepts()); // Return list of all concepts
    		}
        	
        }catch(Exception $e){
       		throw new RestException($e->getCode(), $e->getMessage());
       	}
    }
	
	/**
     * @url GET resource/{concept}
     * @param string $concept
     * @param array $roleIds
     */
    public function getConceptAtoms($concept, $roleIds = null){
    	try{
    		$session = Session::singleton();
    		$session->activateRoles($roleIds);
    			
    		if(!in_array($concept, $session->getEditableConcepts())) throw new Exception ("You do not have access for this call", 403);
    		
        	return Concept::getAllAtomObjects($concept); // "Return list of all atoms for $concept"
        	
        }catch(Exception $e){
        	throw new RestException($e->getCode(), $e->getMessage());
        }
    }
    
    /**
     * @url GET resource/{concept}/{atomId}
     * @param string $concept
     * @param string $atomId
     * @param array $roleIds
     */
    public function getConceptAtom($concept, $atomId, $roleIds = null){
    	try{
    		$session = Session::singleton();
    		$session->activateRoles($roleIds);
    		    			 
    		if(!in_array($concept, $session->getEditableConcepts())) throw new Exception ("You do not have access for this call", 403);
    		
    		$atom = new Atom($atomId, $concept);
    		if(!$atom->atomExists()) throw new Exception("Resource '$atomId' not found", 404);
    		return $atom->getAtom();
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
   		}
    }
    	
	/**************************** UI stuff ****************************/
    
    /**
     * @url GET navBar
     * @param array $roleIds
     */
    public function getNavBar($roleIds = null){
    	try{
    		$session = Session::singleton();
    		$session->activateRoles($roleIds);
    		
    		// top level interfaces
    		foreach ($session->getInterfacesForNavBar() as $ifc){
    			$top[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
    		}
    		
    		// new interfaces
    		foreach ($session->getInterfacesToCreateAtom() as $ifc){
    			$new[] = array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
    		}
    		
    		return array ('top' => $top
    					 ,'new' => $new
    					 ,'refreshMenu' => $GLOBALS['navBar']['refreshMenu']
    					 ,'appMenu' => $GLOBALS['navBar']['appMenu']
    					 ,'roleMenu' => $GLOBALS['navBar']['roleMenu']
    					 ,'defaultSettings' => array ('notifications' => Notifications::getDefaultSettings()
    					 							 ,'switchAutoCommit' => Config::get('interfaceAutoCommitChanges', 'transactions')
    												 ,'cacheGetCalls' => Config::get('interfaceCacheGetCalls', 'transactions'))
    					 ,'notifications' => Notifications::getAll()
    					 ,'session' => array ( 'id' => $session->id
    					 					 , 'loggedIn' => Session::sessionUserLoggedIn())
    					 ,'sessionRoles' => $session->getSessionRoles()
    					 ,'sessionVars' => Session::getSessionVars()
    		);
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
    	}
    }
    
    /**
     * @url GET notifications/all
     * @param array $roleIds
     */
    public function getAllNotifications($roleIds = null){
    	try{
    		$session = Session::singleton();
    		$session->activateRoles($roleIds);
    
    		RuleEngine::getProcessViolationsFromDB($session);
    
    		return Notifications::getAll();
    			
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
    	}
    }
    
    /**
     * @url GET stats/performance/conjuncts
     * @param string $groupBy
     * @param int $from
     * @param int $to
     */
    public function conjPerfStats($groupBy = 'conjuncts', $from = 0, $to = 10){
    	try{
    		if(Config::get('productionEnv')) throw new Exception ("Performance tests are not allowed in production environment", 403);
    		
    		$performanceArr = array();
    		
    		// run all conjuncts (from - to)
    		for ($i = $from; $i <= $to; $i++){
    			$conj = RuleEngine::getConjunct('conj_' . $i);
    			$startTimeStamp = microtime(true); // true means get as float instead of string
    			RuleEngine::checkConjunct('conj_' . $i, false);
    			$endTimeStamp = microtime(true);
    			
    			$performanceArr['conj_'.$i] = array( 'id' => 'conj_' . $i
    										, 'start' => $startTimeStamp
    										, 'end' => $endTimeStamp
    										, 'duration' => $endTimeStamp - $startTimeStamp
    										, 'invariantRules' => implode(';', $conj['invariantRuleNames'])
    										, 'signalRules' => implode(';', $conj['signalRuleNames'])
    										);
    		}
    		
    		
    		switch ($groupBy){
    			case 'conjuncts' :
    				return array_values($performanceArr);
    				break;
    			case 'rules' :
    				$ruleArr = array();
    				foreach(RuleEngine::getAllRules() as $rule){
    					$duration = 0;
    					foreach($rule['conjunctIds'] as $conj){
    						$duration += $performanceArr[$conj]['duration'];
    					}
    					$ruleArr[] = array('ruleName' => $rule['name']
    										, 'duration' => $duration
    										, 'conjuncts' => implode(';', $rule['conjunctIds'])
    										);
    				}
    				return $ruleArr;
    				break;
    			case 'relations' :
    				$relArr = array();
    				foreach(Relation::getAllRelations() as $sig => $rel){
    					$duration = 0;
    					$conjuncts = array_merge($rel['affectedInvConjunctIds'], $rel['affectedSigConjunctIds']);
    					foreach($conjuncts as $conj){
    						$duration += $performanceArr[$conj]['duration'];
    					}
    					$relArr[] = array('relationSignature' => $sig
    									, 'duration' => $duration
    									, 'conjuncts' => implode(';', $conjuncts)
    									);
    				}
    				return $relArr;
    				break;
    			default :
    				throw new Exception ("Unknown groupBy argument", 500);
    				break;
    		}
    		
    	}catch(Exception $e){
    		throw new RestException($e->getCode(), $e->getMessage());
    	}
    }
}
?>