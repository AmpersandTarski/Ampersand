<?php

Class Atom {
	
	public $id;
	
	public function __construct($id){
		
		$this->id = $id;
		
	}
	
	public function getContent($interface){
		$database = Database::singleton();
		
		$query = "SELECT DISTINCT `tgt` FROM (".$interface->expressionSQL.") AS results WHERE src='".addslashes($this->id)."' AND `tgt` IS NOT NULL";
		$tgtAtoms = array_column($database->Exe($query), 'tgt');
		
		foreach ($tgtAtoms as $tgtAtomId){
			$tgtAtom = new Atom($tgtAtomId);
				
			// determine value atom 
			if($interface->tgtDataType == "concept"){ // subinterface refers to other concept (i.e. not datatype).
				$content = array ('id' => $tgtAtom->id, 'label' => $tgtAtom->id); // TODO: enable ampersand VIEWS here

			}else{
				if(strtolower($tgtAtom->id) == "true") $tgtAtom->id = true; // convert string "true" to boolval true
				if(strtolower($tgtAtom->id) == "false") $tgtAtom->id = false; // convert string "false" to boolval false
				
				$content = $tgtAtom->id;
				
				
			}
			
			// subinterfaces
			foreach($interface->subInterfaces as $subinterface){
			
				$otherAtom = $tgtAtom->getContent($subinterface);
				$content[$subinterface->name] = $otherAtom;
				
			}
			
			// determine whether value of atom must be inserted as list or as single value
			if($interface->univalent AND !($interface->tgtDataType == "concept")){ // in cause of univalent (i.e. count of $tgtAtoms <= 1) and a datatype (i.e. not concept)
				$arr = $content;
			}elseif(!($interface->tgtDataType == "concept")){
				$arr[] = $content;
			}else{
				$arr[$tgtAtom->id] = $content;
			}			
				
			unset($content);			
		
		}
		
		return $arr;

	}
	
	public function patch(&$interface, $request_data){
		$database = Database::singleton();
		
		$before = current($this->getContent($interface));
		
		$patches = JsonPatch::diff($before, $request_data);
		
		foreach ((array)$patches as $key => $patch){
			switch($patch['op']){ // operations
				case "replace" :
					$pathArr = explode('/', $patch['path']);
						
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					if(empty(current($pathArr))) array_shift($pathArr); // remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					
					// find the right subinterface
					while (count($pathArr)){
												
						$tgtInterface = ObjectInterface::getSubinterface($tgtInterface, array_shift($pathArr));
						
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom 
						if (empty($tgtAtom)) $tgtAtom = $patch['value'];
						
					}
					
					// perform editUpdate
					if($tgtInterface->editable){
						if(is_bool($tgtAtom)) $tgtAtom = var_export($tgtAtom, true); // convert true and false into "true" and "false" strings
						
						// in case of empty $tgtAtom perform remove instead of replace.
						if(!empty($tgtAtom)){ 
							$database->editUpdate($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept, 'child', '');
						}else{
							// the final $tgtAtom is not provided, so we have to get this value to perform the editDelete function
							if(empty($tgtAtom)) $tgtAtom = JsonPatch::get($before, $patch['path']);
							$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept, 'child', '');
							 
						}					
					}else{
						ErrorHandling::addError($tgtInterface->name . " is not editable in interface '" . $interface->name . "'");
					}
					
					break;
				case "add" :
					
					break;
				case "remove" :
					$pathArr = explode('/', $patch['path']);
					
					$tgtInterface = $interface;
					$tgtAtom = $this->id; // init of tgtAtom is this atom itself, will be changed in while statement
					
					if(empty(current($pathArr))) array_shift($pathArr); // remove first empty arr element, due to root slash e.g. '/Projects/{atomid}/...'
					
					// find the right subinterface
					while (count($pathArr)){
						
						$tgtInterface = ObjectInterface::getSubinterface($tgtInterface, array_shift($pathArr));
						
						$srcAtom = $tgtAtom; // set srcAtom, before changing tgtAtom
						$tgtAtom = array_shift($pathArr); // set tgtAtom 

					}
					
					// perform editDelete
					if($tgtInterface->editable){
						// in case of 'remove' for a link to a non-concept (i.e. datatype), the final $tgtAtom is not provided, so we have to get this value to perform the editDelete function
						if(empty($tgtAtom)) $tgtAtom = JsonPatch::get($before, $patch['path']);
						$database->editDelete($tgtInterface->relation, $tgtInterface->relationIsFlipped, $srcAtom, $tgtInterface->srcConcept, $tgtAtom, $tgtInterface->tgtConcept, 'child', '');
					}else{
						ErrorHandling::addError($tgtInterface->name . " is not editable in interface '" . $interface->name . "'");
					}
					
					break;
			}
		}
		
		$database->closeTransaction(); // close transaction => ROLLBACK or COMMIT
		
		return array_merge(
				array('patches' => $patches), 
				array('content' => current($this->getContent($interface))), 
				array('notifications' => ErrorHandling::getAll())); 
		
	}
	
}

?>