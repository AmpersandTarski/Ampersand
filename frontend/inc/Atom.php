<?php

Class Atom {
	
	public $id;
	
	public function __construct($id){
		
		$this->id = $id;
		
	}
	
	public function getContent($interface){
		$database = Database::singleton();
		
		if(!$interface->univalent) $content = array();
		
		$tgtAtoms = array_column($database->Exe("SELECT DISTINCT `tgt` FROM (".$interface->expressionSQL.") AS results WHERE src='".addslashes($this->id)."' AND `tgt` IS NOT NULL"), 'tgt');
		foreach ($tgtAtoms as $tgtAtom){
				
			if(count($interface->subInterfaces) > 0){				
				$atom = new Atom($tgtAtom);
				$content['id'] = $atom->id;
				$content['label'] = $atom->id; // TODO: enable ampersand VIEWS here
				foreach($interface->subInterfaces as $subinterface){
					
					$content[$subinterface->name] = $atom->getContent($subinterface);
				}
				count($tgtAtoms) > 1 ? $arr[] = $content : $arr = $content;
			}else{
				if(strtolower($tgtAtom) === "true") $tgtAtom = true;
				if(strtolower($tgtAtom) === "false") $tgtAtom = false;
				
		
				/*
				$links = array();
				$interfaces = array();
				foreach($session->role->getInterfaces(null, $this->tgtConcept) as $interfaceForTgtConcept){
					$links[] = $interfaceForTgtConcept->link . '/atom/' . urlencode($tgtAtom);
					$interfaces[] = $interfaceForTgtConcept->name;
				}*/
				if($interface->univalent){
					if($interface->tgtDataType == "concept"){
						$content = array('id' => $tgtAtom, 'label' => $tgtAtom); // TODO: enable ampersand VIEWS here
						// 'links' => $link; // TODO: views waarmee dit atom ook bekeken kan worden
					}else{
						$content = $tgtAtom;
						
					}					
					
				}else{
					if($interface->tgtDataType == "concept"){
						$content[] = array('id' => $tgtAtom, 'label' => $tgtAtom); // TODO: enable ampersand VIEWS here
						// 'links' => $link; // TODO: views waarmee dit atom ook bekeken kan worden
					}else{
						$content[] = $tgtAtom;
						
					}	
					
				}
				$arr = $content;
			}
		}
		
		return $arr;

	}
	
	public function setContent($interface, $content){
		$database = Database::singleton();
		
		$changes = JsonPatch::diff($this->getContent($interface), $content);
		
		foreach ((array)$changes as $change){
			switch($change['op']){ // operations
				case "replace" :
					$database->editUpdate('projectName', false, $this->id, "Project", $change['value'], "TEXT", 'child' , '');
					ErrorHandling::addSuccess('Project updated');
					break;
				case "add" :
					
					break;
				case "" :
					
					break;
			}
		}
		
		return array_merge($this->getContent($interface), array('notifications' => ErrorHandling::getAll())); 
		
	}
	
}

?>