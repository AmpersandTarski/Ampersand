<?php

class Viewer {
	
	// TODO: make non static or private function
	public static function viewInterface($interface){
		
		
	}
	
	public static function getView($concept){
		global $allViews;
		
		foreach ((array)$allViews as $view){
			if ($concept == $view['concept'] || in_array($concept, Concept::getSpecializations($view['concept']))) return $view;
		}
		return null;
	}
	
	// TODO: make non static or private function
	public static function viewAtom($atom, $srcConcept){
		global $allViews; // from Generics.php
		$database = Database::singleton();
		
		foreach ($allViews as $view){
			if($view['concept'] == $srcConcept){
				$viewStrs = array ();
				foreach ($view['segments'] as $viewSegment) 
					if ($viewSegment['segmentType'] == 'Text')
						$viewStrs[] = htmlSpecialChars($viewSegment['Text']);
					elseif ($viewSegment['segmentType'] == 'Html')
						$viewStrs[] = $viewSegment['Html'];
					else {
						$query = "SELECT DISTINCT `tgt` FROM (".$viewSegment['expSQL'].") AS results WHERE src='".addslashes($atom)."'";
						$rows = $database->Exe($query);
						$txt = count($rows) ? $rows[0]['tgt'] : "<View relation not total>"; // this can happen in a create-new interface when the view fields have not yet been filled out.
						$viewStrs[] = htmlSpecialChars($txt);
					}
				return implode($viewStrs);
			}		
		}
		return $atom; // in case no view exists for this $srcConcept
	}
	
	public static function escapeHtmlAttrStr($str) {
		return str_replace(array('"', '&'), array('&quot;', '%26'), $str); // we do escapeSQL and replace \" by &quot; and \' by '
	}
	
	// TODO: can be deleted when not using Ampersand.js anymore
	public static function genEditableConceptInfo($interfaceName) {
		$atomViewMap = array ();
		$interface = new UserInterface($interfaceName);
		
		foreach ($interface->editableConcepts as $editableConcept) {
			$atomsAndViews = array ();
			
			foreach (Concept::getAllAtoms($editableConcept) as $atom) {
				$atomsAndViews[] = array ('atom' => $atom, 'view' => Viewer::viewAtom($atom, $editableConcept));
			}
			$atomViewMap[$editableConcept] = array ('hasView' => Viewer::getView($editableConcept)!=null, 'atomViewMap' => $atomsAndViews);
		}
		
		$atomViewMapJson = json_encode( $atomViewMap );
		
		return "function getEditableConceptInfo() { return ".$atomViewMapJson."; }";
	}
	
	// TODO: can be deleted when not using Ampersand.js anymore
	public static function generateInterfaceMap(){
		$session = Session::singleton();
		
		$interfaceMap = 'function getInterfacesMap() { var interfacesMap = new Array();'; // TODO: use Json for this
		foreach($session->role->getInterfaces() as $interface) {
				$conceptOrSpecs = array_merge(array($interface->srcConcept), Concept::getSpecializations($interface->srcConcept));

				foreach ($conceptOrSpecs as $concept) 
				$interfaceMap .= '  mapInsert(interfacesMap, "'.Viewer::escapeHtmlAttrStr($concept).'", "'.Viewer::escapeHtmlAttrStr($interface->name).'");';
		}
		$interfaceMap .= '  return interfacesMap; }';
		
		return $interfaceMap;
	}
}

?>